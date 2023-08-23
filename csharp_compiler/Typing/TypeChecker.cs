using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Localizing;

using Nyapl.Parsing.Tree;

using Nyapl.Typing.Types;
using Nyapl.Typing.Effects;
using Nyapl.Typing.Constraints;

using Nyapl.IrGeneration;

namespace Nyapl.Typing;

public class TypeChecker {
	public static readonly Typ i32 = new Intrinsic(IntrinsicType.I32);
	public static readonly Typ boolean = new Intrinsic(IntrinsicType.Bool);
	public static readonly Typ tuple = new Intrinsic(IntrinsicType.Tuple);

	private LValueNode Check(Context ctx, LValueNode lVal, bool mutableRequired = true) {
		switch (lVal) {
			case TupleLValueNode tupl: {
				var children = new AstListNode<LValueNode>(tupl.Children.Location, tupl.Children.Select(l => Check(ctx, l)).ToList().AsReadOnly());
				var t = new Apply(tuple, children.Select(c => c.Type!).ToList().AsReadOnly());

				return new TupleLValueNode(tupl.Location, children, t);
			}
			case NamedLValueNode named: {
				if (!ctx.LookupVar(named.Name, out Typ? t, out var fullName, out var mutable)) throw new TypeError(named.Location, $"Variable {named.Name} not declared");
				if (!mutable && mutableRequired) throw new TypeError(named.Location, $"Variable {named.Name} not mutable");
				return new NamedLValueNode(named.Location, fullName, t);
			}
		}
		throw new Exception($"Unimplemented: Check({lVal.GetType().Name})");
	}

	private SideEffectNode Check(Context ctx, SideEffectNode effect) {
		return new(effect.Location, effect.Name, new NamedEffect(effect.Name));
	}

	private DestructureItemNode Check(Context ctx, DestructureItemNode item) {
		switch (item) {
			case NamedDestructureNode named: {
				var type = Check(ctx, named.TypeNode);
				return new NamedDestructureNode(named.Location, named.Name, type, type.Type!);
			}
			case TupleDestructureNode tupl: {
				var children = new AstListNode<DestructureItemNode>(tupl.Children.Location, tupl.Children.Select(d => Check(ctx, d)).ToList().AsReadOnly());
				var t = new Apply(tuple, children.Select(c => c.Type!).ToList().AsReadOnly());
				return new TupleDestructureNode(tupl.Location, children, t);
			}
			case HoleDestructureNode hole: {
				return new HoleDestructureNode(hole.Location, ctx.NewMeta());
			}
		}
		throw new Exception($"Unimplemented: Check({item.GetType().Name})");
	}

	private ParameterNode Check(Context ctx, ParameterNode parameter, bool allowHole = true) {
		var type = Check(ctx, parameter.Type, allowHole);
		ctx.DeclareVar(parameter.Name, type.Type!, mutable: false);
		return new(parameter.Location, parameter.Name, type);
	}

	private TypeNode Check(Context ctx, TypeNode type, bool allowHole = true) {
		switch (type) {
			case NamedTypeNode named: {
				return new NamedTypeNode(type.Location, named.Name, ctx.GetType(type.Location, named.Name));
			}
			case TypeHoleNode hole: {
				if (!allowHole) throw new TypeError(hole.Location, $"Hole not allowed in this context");
				return new TypeHoleNode(type.Location, ctx.NewMeta());
			}
			case FunctionTypeNode function: {
				var effects = new AstListNode<SideEffectNode>(function.Effects.Location, function.Effects.Select(e => Check(ctx, e)).ToList().AsReadOnly());
				var parameters = new AstListNode<TypeNode>(function.Parameters.Location, function.Parameters.Select(e => Check(ctx, e, allowHole)).ToList().AsReadOnly());
				var returnType = Check(ctx, function.ReturnType, allowHole);
				var allTypes = new [] {returnType.Type!}.Concat(parameters.Select(p => p.Type!)).ToList().AsReadOnly();
				var t = new Apply(new Function(effects.Select(e => e.Effect!).ToList().AsReadOnly(), null), allTypes);
				return new FunctionTypeNode(function.Location, effects, parameters, returnType, t);
			}
			case TupleTypeNode tupleType: {
				var types = new AstListNode<TypeNode>(tupleType.Types.Location, tupleType.Types.Select(t => Check(ctx, t, allowHole)).ToList().AsReadOnly());
				var t = new Apply(tuple, types.Select(t => t.Type!).ToList().AsReadOnly());
				return new TupleTypeNode(tupleType.Location, types, t);
			}
		}
		throw new Exception($"Unimplemented: Check({type.GetType().Name})");
	}

	private (ExpressionNode, IntrinsicNode?) Check(Context ctx, ExpressionNode expression, bool directChildOfCall = false) {
		switch (expression) {
			case TupleNode tupleNode: {
				var values = new AstListNode<ExpressionNode>(tupleNode.Location, tupleNode.Values.Select(v => Check(ctx, v).Item1).ToList().AsReadOnly());
				var type = new Apply(tuple, values.Select(v => v.Type!).ToList().AsReadOnly());
				return (new TupleNode(tupleNode.Location, values, type), null);
			}
			case HoleExpressionNode hole: {
				return (new HoleExpressionNode(hole.Location, ctx.NewMeta()), null);
			}
			case IntLiteralNode iLit: {
				return (new IntLiteralNode(iLit.Location, iLit.Value, i32), null);
			}
			case BoolLiteralNode bLit: {
				return (new BoolLiteralNode(bLit.Location, bLit.Value, boolean), null);
			}
			case VarLookupNode varLookup: {
				if (!ctx.LookupVar(varLookup.Name, out Typ? t, out var fullName, out _)) throw new TypeError(varLookup.Location, $"Variable {varLookup.Name} not declared");
				return (new VarLookupNode(varLookup.Location, fullName, t), null);
			}
			case IntrinsicNode intrinsic: {
				if (!ctx.Unsafe) throw new TypeError(intrinsic.Location, "Intrinsics are not allowed outside of `unsafe` blocks");
				if (!directChildOfCall) throw new TypeError(intrinsic.Location, "Intrinsics are only allowed as the child of a call");
				if (!ctx.LookupIntrinsic(intrinsic.Name, out Typ? t)) throw new TypeError(intrinsic.Location, $"Intrisic {intrinsic.Name} does not exist");
				var node = new IntrinsicNode(intrinsic.Location, intrinsic.Name, t);
				return (node, node);
			}
			case BinOpNode binOp: {
				// get type of left operand
				var lExpr = Check(ctx, binOp.LExpr).Item1;
				// get type of right operand
				var rExpr = Check(ctx, binOp.RExpr).Item1;
				// create a return type meta
				var resultType = ctx.NewMeta();
				// add a constraint for the right binary operator
				ctx.AddTypeConstraint(new TypeConstraint.BinOp(binOp.Location, lExpr.Type!, rExpr.Type!, resultType, binOp.OP, ctx));
				return (new BinOpNode(binOp.Location, lExpr, binOp.OP, rExpr, resultType), null);
			}
			case UnOpNode unOp: {
				// get type of operand
				var expr = Check(ctx, unOp.Expr).Item1;
				// create a return type meta
				var resultType = ctx.NewMeta();
				// add a constraint for the right unary operator
				ctx.AddTypeConstraint(new TypeConstraint.UnOp(unOp.Location, expr.Type!, resultType, unOp.OP, ctx));
				return (new UnOpNode(unOp.Location, expr, unOp.OP, resultType), null);
			}
			case CallNode call: {
				var (baseExpr, intrinsic) = Check(ctx, call.BaseExpr, directChildOfCall: true);
				var arguments = new AstListNode<ExpressionNode>(call.Arguments.Location, call.Arguments.Select(arg => Check(ctx, arg).Item1).ToList().AsReadOnly());
				var effects = ctx.NewMetaEffect();
				ctx.AddEffectConstraint(new EffectConstraint.AtMost(arguments.Location, effects, ctx.SideEffects()));
				Typ baseType = ctx.Force(baseExpr.Type!);

				Typ t = ctx.NewMeta();
				var argTs = arguments.Select(_ => ctx.NewMeta()).Append(ctx.NewMeta()).Cast<Typ>().ToList().AsReadOnly();
				ctx.Unify(baseExpr.Location, argTs.First(), t);
				var funcType = new Apply(new Function(new List<Effect>(new[] {effects}).AsReadOnly(), null), argTs);
				ctx.Unify(baseExpr.Location, funcType, baseType);
				argTs.Skip(1).Zip(arguments, (a, b) => { ctx.Unify(b.Location, a, b.Type!); return true; }).All(b => b);

				return intrinsic == null ? (new CallNode(call.Location, baseExpr, arguments, t), null) : (new IntrinsicCallNode(call.Location, intrinsic, arguments, t), null);
			}
		}
		throw new Exception($"Unimplemented: Check({expression.GetType().Name})");
	}

	private ElseStatementNode Check(Context ctx, ElseStatementNode @else) {
		ctx.NewVariableScope(true);
		var body = new AstListNode<StatementNode>(@else.Body.Location, @else.Body.Select(s => Check(ctx, s)).ToList().AsReadOnly());
		ctx.EndVariableScope();

		return new(@else.Location, body);
	}

	private ElifStatementNode Check(Context ctx, ElifStatementNode elif) {
		var expr = Check(ctx, elif.Expr).Item1;
		ctx.Unify(expr.Location, expr.Type!, boolean);
		ctx.NewVariableScope(true);
		var body = new AstListNode<StatementNode>(elif.Body.Location, elif.Body.Select(s => Check(ctx, s)).ToList().AsReadOnly());
		ctx.EndVariableScope();

		return new(elif.Location, expr, body);
	}

	private IfStatementNode Check(Context ctx, IfStatementNode ifStatement) {
		var expr = Check(ctx, ifStatement.IfExpr).Item1;
		ctx.Unify(expr.Location, expr.Type!, boolean);
		ctx.NewVariableScope(true);
		var body = new AstListNode<StatementNode>(ifStatement.IfBody.Location, ifStatement.IfBody.Select(s => Check(ctx, s)).ToList().AsReadOnly());
		ctx.EndVariableScope();

		var elifs = ifStatement.Elifs.Select(e => Check(ctx, e)).ToList().AsReadOnly();

		var @else = ifStatement.Else == null ? null : Check(ctx, ifStatement.Else);

		return new(ifStatement.Location, expr, body, elifs, @else);
	}

	private WhileStatementNode Check(Context ctx, WhileStatementNode whileStatement) {
		var expr = Check(ctx, whileStatement.Expr).Item1;
		ctx.Unify(expr.Location, expr.Type!, boolean);
		ctx.NewVariableScope(true);
		var body = new AstListNode<StatementNode>(whileStatement.Body.Location, whileStatement.Body.Select(s => Check(ctx, s)).ToList().AsReadOnly());
		ctx.EndVariableScope();

		return new(whileStatement.Location, expr, body);
	}

	private void Declare(Context ctx, DestructureItemNode item, bool mutable) {
		switch (item) {
			case NamedDestructureNode n: {
				if(!ctx.DeclareVar(n.Name, n.Type!, mutable)) throw new TypeError(n.Location, $"Variable {n.Name} already declared");
			} break;
			case TupleDestructureNode t: {
				foreach (var child in t.Children) {
					Declare(ctx, child, mutable);
				}
			} break;
			case HoleDestructureNode:
			break;
			default: throw new Exception($"Unknown DestructureItemNode: {item.GetType().Name}");
		}
	}

	private StatementNode Check(Context ctx, StatementNode statement) {
		switch (statement) {
			case ReturnStatementNode ret: {
				var expression = ret.Expression;
				if (expression != null) {
					expression = Check(ctx, expression).Item1;
					ctx.Unify(expression.Location, expression.Type!, ctx.ReturnType());
				} else {
					throw new Exception($"Empty return statements are not allowed yet");
				}
				return new ReturnStatementNode(ret.Location, expression);
			}
			case DeclareVarNode dec: {
				var declaredType = Check(ctx, dec.Type);
				var expressionType = Check(ctx, dec.Expression).Item1;
				ctx.Unify(declaredType.Location, declaredType.Type, expressionType.Type);
				if(!ctx.DeclareVar(dec.Name, declaredType.Type!, dec.Mutable)) throw new TypeError(dec.Location, $"Variable {dec.Name} already declared");
				return new DeclareVarNode(dec.Location, dec.Name, dec.Mutable, declaredType, expressionType);
			}
			case DestructureNode destruct: {
				var items = new AstListNode<DestructureItemNode>(destruct.Location, destruct.Items.Select(n => Check(ctx, n)).ToList().AsReadOnly());
				var expression = Check(ctx, destruct.Expression).Item1;
				var expected = new Apply(tuple, items.Select(n => n.Type!).ToList().AsReadOnly());
				ctx.Unify(destruct.Location, expected, expression.Type!);
				foreach (var item in items) {
					Declare(ctx, item, destruct.Mutable);
				}
				return new DestructureNode(destruct.Location, items, destruct.Mutable, expression);
			}
			case WhileStatementNode @while: {
				return Check(ctx, @while);
			}
			case IfStatementNode @if: {
				return Check(ctx, @if);
			}
			case FunctionNode func: {
				Check(ctx, func);
				return new NoopStatementNode(func.Location);
			}
			case UnsafeStatementNode @unsafe: {
				var wereUnsafe = ctx.Unsafe;
				ctx.Unsafe = true;

				// check the body
				var body = new AstListNode<StatementNode>(@unsafe.Body.Location, @unsafe.Body.Select(s => Check(ctx, s)).ToList().AsReadOnly());
				var effects = new AstListNode<SideEffectNode>(@unsafe.Effects.Location, @unsafe.Effects.Select(e => Check(ctx, e)).ToList().AsReadOnly());
				// check sideeffects
				var metaEffect = ctx.NewMetaEffect();
				ctx.AddEffectConstraint(new EffectConstraint.Equivalent(@unsafe.Effects.Location, new Effect[] {metaEffect}.ToList().AsReadOnly(), effects.Select(e => e.Effect!).ToList().AsReadOnly()));
				ctx.AddEffectConstraint(new EffectConstraint.AtMost(@unsafe.Effects.Location, metaEffect, ctx.SideEffects()));
				ctx.Unsafe = wereUnsafe;


				var @new = new UnsafeStatementNode(@unsafe.Location, effects, body);

				return @new;
			}
			case ReassignNode reassign: {
				var lVal = Check(ctx, reassign.LVal);
				var expr = Check(ctx, reassign.Expr).Item1;

				ctx.Unify(reassign.Location, lVal.Type, expr.Type);

				return new ReassignNode(reassign.Location, lVal, expr);
				throw new Exception("STUB");
			}
			case StandaloneCallNode call: {
				var baseExpr = Check(ctx, call.BaseExpr, mutableRequired: false);
				var arguments = new AstListNode<ExpressionNode>(call.Arguments.Location, call.Arguments.Select(arg => Check(ctx, arg).Item1).ToList().AsReadOnly());
				var effects = ctx.NewMetaEffect();
				ctx.AddEffectConstraint(new EffectConstraint.AtMost(arguments.Location, effects, ctx.SideEffects()));
				Typ baseType = ctx.Force(baseExpr.Type!);

				Typ t = ctx.NewMeta();
				var argTs = arguments.Select(_ => ctx.NewMeta()).Append(ctx.NewMeta()).Cast<Typ>().ToList().AsReadOnly();
				ctx.Unify(baseExpr.Location, argTs.First(), t);
				var funcType = new Apply(new Function(new List<Effect>(new[] {effects}).AsReadOnly(), null), argTs);
				ctx.Unify(baseExpr.Location, funcType, baseType);
				argTs.Skip(1).Zip(arguments, (a, b) => { ctx.Unify(b.Location, a, b.Type!); return true; }).All(b => b);

				// temporary(?), forces the call to resolve to a ()
				ctx.Unify(baseExpr.Location, t, new Apply(tuple, new Typ[]{}.ToList().AsReadOnly()));

				return new StandaloneCallNode(call.Location, baseExpr, arguments, t);
			}
			case IntrinsicStandaloneCallNode call: {
				var (baseExpr, intrinsic) = Check(ctx, call.BaseExpr, directChildOfCall: true);
				var arguments = new AstListNode<ExpressionNode>(call.Arguments.Location, call.Arguments.Select(arg => Check(ctx, arg).Item1).ToList().AsReadOnly());
				var effects = ctx.NewMetaEffect();
				ctx.AddEffectConstraint(new EffectConstraint.AtMost(arguments.Location, effects, ctx.SideEffects()));
				Typ baseType = ctx.Force(baseExpr.Type!);

				Typ t = ctx.NewMeta();
				var argTs = arguments.Select(_ => ctx.NewMeta()).Append(ctx.NewMeta()).Cast<Typ>().ToList().AsReadOnly();
				ctx.Unify(baseExpr.Location, argTs.First(), t);
				var funcType = new Apply(new Function(new List<Effect>(new[] {effects}).AsReadOnly(), null), argTs);
				ctx.Unify(baseExpr.Location, funcType, baseType);
				argTs.Skip(1).Zip(arguments, (a, b) => { ctx.Unify(b.Location, a, b.Type!); return true; }).All(b => b);

				// temporary(?), forces the call to resolve to a ()
				ctx.Unify(baseExpr.Location, t, new Apply(tuple, new Typ[]{}.ToList().AsReadOnly()));

				return new IntrinsicStandaloneCallNode(call.Location, intrinsic!, arguments, t);
			}
		}
		throw new Exception($"Unimplemented: Check({statement.GetType().Name})");
	}

	private ReadOnlyCollection<Effect> Zonk(Context ctx, ReadOnlyCollection<Effect> effects) {
		return effects.SelectMany<Effect,Effect>((e) => {
			if (e is MetaEffect m) {
				var effects = ctx.GetMetaEffect(m.ID);
				if (effects != null) {
					return effects;
				}
			}
			return new List<Effect>() { e };
		}).ToList().AsReadOnly();
	}

	private Typ Zonk(Context ctx, Typ type) {
		switch (type) {
			case Apply a:
				return new Apply(Zonk(ctx, a.BaseType), a.ParameterTypes.Select(t => Zonk(ctx, t)).ToList().AsReadOnly());
			case Function f:
				return new Function(Zonk(ctx, f.Effects), f.IsIntrinsic);
			case Trivial:
			case Intrinsic:
				return type;
			case Meta m: {
				Typ t = ctx.Force(m);
				if (t == m) return t;
				return Zonk(ctx, ctx.Force(m));
			}
			default: throw new Exception($"Zonk(ctx, {type.GetType().Name}) not implemented yet");
		}
	}

	private T Zonk<T>(Context ctx, T node) where T : AstNode {
		if (node is ITypedNode typed) {
			typed.Type = Zonk(ctx, typed.Type!);
		}
		foreach (var child in node.GetChildren()) {
			Zonk(ctx, child);
		}
		return node;
	}

	private bool HasMeta(Typ type, int safeMetaLimit) {
		if (type is Meta m) {
			return m.ID >= safeMetaLimit;
		}
		if (type is Apply a) {
			return HasMeta(a.BaseType, safeMetaLimit) || a.ParameterTypes.Select(s => HasMeta(s, safeMetaLimit)).Any(b => b);
		}
		if (type is Function f) {
			return false;
		}
		if (type is Trivial || type is Intrinsic) {
			return false;
		}

		throw new Exception($"HasMeta({type.GetType().Name}) not implemented yet");
	}

	private IEnumerable<CompileError> ScanForUnresolvedMetas(AstNode node, int safeMetaLimit) {
		if (node is ITypedNode typed) {
			if (HasMeta(typed.Type!, safeMetaLimit)) yield return new TypeError(node.Location, "Could not infer type");
		}
		foreach (var child in node.GetChildren()) {
			var errors = ScanForUnresolvedMetas(child, safeMetaLimit);
			foreach (var error in errors) yield return error;
		}
	}

	private TypeDefNode Check(Context ctx, TypeDefNode typedef) {
		var type = Check(ctx, typedef.TypeNode, allowHole: false);
		ctx.DeclareType(typedef.Location, typedef.Name, type.Type!);
		return new(typedef.Location, typedef.Name, type, type.Type!);
	}

	private FunctionNode Check(Context ctx, FunctionNode function, bool topLevel = false, bool shallowPass = false) {
		string fullName = topLevel ? function.Name : ctx.CurrentFunctionName() + "/" + function.Name;

		ctx.NewVariableScope(false);

		var sideEffects = new AstListNode<SideEffectNode>(function.SideEffects.Location, function.SideEffects.Select(s => Check(ctx, s)).ToList().AsReadOnly());
		var parameters  = new AstListNode<ParameterNode>(function.Parameters.Location, function.Parameters.Select(p => Check(ctx, p, allowHole: !topLevel)).ToList().AsReadOnly());
		var returnType  = Check(ctx, function.ReturnType, allowHole: !topLevel);

		ctx.NewFunction(returnType.Type!, sideEffects.Select(s => s.Effect!).ToList().AsReadOnly(), fullName);

		if (topLevel)
			ctx.NewMetaScope();
		// function-scoped metas

		var body = shallowPass ? function.Body : new AstListNode<StatementNode>(function.Body.Location, function.Body.Select(s => Check(ctx, s)).ToList().AsReadOnly());

		var func = shallowPass ? function : Zonk<FunctionNode>(ctx, new(function.Location, fullName, sideEffects, parameters, returnType, body));

		if (topLevel) {
			if (!shallowPass) {
				var errors = ScanForUnresolvedMetas(func, ctx.SafeMetaLimit());
				if (errors.Any()) {
					throw new BundledError(errors.ToList().AsReadOnly());
				}
			}
			ctx.EndMetaScope();
		}

		ctx.EndFunction();

		ctx.EndVariableScope();

		if (!topLevel || shallowPass)
			if (!ctx.DeclareVar(function.Name, new Apply(new Function(sideEffects.Select(e => e.Effect!).ToList().AsReadOnly(), false), new Typ[] {returnType.Type!}.Concat(parameters.Select(p => p.Type.Type!)).ToList().AsReadOnly()), mutable: false, isFunc: true, fullName)) throw new TypeError(function.Location, $"Variable {function.Name} already declared");

		if (!topLevel) {
			ctx.AddNestedFunction(func);
		}

		return func;
	}

	public TypedFileNode Check(LocalizedFileNode tree) {
		var ctx = new Context(tree.Platform);
		ctx.NewMetaScope();
		var typeDefs = tree.TypeDefs.Select(t => Check(ctx, t)).ToList().AsReadOnly();
		foreach (var f in tree.Functions) Check(ctx, f, topLevel: true, shallowPass: true);
		var functions = tree.Functions.Select(f => Check(ctx, f, topLevel: true)).ToList();
		ctx.EndMetaScope();
		ctx.PrintGlobals();
		functions.AddRange(ctx.GetNestedFunctions());
		return new(tree.Location, tree.Platform, functions.AsReadOnly(), typeDefs, ctx);
	}

	public class Context {
		private List<FunctionNode> nested = new();

		public void AddNestedFunction(FunctionNode node) {
			nested.Add(node);
		}

		public List<FunctionNode> GetNestedFunctions() {
			return nested;
		}

		public bool Unsafe { get; set; } = false;
		private Localizer.Platform Platform { get; }

		public bool LookupIntrinsic(string name, out Typ? type) {
			if (Platform.HasIntrinsic(name)) {
				type = Platform.GetIntrinsic(name);
				return true;
			}
			type = null;
			return false;
		}

		Dictionary<string,Typ> namedTypes = new() {
			{ "i32", i32 },
			{ "bool", boolean },
		};

		// TODO rework the way operators work so I can make tuples and functions have equality (assuming all types in the tuple have equality, for functions it doesn't matter)
		List<(BinOpKind, Typ, Typ, Typ, IrOpKind)> binOperators = new() {
			(BinOpKind.Add,      i32, i32, i32, IrOpKind.Add),
			(BinOpKind.Subtract, i32, i32, i32, IrOpKind.Subtract),
			(BinOpKind.Multiply, i32, i32, i32, IrOpKind.Multiply),
			(BinOpKind.Divide,   i32, i32, i32, IrOpKind.Divide),
			(BinOpKind.Modulo,   i32, i32, i32, IrOpKind.Modulo),

			(BinOpKind.Equal,    i32, i32, boolean, IrOpKind.Equal),
			(BinOpKind.NotEq,    i32, i32, boolean, IrOpKind.NotEq),

			(BinOpKind.Equal,    boolean, boolean, boolean, IrOpKind.Equal),
			(BinOpKind.NotEq,    boolean, boolean, boolean, IrOpKind.NotEq),
		};
		List<(UnOpKind, Typ, Typ, IrOpKind)> unOperators = new() {
			(UnOpKind.Not,      boolean, boolean, IrOpKind.Not),
			(UnOpKind.Positive, i32, i32, IrOpKind.Positive),
			(UnOpKind.Negative, i32, i32, IrOpKind.Negative),
		};

		public IrParam.IntrinsicOp GetOp(Typ left, Typ right, BinOpKind kind)
			=> new(binOperators.First(t => t.Item1 == kind && t.Item2 == left && t.Item3 == right).Item5);

		public IrParam.IntrinsicOp GetOp(Typ source, UnOpKind kind)
			=> new(unOperators.First(t => t.Item1 == kind && t.Item2 == source).Item4);

		public ReadOnlyCollection<(BinOpKind, Typ, Typ, Typ)> GetBinOperators()
			=> binOperators.Select(t => (t.Item1, t.Item2, t.Item3, t.Item4)).ToList().AsReadOnly();

		public ReadOnlyCollection<(UnOpKind, Typ, Typ)> GetUnOperators()
			=> unOperators.Select(t => (t.Item1, t.Item2, t.Item3)).ToList().AsReadOnly();

		public void DeclareType(SourceLoc loc, string name, Typ type) {
			if (namedTypes.ContainsKey(name)) throw new TypeError(loc, $"Attempting to redefine type {name}");
			namedTypes[name] = type;
		}

		public Typ GetType(SourceLoc loc, string name) {
			if (!namedTypes.ContainsKey(name)) throw new TypeError(loc, $"Unknown type '{name}'");
			return namedTypes[name];
		}


		Solver solver = new();

		Stack<FunctionInfo> functionData = new();

		private readonly struct FunctionInfo {
			public Typ ReturnType { get; }
			public ReadOnlyCollection<Effect> Effects { get; }
			public string FullName { get; }
			public FunctionInfo(Typ returnType, ReadOnlyCollection<Effect> effects, string fullName) {
				ReturnType = returnType;
				Effects = effects;
				FullName = fullName;
			}
		}

		MetaScope metas = new();

		private class MetaScope {
			public MetaScope() : this(null) {}
			public MetaScope(MetaScope? parent) {
				Parent = parent;
				ID = parent?.ID ?? 0;
			}

			public int ID { get; private set; }
			Dictionary<int, Typ?> metas = new();
			Dictionary<int, ReadOnlyCollection<Effect>?> effects = new();
			public MetaScope? Parent { get; }

			public int SafeLimit() {
				// get the safe limit if this scope gets dropped
				return Parent?.ID ?? 0;
			}

			public Typ? Get(int id) {
				if (metas.ContainsKey(id)) return metas[id];
				else if (Parent != null) return Parent.Get(id);
				else throw new Exception($"Meta {id} not found, this shouldn't happen lol");
			}

			public void Set(int id, Typ type) {
				if (metas.ContainsKey(id)) metas[id] = type;
				else if (Parent != null) Parent.Set(id, type);
				else throw new Exception($"Meta {id} not found, this shouldn't happen lol");
			}

			public ReadOnlyCollection<Effect>? GetEffect(int id) {
				if (effects.ContainsKey(id)) return effects[id];
				else if (Parent != null) return Parent.GetEffect(id);
				else throw new Exception($"MetaEffect {id} not found, this shouldn't happen lol");
			}

			public void SetEffect(int id, ReadOnlyCollection<Effect> es) {
				if (effects.ContainsKey(id)) effects[id] = es;
				else if (Parent != null) Parent.SetEffect(id, es);
				else throw new Exception($"MetaEffect {id} not found, this shouldn't happen lol");
			}

			public Meta NewMeta() {
				metas[ID] = null;
				return new(ID++);
			}

			public MetaEffect NewMetaEffect() {
				effects[ID] = null;
				return new(ID++);
			}

			public override string ToString() =>
				string.Join("\n", metas.Select((pair) => $"\t{pair.Key}: {pair.Value}")) + "\n" +
				string.Join("\n", effects.Select((pair) => $"\t{pair.Key}: {pair.Value}"));
		}

		VariableScope variables = new(false);

		private class VariableScope {
			public VariableScope? Parent { get; }
			Dictionary<string, (Typ type, bool isFunc, string fullName, bool mutable)> variables = new();
			bool CanSearchParent { get; }

			public VariableScope(bool canSearchParent) : this(canSearchParent, null) {}
			public VariableScope(bool canSearchParent, VariableScope? parent) {
				Parent = parent;
				CanSearchParent = canSearchParent;
			}

			public bool Declare(string name, Typ type, bool isFunc, string? fullName, bool mutable) {
				if (variables.ContainsKey(name)) return false;
				variables[name] = (type, isFunc, fullName ?? name, mutable);
				return true;
			}

			public bool Lookup(string name, out Typ? type, out string fullName, out bool mutable, bool returnOnlyFunctions = false) {
				type = null;
				fullName = name;
				mutable = false;
				if (variables.ContainsKey(name)) {
					var v = variables[name];
					if (v.isFunc)
						fullName = v.fullName;

					mutable = v.mutable;

					if (returnOnlyFunctions) {
						// if we are in global scope, return it anyway
						// or if this is a function
						if (Parent == null || v.isFunc) {
							type = v.type;
							return true;
						}
						// else, we didn't find it
						return false;
					}

					type = v.type;
					return true;
				}
				if (Parent != null) return Parent.Lookup(name, out type, out fullName, out mutable, returnOnlyFunctions || !CanSearchParent);
				return false;
			}

			public override string ToString() => string.Join("\n", variables.Select((pair) => $"\t{pair.Key}: {(pair.Value.isFunc ? "[fn]" : "")}{pair.Value.type}"));
		}

		public Context(Localizer.Platform platform) {
			Platform = platform;
		}

		public void NewFunction(Typ returnType, ReadOnlyCollection<Effect> effects, string fullName) {
			functionData.Push(new(returnType, effects, fullName));
		}

		public void EndFunction() {
			functionData.Pop();
		}

		public Typ ReturnType() {
			return functionData.Peek().ReturnType;
		}

		public ReadOnlyCollection<Effect> SideEffects() {
			return functionData.Peek().Effects;
		}

		public string CurrentFunctionName() {
			return functionData.Peek().FullName;
		}

		public void NewMetaScope() {
			metas = new(metas);
		}

		public void EndMetaScope() {
			if (metas.Parent == null) throw new Exception("Ended meta scope too often");
			metas = metas.Parent;
		}

		public void NewVariableScope(bool canSearchParent) {
			variables = new(canSearchParent, variables);
		}

		public void EndVariableScope() {
			if (variables.Parent == null) throw new Exception("Ended variable scope too often");
			variables = variables.Parent;
		}

		public bool DeclareVar(string name, Typ type, bool mutable, bool isFunc = false, string? fullName = null) {
			return variables.Declare(name, type, isFunc, fullName, mutable);
		}

		public bool LookupVar(string name, out Typ? type, out string fullName, out bool mutable) {
			return variables.Lookup(name, out type, out fullName, out mutable);
		}

		public Typ Force(Typ t) {
			if (t is Meta m) {
				if (metas.Get(m.ID) != null) return Force(metas.Get(m.ID)!);
			}
			return t;
		}

		public bool Occurs(int id, Typ t) {
			switch (t) {
				case Function:
				case Trivial:
				case Intrinsic:
					return false;
				case Meta m: return m.ID == id;
				case Apply a: return Occurs(id, a.BaseType) || a.ParameterTypes.Any(x => Occurs(id, x));
				default: throw new Exception($"Occurs not implemented for {t.GetType().Name}");
			}
		}

		public void Solve(SourceLoc loc, int id, Typ t) {
			if (Occurs(id, t)) throw new TypeError(loc, "Metavariable occurs checking failed, your type is too fucky for the compiler");
			if (metas.Get(id) == null) {
				metas.Set(id, t);
				solver.NotifyMetaSolved(this, id);
			} else {
				Unify(loc, metas.Get(id)!, t);
			}
		}

		public void Unify(SourceLoc loc, ReadOnlyCollection<Effect> l, ReadOnlyCollection<Effect> r) {
			AddEffectConstraint(new EffectConstraint.Equivalent(loc, l, r));
		}

		// TODO maybe add subtyping parameter to only allow subtying when that is wanted, unsure
		public void Unify(SourceLoc loc, Typ? l, Typ? r) {
			if (l == null) throw new ArgumentNullException(nameof(l));
			if (r == null) throw new ArgumentNullException(nameof(r));

			if (l is Trivial lT && r is Trivial rT) {
				if (lT.Name != rT.Name) throw new TypeError(loc, $"{lT.Name} and {rT.Name} are not compatible types");
				return;
			} else if (l is Intrinsic lI && r is Intrinsic rI) {
				if (lI.Type != rI.Type) throw new TypeError(loc, $"{lI.Type} and {rI.Type} are not compatible types");
				return;
			} else if (l is Function lF && r is Function rF) {
				// check the effects
				Unify(loc, lF.Effects, rF.Effects);
				return;
			} else if (l is Apply lA && r is Apply rA) {
				if (lA.ParameterTypes.Count != rA.ParameterTypes.Count)
					throw new TypeError(loc, $"Generic parameter count does not match, Expected: {rA.ParameterTypes.Count}, Actual: {lA.ParameterTypes.Count}");
				Unify(loc, lA.BaseType, rA.BaseType);
				lA.ParameterTypes.Zip(rA.ParameterTypes).All((t) => { Unify(loc, t.Item1, t.Item2); return true; });
				return;
			} else if (l is Meta lM) {
				AddTypeConstraint(new TypeConstraint.Equal(loc, l, r, this));
				return;
			} else if (r is Meta rM) {
				AddTypeConstraint(new TypeConstraint.Equal(loc, l, r, this));
				return;
			}

			throw new TypeError(loc, $"Cannot Unify {l} and {r}");
		}

		public MetaEffect NewMetaEffect() => metas.NewMetaEffect();

		public ReadOnlyCollection<Effect> GetFlatEffect(Effect e) {
			if (e is MetaEffect m) {
				var effects = metas.GetEffect(m.ID);
				if (effects == null) return new List<Effect>() {e}.AsReadOnly();
				else return effects.SelectMany(e => GetFlatEffect(e)).ToList().AsReadOnly();
			}
			return new List<Effect>() {e}.AsReadOnly();
		}

		public ReadOnlyCollection<Effect>? GetMetaEffect(int id) => metas.GetEffect(id);

		public void SetMetaEffect(int id, ReadOnlyCollection<Effect> effects) => metas.SetEffect(id, effects);

		public Meta NewMeta() => metas.NewMeta();

		public int SafeMetaLimit() => metas.SafeLimit();

		public void AddEffectConstraint(EffectConstraint constraint) {
			solver.Solve(this, constraint);
		}

		public void AddTypeConstraint(TypeConstraint constraint) {
			solver.Solve(this, constraint);
		}

		public void PrintGlobals() => Console.WriteLine("Globals:\n" + variables);

		public ushort GetSize(Typ type) {
			if (type is Intrinsic i) {
				switch (i.Type) {
					case IntrinsicType.I32:
						return 4;
					case IntrinsicType.Bool:
						return 1;
					default:
						throw new Exception($"Cannot get size for intrinsic: {i.Type}");
				}
			}
			if (type is Apply a) {
				switch (a.BaseType) {
					case Intrinsic aI:
						switch (aI.Type) {
							case IntrinsicType.Tuple:
								return a.ParameterTypes.Select(p => GetSize(p)).Aggregate((ushort)0, (sum, val) => (ushort)(sum + val));
							default: throw new Exception($"Cannot get size for Apply(Intrinsic): {aI.Type}");
						}
					case Function f:
						return Platform.PointerSize;
					default:
						throw new Exception($"Cannot get size for apply: {a}");
				}
			}
			throw new Exception($"Cannot get size for type: {type.GetType().Name}");
		}
	}
}
