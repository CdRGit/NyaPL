using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;

using Nyapl.Typing.Types;
using Nyapl.Typing.Effects;
using Nyapl.Typing.Constraints;

namespace Nyapl.Typing;

public class TypeChecker {
	private SideEffectNode Check(Context ctx, SideEffectNode effect) {
		return new(effect.Location, effect.Name, new NamedEffect(effect.Name));
	}

	private ParameterNode Check(Context ctx, ParameterNode parameter, bool allowHole = true) {
		var type = Check(ctx, parameter.Type, allowHole);
		ctx.DeclareVar(parameter.Name, type.Type!);
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
				var t = new Apply(new Function(effects.Select(e => e.Effect!).ToList().AsReadOnly()), allTypes);
				return new FunctionTypeNode(function.Location, effects, parameters, returnType, t);
			}
		}
		throw new Exception($"Unimplemented: Check({type.GetType().Name})");
	}

	private ExpressionNode Check(Context ctx, ExpressionNode expression) {
		switch (expression) {
			case HoleExpressionNode hole: {
				return new HoleExpressionNode(hole.Location, ctx.NewMeta());
			}
			case IntLiteralNode iLit: {
				return new IntLiteralNode(iLit.Location, iLit.Value, ctx.GetType(iLit.Location, "i32"));
			}
			case VarLookupNode varLookup: {
				if (!ctx.LookupVar(varLookup.Name, out Typ? t)) throw new TypeError(varLookup.Location, $"Variable {varLookup.Name} not declared");
				return new VarLookupNode(varLookup.Location, varLookup.Name, t);
			}
			case BinOpNode binOp: {
				// get type of left operand
				var lExpr = Check(ctx, binOp.LExpr);
				// get type of right operand
				var rExpr = Check(ctx, binOp.RExpr);
				// create a return type meta
				var resultType = ctx.NewMeta();
				// add a constraint for the right binary operator
				ctx.AddTypeConstraint(new TypeConstraint.BinOp(binOp.Location, lExpr.Type!, rExpr.Type!, resultType, binOp.OP, ctx));
				return new BinOpNode(binOp.Location, lExpr, binOp.OP, rExpr, resultType);
			}
			case CallNode call: {
				var baseExpr = Check(ctx, call.BaseExpr);
				var arguments = new AstListNode<ExpressionNode>(call.Arguments.Location, call.Arguments.Select(arg => Check(ctx, arg)).ToList().AsReadOnly());
				var effects = ctx.NewMetaEffect();
				ctx.AddEffectConstraint(new EffectConstraint.AtMost(arguments.Location, effects, ctx.SideEffects()));
				Typ baseType = ctx.Force(baseExpr.Type!);

				Typ t = ctx.NewMeta();
				var argTs = arguments.Select(_ => ctx.NewMeta()).Append(ctx.NewMeta()).Cast<Typ>().ToList().AsReadOnly();
				ctx.Unify(baseExpr.Location, argTs.First(), t);
				var funcType = new Apply(new Function(new List<Effect>(new[] {effects}).AsReadOnly()), argTs);
				ctx.Unify(baseExpr.Location, funcType, baseType);
				argTs.Skip(1).Zip(arguments, (a, b) => { ctx.Unify(b.Location, a, b.Type!); return true; }).All(b => b);

				return new CallNode(call.Location, baseExpr, arguments, t);
			}
		}
		throw new Exception($"Unimplemented: Check({expression.GetType().Name})");
	}

	private StatementNode Check(Context ctx, StatementNode statement) {
		switch (statement) {
			case ReturnStatementNode ret: {
				var expression = ret.Expression;
				if (expression != null) {
					expression = Check(ctx, expression);
					ctx.Unify(expression.Location, expression.Type!, ctx.ReturnType());
				} else {
					throw new Exception($"Empty return statements are not allowed yet");
				}
				return new ReturnStatementNode(ret.Location, expression);
			}
			case DeclareVarNode dec: {
				var declaredType = Check(ctx, dec.Type);
				var expressionType = Check(ctx, dec.Expression);
				ctx.Unify(declaredType.Location, declaredType.Type, expressionType.Type);
				if(!ctx.DeclareVar(dec.Name, declaredType.Type!)) throw new TypeError(dec.Location, $"Variable {dec.Name} already declared");
				return new DeclareVarNode(dec.Location, dec.Name, declaredType, expressionType);
			}
			case FunctionNode func: {
				return Check(ctx, func);
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
				return new Function(Zonk(ctx, f.Effects));
			case Trivial:
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
		Console.WriteLine($"HasMeta({type}, {safeMetaLimit})");
		if (type is Meta m) {
			return m.ID >= safeMetaLimit;
		}
		if (type is Apply a) {
			return HasMeta(a.BaseType, safeMetaLimit) || a.ParameterTypes.Select(s => HasMeta(s, safeMetaLimit)).Any(b => b);
		}
		if (type is Function f) {
			return false;
		}
		if (type is Trivial tr) {
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
		ctx.NewVariableScope(false);

		var sideEffects = new AstListNode<SideEffectNode>(function.SideEffects.Location, function.SideEffects.Select(s => Check(ctx, s)).ToList().AsReadOnly());
		var parameters  = new AstListNode<ParameterNode>(function.Parameters.Location, function.Parameters.Select(p => Check(ctx, p, allowHole: !topLevel)).ToList().AsReadOnly());
		var returnType  = Check(ctx, function.ReturnType, allowHole: !topLevel);

		ctx.NewFunction(returnType.Type!, sideEffects.Select(s => s.Effect!).ToList().AsReadOnly());

		if (topLevel)
			ctx.NewMetaScope();
		// function-scoped metas

		var body = shallowPass ? function.Body : new AstListNode<StatementNode>(function.Body.Location, function.Body.Select(s => Check(ctx, s)).ToList().AsReadOnly());

		var func = shallowPass ? function : Zonk<FunctionNode>(ctx, new(function.Location, function.Name, sideEffects, parameters, returnType, body));

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
			if (!ctx.DeclareVar(function.Name, new Apply(new Function(sideEffects.Select(e => e.Effect!).ToList().AsReadOnly()), new Typ[] {returnType.Type!}.Concat(parameters.Select(p => p.Type.Type!)).ToList().AsReadOnly()), isFunc: true)) throw new TypeError(function.Location, $"Variable {function.Name} already declared");

		return func;
	}

	public FileNode Check(FileNode tree) {
		var ctx = new Context();
		ctx.NewMetaScope();
		var typeDefs = tree.TypeDefs.Select(t => Check(ctx, t)).ToList().AsReadOnly();
		foreach (var f in tree.Functions) Check(ctx, f, topLevel: true, shallowPass: true);
		var functions = tree.Functions.Select(f => Check(ctx, f, topLevel: true)).ToList().AsReadOnly();
		ctx.EndMetaScope();
		ctx.PrintGlobals();
		return new(tree.Location, functions, typeDefs);
	}

	public class Context {
		Dictionary<string,Typ> namedTypes = new() {
			{ "i32", new Trivial("i32") },
		};

		List<(BinOpKind, Typ, Typ, Typ)> binOperators = new() {
			(BinOpKind.Add, new Trivial("i32"), new Trivial("i32"), new Trivial("i32")),
		};

		public ReadOnlyCollection<(BinOpKind, Typ, Typ, Typ)> GetBinOperators()
			=> binOperators.AsReadOnly();

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
			public FunctionInfo(Typ returnType, ReadOnlyCollection<Effect> effects) {
				ReturnType = returnType;
				Effects = effects;
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
				Console.WriteLine($"Set({id}, {type})");
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
			Dictionary<string, (Typ type, bool isFunc)> variables = new();
			bool CanSearchParent { get; }

			public VariableScope(bool canSearchParent) : this(canSearchParent, null) {}
			public VariableScope(bool canSearchParent, VariableScope? parent) {
				Parent = parent;
				CanSearchParent = canSearchParent;
			}

			public bool Declare(string name, Typ type, bool isFunc) {
				if (variables.ContainsKey(name)) return false;
				variables[name] = (type, isFunc);
				return true;
			}

			public bool Lookup(string name, out Typ? type, bool returnOnlyFunctions = false) {
				type = null;
				if (variables.ContainsKey(name)) {
					var v = variables[name];

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
				if (Parent != null) return Parent.Lookup(name, out type, returnOnlyFunctions || !CanSearchParent);
				return false;
			}

			public override string ToString() => string.Join("\n", variables.Select((pair) => $"\t{pair.Key}: {(pair.Value.isFunc ? "[fn]" : "")}{pair.Value.type}"));
		}

		public Context() {

		}

		public void NewFunction(Typ returnType, ReadOnlyCollection<Effect> effects) {
			functionData.Push(new(returnType, effects));
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

		public void NewMetaScope() {
			metas = new(metas);
		}

		public void EndMetaScope() {
			Console.WriteLine($"Dropping meta scope:\n{metas}");
			if (metas.Parent == null) throw new Exception("Ended meta scope too often");
			metas = metas.Parent;
		}

		public void NewVariableScope(bool canSearchParent) {
			variables = new(canSearchParent, variables);
		}

		public void EndVariableScope() {
			Console.WriteLine($"Dropping variable scope:\n{variables}");
			if (variables.Parent == null) throw new Exception("Ended variable scope too often");
			variables = variables.Parent;
		}

		public bool DeclareVar(string name, Typ type, bool isFunc = false) {
			return variables.Declare(name, type, isFunc);
		}

		public bool LookupVar(string name, out Typ? type) {
			return variables.Lookup(name, out type);
		}

		public Typ Force(Typ t) {
			if (t is Meta m) {
				if (metas.Get(m.ID) != null) return Force(metas.Get(m.ID)!);
			}
			return t;
		}

		public bool Occurs(int id, Typ t) {
			switch (t) {
				case Function: return false;
				case Trivial: return false;
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

			Console.WriteLine($"Unify({l}, {r})");

			if (l is Trivial lT && r is Trivial rT) {
				if (lT.Name != rT.Name) throw new TypeError(loc, $"{lT.Name} and {rT.Name} are not compatible types");
				return;
			} else if (l is Function lF && r is Function rF) {
				// check the effects
				Unify(loc, lF.Effects, rF.Effects);
				return;
			} else if (l is Apply lA && r is Apply rA) {
				if (lA.ParameterTypes.Count != rA.ParameterTypes.Count)
					throw new TypeError(loc, $"Generic parameter count does not match {lA.ParameterTypes.Count} {rA.ParameterTypes.Count}");
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
	}
}
