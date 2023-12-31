using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;

using Nyapl.Localizing;

using Nyapl.Typing;
using Nyapl.Typing.Types;

namespace Nyapl.IrGeneration;

public class IrGenerator {
	private IrBlock Generate(IrBlock block, Context ctx, ExpressionNode expression) {
		switch (expression) {
			case TupleNode tuple: {
					var registers = tuple.Values.Select(v => {
						block = Generate(block, ctx, v);
						var reg = ctx.GetPreviousRegister();
						return (IrParam)reg;
					}).ToArray();
					block.AddInstr(new(
						IrKind.CreateTuple,
						new [] { ctx.GetNewRegister(tuple.Type!) }.Concat(registers).ToArray()
					));
					return block;
				}
			case BoolLiteralNode boolean: {
					block.AddInstr(new(
						IrKind.Copy,
						ctx.GetNewRegister(boolean.Type!),
						new IrParam.Bool(boolean.Value)
					));
					return block;
				}
			case IntLiteralNode integer: {
					block.AddInstr(new(
						IrKind.Copy,
						ctx.GetNewRegister(integer.Type!),
						// TODO: properly work with more-than-just-32-bit-integers
						new IrParam.Int(integer.Value, 32)
					));
					return block;
				}
			case IntrinsicNode intrinsic: {
						throw new Exception("this should be unreachable");
					}
			case VarLookupNode lookup: {
					var function = ctx.GetFunction(lookup.Name);
					const ulong notFound = 0xFFFF_FFFF_FFFF_FFFF;
					if (function != notFound) {
						// we are looking up a function
						block.AddInstr(new(
							IrKind.LoadFunction,
							ctx.GetNewRegister(lookup.Type!),
							new IrParam.Function(lookup.Name)
						));
					} else {
						// variable found
						block.AddInstr(new(
							IrKind.LoadLocal,
							ctx.GetNewRegister(lookup.Type!),
							new IrParam.Local(lookup.Name, lookup.Type!)
						));
					}
					return block;
				}
			case CallNode call: {
					block = Generate(block, ctx, call.BaseExpr);
					var baseReg = ctx.GetPreviousRegister();
					var argRegs = new IrParam.Register[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						block = Generate(block, ctx, call.Arguments.Children[i]);
						argRegs[i] = ctx.GetPreviousRegister();
					}

					var fType = ((call.BaseExpr.Type! as Apply)!.BaseType as Function)!;

					var pure = !fType.Effects.Any();

					block.AddInstr(new(
						pure ? IrKind.Call : IrKind.CallImpure,
						new [] { ctx.GetNewRegister(call.Type!), baseReg }.Concat(argRegs).ToArray()
					));
					return block;
				}
			case IntrinsicCallNode call: {
					var baseIntrin = new IrParam.Intrinsic(call.BaseExpr.Name, call.BaseExpr.Type!);
					var argRegs = new IrParam.Register[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						block = Generate(block, ctx, call.Arguments.Children[i]);
						argRegs[i] = ctx.GetPreviousRegister();
					}

					var fType = ((call.BaseExpr.Type! as Apply)!.BaseType as Function)!;

					block.AddInstr(new(
						IrKind.IntrinsicImpure,
						new IrParam[] { ctx.GetNewRegister(call.Type!), baseIntrin }.Concat(argRegs).ToArray()
					));
					return block;
				}
			case BinOpNode bin: {
					block = Generate(block, ctx, bin.LExpr);
					var leftReg = ctx.GetPreviousRegister();
					block = Generate(block, ctx, bin.RExpr);
					var rightReg = ctx.GetPreviousRegister();
					block.AddInstr(new(
						IrKind.Intrinsic,
						ctx.GetNewRegister(bin.Type!),
						ctx.TypeCtx.GetOp(bin.LExpr.Type!, bin.RExpr.Type!, bin.OP),
						leftReg,
						rightReg
					));
					return block;
				}
			case UnOpNode un: {
					block = Generate(block, ctx, un.Expr);
					var reg = ctx.GetPreviousRegister();
					block.AddInstr(new(
						IrKind.Intrinsic,
						ctx.GetNewRegister(un.Type!),
						ctx.TypeCtx.GetOp(un.Expr.Type!, un.OP),
						reg
					));
					return block;
				}
			default:
				throw new Exception($"Generate(ctx, {expression.GetType().Name}) not implemented yet");
		}
	}

	// resolve
	private IrBlock Generate(IrBlock block, Context ctx, LValueNode lVal) {
		switch (lVal) {
			case NamedLValueNode lookup: {
					var function = ctx.GetFunction(lookup.Name);
					const ulong notFound = 0xFFFF_FFFF_FFFF_FFFF;
					if (function != notFound) {
						// we are looking up a function
						block.AddInstr(new(
							IrKind.LoadFunction,
							ctx.GetNewRegister(lookup.Type!),
							new IrParam.Function(lookup.Name)
						));
					} else {
						// variable found
						block.AddInstr(new(
							IrKind.LoadLocal,
							ctx.GetNewRegister(lookup.Type!),
							new IrParam.Local(lookup.Name, lookup.Type!)
						));
					}
					return block;
				}
			default:
				throw new Exception($"Generate(ctx, {lVal.GetType().Name}) not implemented yet");
		}
	}

	// assign
	private IrBlock Generate(IrBlock block, Context ctx, LValueNode lVal, IrParam.Register sourceRegister) {
		switch (lVal) {
			case NamedLValueNode named: {
					block.AddInstr(new(
						IrKind.StoreLocal,
						new IrParam.Local(named.Name, sourceRegister.Type),
						sourceRegister
					));
					return block;
				}
			case TupleLValueNode tuple: {
					for (int i = 0; i < tuple.Children.Children.Count; i++) {
						var child = tuple.Children.Children[i];
						var reg = ctx.GetNewRegister(child.Type!);
						block.AddInstr(new(
							IrKind.LoadTupleSection,
							reg,
							sourceRegister,
							new IrParam.Offset((ulong)i)
						));
						block = Generate(block, ctx, child, reg);
					}
					return block;
				}
			default:
				throw new Exception($"Generate(ctx, {lVal.GetType().Name}, sourceRegister) not implemented yet");
		}
	}

	private IrBlock Generate(IrBlock block, Context ctx, DestructureItemNode item, IrParam.Register tupReg, ulong offset) {
		switch (item) {
			case NamedDestructureNode named: {
				var reg = ctx.GetNewRegister(named.Type!);
				block.AddInstr(new(
					IrKind.LoadTupleSection,
					reg,
					tupReg,
					new IrParam.Offset(offset)
				));
				block.AddInstr(new(
					IrKind.StoreLocal,
					new IrParam.Local(named.Name, reg.Type),
					reg
				));
				return block;
			}
		case HoleDestructureNode: return block;
		case TupleDestructureNode tuple: {
				var newTupReg = ctx.GetNewRegister(tuple.Type!);
				block.AddInstr(new(
					IrKind.LoadTupleSection,
					newTupReg,
					tupReg,
					new IrParam.Offset(offset)
				));
				for (int i = 0; i < tuple.Children.Children.Count; i++) {
					var child = tuple.Children.Children[i];
					block = Generate(block, ctx, child, newTupReg, (ulong)i);
				}
				return block;
			}
		default:
			throw new Exception($"Generating DestructureNode.{item.GetType().Name} not implemented yet");
		}
	}

	private IrBlock Generate(IrBlock block, Context ctx, StatementNode statement) {
		switch (statement) {
			case NoopStatementNode: return block; // NOP
			case DeclareVarNode v: {
				block = Generate(block, ctx, v.Expression);
				block.AddInstr(new(
					IrKind.StoreLocal,
					new IrParam.Local(v.Name, ctx.GetPreviousRegister().Type),
					ctx.GetPreviousRegister()
				));
				return block;
			}
			case ReassignNode reassign: {
				block = Generate(block, ctx, reassign.Expr);
				var sourceRegister = ctx.GetPreviousRegister();
				block = Generate(block, ctx, reassign.LVal, sourceRegister);
				return block;
			}
			case DestructureNode d: {
				block = Generate(block, ctx, d.Expression);
				var tupReg = ctx.GetPreviousRegister();
				for (int i = 0; i < d.Items.Children.Count; i++) {
					var item = d.Items.Children[i];
					block = Generate(block, ctx, item, tupReg, (ulong)i);
				}
				return block;
			}
			case ReturnStatementNode r: {
				block = Generate(block, ctx, r.Expression);
				block.AddInstr(new(
					IrKind.Return,
					ctx.GetPreviousRegister()
				));
				return block;
			}
			case UnsafeStatementNode u: {
				// safety should have been checked by the typechecker, so we just ignore it and append all the instructions in this unsafe block
				foreach (var s in u.Body) {
					block = Generate(block, ctx, s);
				}
				return block;
			}
			case IfStatementNode i: {
				return Generate(block, ctx, i);
			}
			case WhileStatementNode w: {
				var expr = ctx.NewBlock();
				block.AddInstr(new(
					IrKind.BranchAlways,
					new IrParam.Block(expr)
				));
				block.AddConnection(expr);
				block = expr;
				block = Generate(block, ctx, w.Expr);
				var body = ctx.NewBlock();
				var end = ctx.NewBlock();
				block.AddInstr(new(
					IrKind.BranchBool,
					ctx.GetPreviousRegister(),
					new IrParam.Block(end),
					new IrParam.Block(body)
				));
				block.AddConnection(end, "false");
				block.AddConnection(body, "true");
				foreach (var s in w.Body) {
					body = Generate(body, ctx, s);
				}
				body.AddInstr(new(
					IrKind.BranchAlways,
					new IrParam.Block(expr)
				));
				body.AddConnection(expr);
				return end;
			}
			case StandaloneCallNode call: {
					block = Generate(block, ctx, call.BaseExpr);
					var baseReg = ctx.GetPreviousRegister();
					var argRegs = new IrParam.Register[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						block = Generate(block, ctx, call.Arguments.Children[i]);
						argRegs[i] = ctx.GetPreviousRegister();
					}

					var fType = ((call.BaseExpr.Type! as Apply)!.BaseType as Function)!;

					var pure = !fType.Effects.Any();

					block.AddInstr(new(
						pure ? IrKind.Call : IrKind.CallImpure,
						new [] { ctx.GetNewRegister(call.Type!), baseReg }.Concat(argRegs).ToArray()
					));
					return block;
				}
			case IntrinsicStandaloneCallNode call: {
					var baseIntrin = new IrParam.Intrinsic(call.BaseExpr.Name, call.BaseExpr.Type!);
					var argRegs = new IrParam.Register[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						block = Generate(block, ctx, call.Arguments.Children[i]);
						argRegs[i] = ctx.GetPreviousRegister();
					}

					var fType = ((call.BaseExpr.Type! as Apply)!.BaseType as Function)!;

					block.AddInstr(new(
						IrKind.IntrinsicImpure,
						new IrParam[] { ctx.GetNewRegister(call.Type!), baseIntrin }.Concat(argRegs).ToArray()
					));
					return block;
				}
			default:
				throw new Exception($"Generate(ctx, {statement.GetType().Name}) not implemented yet");
		}
	}

	private IrBlock Generate(IrBlock block, Context ctx, IfStatementNode @if) {
		var body = ctx.NewBlock();
		var othersIdx = 0;
		var others = Enumerable.Range(0, @if.Elifs.Count * 2 + (@if.Else != null ? 1 : 0)).Select(f => ctx.NewBlock()).ToList();
		var end = ctx.NewBlock();
		others.Add(end);
		var next = others[othersIdx++];
		// IF
		block = Generate(block, ctx, @if.IfExpr);
		var exprReg = ctx.GetPreviousRegister();
		block.AddInstr(new(
			IrKind.BranchBool,
			exprReg,
			new IrParam.Block(next),
			new IrParam.Block(body)
		));
		block.AddConnection(next, "false");
		block.AddConnection(body, "true");
		foreach (var s in @if.IfBody) {
			body = Generate(body, ctx, s);
		}
		body.AddInstr(new(
			IrKind.BranchAlways,
			new IrParam.Block(end)
		));
		body.AddConnection(end);
		// ELIF
		for (int i = 0; i < @if.Elifs.Count; i++) {
			var e = @if.Elifs[i];
			block = next;
			// elif expr is in block
			block = Generate(block, ctx, e.Expr);
			body = others[othersIdx++];
			next = others[othersIdx++];
			exprReg = ctx.GetPreviousRegister();
			block.AddInstr(new(
				IrKind.BranchBool,
				exprReg,
				new IrParam.Block(next),
				new IrParam.Block(body)
			));
			block.AddConnection(next, "false");
			block.AddConnection(body, "true");
			foreach (var s in e.Body) {
				body = Generate(body, ctx, s);
			}
			body.AddInstr(new(
				IrKind.BranchAlways,
				new IrParam.Block(end)
			));
			body.AddConnection(end);
		}

		// ELSE
		if (@if.Else != null) {
			body = next;
			foreach (var s in @if.Else.Body) {
				body = Generate(body, ctx, s);
			}
			body.AddInstr(new(
				IrKind.BranchAlways,
				new IrParam.Block(end)
			));
			body.AddConnection(end);
		}
		return end;
	}

	private IrBlock Generate(IrBlock block, Context ctx, FunctionNode function) {
		// move arguments to registers
		var argList = function.Parameters.Select(p => ctx.GetNewRegister(p.Type.Type!)).ToArray();
		var argListFinal = new List<IrParam>();
		argListFinal.Add(new IrParam.IrType(function.Type));
		argListFinal.AddRange(argList);
		block.AddInstr(new(
			IrKind.LoadArguments,
			argListFinal.ToArray()
		));
		for (int i = 0; i < function.Parameters.Children.Count; i++) {
			var param = function.Parameters.Children[i];
			block.AddInstr(new(
				IrKind.StoreLocal,
				new IrParam.Local(param.Name, argList[i].Type),
				argList[i]
			));
		}

		foreach (var statement in function.Body) {
			block = Generate(block, ctx, statement);
		}

		return block;
	}

	public IrResult Generate(TypedFileNode file) {
		Dictionary<string, IrBlock> functions = new();
		Context ctx = new(file.Platform, file.Functions.Select(f => f.Name).ToArray(), file.Context);

		foreach(var function in file.Functions) {
			IrBlock block = ctx.NewBlock();
			Generate(block, ctx, function);
			functions[function.Name] = block;
		}

		return new(file.Platform, ctx.GetBlocks(), functions.AsReadOnly(), ctx.RegisterCount);
	}

	private class Context {
		public Context(Localizer.Platform platform, string[] functions, TypeChecker.Context typeCtx) {
			Platform = platform;
			Functions = functions;
			TypeCtx = typeCtx;
		}

		private List<IrBlock> blocks = new();

		public IrBlock NewBlock() {
			IrBlock block = new(blocks.Count);
			blocks.Add(block);
			return block;
		}

		public ReadOnlyCollection<IrBlock> GetBlocks() {
			return blocks.AsReadOnly();
		}

		public Localizer.Platform Platform { get; }
		public TypeChecker.Context TypeCtx { get; }
		public string[] Functions { get; }

		public ulong GetFunction(string name) => (ulong)Array.IndexOf(Functions, name);


		private uint currentRegister = 0;
		public uint RegisterCount { get => currentRegister; }
		private IrParam.Register? previousRegister;

		public IrParam.Register GetNewRegister(Typ type) => previousRegister = new IrParam.Register(type, currentRegister++);
		public IrParam.Register GetPreviousRegister() => previousRegister ?? throw new Exception("no previous register");
	}
}
