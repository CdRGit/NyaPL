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
					if (tuple.Values.Children.Count == 0) {
						block.AddInstr(new(
							IrInstr.IrKind.EmptyTuple,
							ctx.GetNewRegister(0)
						));
					}

					ushort size = 0;
					IrParam.Register? prevreg = null;
					for (var i = 0; i < tuple.Values.Children.Count; i++) {
						block = Generate(block, ctx, tuple.Values.Children[i]);
						var reg = ctx.GetPreviousRegister();
						size += ctx.TypeCtx.GetSize(tuple.Values.Children[i].Type!);
						if (i == 0) {
							prevreg = reg;
						} else {
							var register = ctx.GetNewRegister(size);
							block.AddInstr(new(
								IrInstr.IrKind.AppendTupleSection,
								ctx.GetNewRegister(size),
								prevreg!,
								reg
							));
						}
					}
					return block;
				}
			case BoolLiteralNode boolean: {
					block.AddInstr(new(
						IrInstr.IrKind.BoolLiteral,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(TypeChecker.boolean)),
						new IrParam.Bool(boolean.Value)
					));
					return block;
				}
			case IntLiteralNode integer: {
					block.AddInstr(new(
						IrInstr.IrKind.IntLiteral,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(TypeChecker.i32)),
						new IrParam.Int(integer.Value)
					));
					return block;
				}
			case IntrinsicNode intrinsic: {
						var index = Array.IndexOf(ctx.Platform.Intrinsics.Keys.ToArray(), intrinsic.Name);
						block.AddInstr(new(
							IrInstr.IrKind.LoadIntrinsic,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							new IrParam.Intrinsic((ulong)index)
						));
						return block;
					}
			case VarLookupNode lookup: {
					var function = ctx.GetFunction(lookup.Name);
					const ulong notFound = 0xFFFF_FFFF_FFFF_FFFF;
					if (function != notFound) {
						// we are looking up a function
						block.AddInstr(new(
							IrInstr.IrKind.LoadFunction,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							new IrParam.Function((ulong)ctx.GetFunction(lookup.Name))
						));
					} else {
						// variable found
						block.AddInstr(new(
							IrInstr.IrKind.LoadLocal,
							ctx.GetNewRegister(ctx.TypeCtx.GetSize(lookup.Type!)),
							new IrParam.Local(lookup.Name, ctx.TypeCtx.GetSize(lookup.Type!))
						));
					}
					return block;
				}
			case CallNode call: {
					block = Generate(block, ctx, call.BaseExpr);
					var baseReg = ctx.GetPreviousRegister();
					var argRegs = new (IrParam.Register, ushort)[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						block = Generate(block, ctx, call.Arguments.Children[i]);
						argRegs[i] = (ctx.GetPreviousRegister(), ctx.GetPreviousRegister().Size);
					}
					for (uint i = 0; i < argRegs.Length; i++) {
						block.AddInstr(new(
							IrInstr.IrKind.StoreParam,
							new IrParam.Parameter(argRegs[i].Item2, i),
							argRegs[i].Item1
						));
					}

					var fType = ((call.BaseExpr.Type! as Apply)!.BaseType as Function)!;

					var pure = fType.IsIntrinsic == false && !fType.Effects.Any();

					block.AddInstr(new(
						pure ? IrInstr.IrKind.Call : IrInstr.IrKind.CallImpure,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(call.Type!)),
						baseReg,
						new IrParam.Count((ulong)call.Arguments.Children.Count)
					));
					return block;
				}
			case BinOpNode bin: {
					var size = ctx.TypeCtx.GetSize(bin.Type!);
					block = Generate(block, ctx, bin.LExpr);
					var leftReg = ctx.GetPreviousRegister();
					block = Generate(block, ctx, bin.RExpr);
					var rightReg = ctx.GetPreviousRegister();
					switch (bin.OP) {
						case BinOpKind.Multiply:
							block.AddInstr(new(
								IrInstr.IrKind.Multiply,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Divide:
							block.AddInstr(new(
								IrInstr.IrKind.Divide,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Modulo:
							block.AddInstr(new(
								IrInstr.IrKind.Modulo,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Add:
							block.AddInstr(new(
								IrInstr.IrKind.Add,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Subtract:
							block.AddInstr(new(
								IrInstr.IrKind.Subtract,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Equal:
							block.AddInstr(new(
								IrInstr.IrKind.Equal,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.NotEq:
							block.AddInstr(new(
								IrInstr.IrKind.NotEq,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						default:
							throw new Exception($"Generating `BinOpKind.{bin.OP}` not implemented yet");
					}
					return block;
				}
			case UnOpNode un: {
					var size = ctx.TypeCtx.GetSize(un.Type!);
					block = Generate(block, ctx, un.Expr);
					var reg = ctx.GetPreviousRegister();
					switch (un.OP) {
						case UnOpKind.Not:
							block.AddInstr(new(
								IrInstr.IrKind.Not,
								ctx.GetNewRegister(size),
								reg
							));
							break;
						case UnOpKind.Negative:
							block.AddInstr(new(
								IrInstr.IrKind.Negative,
								ctx.GetNewRegister(size),
								reg
							));
							break;
						case UnOpKind.Positive:
							block.AddInstr(new(
								IrInstr.IrKind.Positive,
								ctx.GetNewRegister(size),
								reg
							));
							break;
						default:
							throw new Exception($"Generating `UnOpKind.{un.OP}` not implemented yet");
					}
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
							IrInstr.IrKind.LoadFunction,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							new IrParam.Function((ulong)ctx.GetFunction(lookup.Name))
						));
					} else {
						// variable found
						block.AddInstr(new(
							IrInstr.IrKind.LoadLocal,
							ctx.GetNewRegister(ctx.TypeCtx.GetSize(lookup.Type!)),
							new IrParam.Local(lookup.Name, ctx.TypeCtx.GetSize(lookup.Type!))
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
						IrInstr.IrKind.StoreLocal,
						new IrParam.Local(named.Name, sourceRegister.Size),
						sourceRegister
					));
					return block;
				}
			case TupleLValueNode tuple: {
					ulong rollingOffset = 0;
					for (int i = 0; i < tuple.Children.Children.Count; i++) {
						var child = tuple.Children.Children[i];
						var size = ctx.TypeCtx.GetSize(child.Type!);
						var reg = ctx.GetNewRegister(size);
						block.AddInstr(new(
							IrInstr.IrKind.LoadTupleSection,
							reg,
							sourceRegister,
							new IrParam.Offset(rollingOffset)
						));
						block = Generate(block, ctx, child, reg);
						rollingOffset += size;
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
				var reg = ctx.GetNewRegister(ctx.TypeCtx.GetSize(named.Type!));
				block.AddInstr(new(
					IrInstr.IrKind.LoadTupleSection,
					reg,
					tupReg,
					new IrParam.Offset(offset)
				));
				block.AddInstr(new(
					IrInstr.IrKind.StoreLocal,
					new IrParam.Local(named.Name, reg.Size),
					reg
				));
				return block;
			}
		case HoleDestructureNode: return block;
		case TupleDestructureNode tuple: {
				ulong newOffset = offset;
				foreach (var child in tuple.Children) {
					block = Generate(block, ctx, child, tupReg, newOffset);
					newOffset += ctx.TypeCtx.GetSize(child.Type!);
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
					IrInstr.IrKind.StoreLocal,
					new IrParam.Local(v.Name, ctx.GetPreviousRegister().Size),
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
				ulong rollingOffset = 0;
				for (int i = 0; i < d.Items.Children.Count; i++) {
					var item = d.Items.Children[i];
					block = Generate(block, ctx, item, tupReg, rollingOffset);
					rollingOffset += ctx.TypeCtx.GetSize(item.Type!);
				}
				return block;
			}
			case ReturnStatementNode r: {
				block = Generate(block, ctx, r.Expression);
				block.AddInstr(new(
					IrInstr.IrKind.Return,
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
					IrInstr.IrKind.BranchAlways,
					new IrParam.Block(expr)
				));
				block.AddConnection(expr);
				block = expr;
				block = Generate(block, ctx, w.Expr);
				var body = ctx.NewBlock();
				var end = ctx.NewBlock();
				block.AddInstr(new(
					IrInstr.IrKind.BranchBool,
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
					IrInstr.IrKind.BranchAlways,
					new IrParam.Block(expr)
				));
				body.AddConnection(expr);
				return end;
			}
			case StandaloneCallNode call: {
					block = Generate(block, ctx, call.BaseExpr);
					var baseReg = ctx.GetPreviousRegister();
					var argRegs = new (IrParam.Register, ushort)[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						block = Generate(block, ctx, call.Arguments.Children[i]);
						argRegs[i] = (ctx.GetPreviousRegister(), ctx.GetPreviousRegister().Size);
					}
					for (uint i = 0; i < argRegs.Length; i++) {
						block.AddInstr(new(
							IrInstr.IrKind.StoreParam,
							new IrParam.Parameter(argRegs[i].Item2, i),
							argRegs[i].Item1
						));
					}

					var fType = ((call.BaseExpr.Type! as Apply)!.BaseType as Function)!;

					var pure = fType.IsIntrinsic == false && !fType.Effects.Any();

					block.AddInstr(new(
						pure ? IrInstr.IrKind.Call : IrInstr.IrKind.CallImpure,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(call.Type!)),
						baseReg,
						new IrParam.Count((ulong)call.Arguments.Children.Count)
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
			IrInstr.IrKind.BranchBool,
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
			IrInstr.IrKind.BranchAlways,
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
				IrInstr.IrKind.BranchBool,
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
				IrInstr.IrKind.BranchAlways,
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
				IrInstr.IrKind.BranchAlways,
				new IrParam.Block(end)
			));
			body.AddConnection(end);
		}
		return end;
	}

	private IrBlock Generate(IrBlock block, Context ctx, FunctionNode function) {
		// move arguments to registers
		uint i = 0;
		foreach (var param in function.Parameters) {
			block.AddInstr(new(
				IrInstr.IrKind.LoadArgument,
				ctx.GetNewRegister(ctx.TypeCtx.GetSize(param.Type.Type!)),
				new IrParam.Argument(ctx.TypeCtx.GetSize(param.Type.Type!), i++)
			));
			block.AddInstr(new(
				IrInstr.IrKind.StoreLocal,
				new IrParam.Local(param.Name, ctx.GetPreviousRegister().Size),
				ctx.GetPreviousRegister()
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

		public IrParam.Register GetNewRegister(ushort size) => previousRegister = new IrParam.Register(size, currentRegister++);
		public IrParam.Register GetPreviousRegister() => previousRegister ?? throw new Exception("no previous register");
	}
}
