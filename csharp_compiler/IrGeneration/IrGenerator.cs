using System;
using System.Linq;
using System.Collections.Generic;

using Nyapl.Parsing.Tree;

using Nyapl.Localizing;

using Nyapl.Typing;

namespace Nyapl.IrGeneration;

public class IrGenerator {
	private List<IrInstr> Generate(Context ctx, ExpressionNode expression) {
		List<IrInstr> instructions = new();

		switch (expression) {
			case TupleNode tuple: {
					var valRegs = new (ulong, ulong)[tuple.Values.Children.Count];
					var rollingOffset = 0ul;
					for (var i = 0; i < valRegs.Length; i++) {
						instructions.AddRange(Generate(ctx, tuple.Values.Children[i]));
						valRegs[i] = (ctx.GetPreviousRegister(), rollingOffset);
						rollingOffset += ctx.TypeCtx.GetSize(tuple.Values.Children[i].Type!);
					}
					var register = ctx.GetNewRegister(ctx.TypeCtx.GetSize(tuple.Type!));
					instructions.AddRange(valRegs.Select(r => new IrInstr(
							IrInstr.IrKind.StoreTupleSection,
							register,
							r.Item1,
							r.Item2
						)));
				} break;
			case BoolLiteralNode boolean: {
					instructions.Add(new(
						IrInstr.IrKind.BoolLiteral,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(TypeChecker.boolean)),
						boolean.Value ? 1ul : 0ul
					));
				} break;
			case IntLiteralNode integer: {
					instructions.Add(new(
						IrInstr.IrKind.IntLiteral,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(TypeChecker.i32)),
						integer.Value
					));
				} break;
			case IntrinsicNode intrinsic: {
						var index = Array.IndexOf(ctx.Platform.Intrinsics.Keys.ToArray(), intrinsic.Name);
						instructions.Add(new(
							IrInstr.IrKind.LoadIntrinsic,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							(ulong)index
						));
					} break;
			case VarLookupNode lookup: {
					var register = ctx.GetVariable(lookup.Name);
					if (register == 0xFFFF_FFFF_FFFF_FFFF) {
						// we are looking up a function
						instructions.Add(new(
							IrInstr.IrKind.LoadFunction,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							ctx.GetFunction(lookup.Name)
						));
					} else {
						// variable found
						instructions.Add(new(
							IrInstr.IrKind.Copy,
							ctx.GetNewRegister(ctx.TypeCtx.GetSize(lookup.Type!)),
							register
						));
					}
				} break;
			case CallNode call: {
					instructions.AddRange(Generate(ctx, call.BaseExpr));
					var baseReg = ctx.GetPreviousRegister();
					var argRegs = new (ulong, ulong)[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						instructions.AddRange(Generate(ctx, call.Arguments.Children[i]));
						argRegs[i] = (ctx.GetPreviousRegister(), ctx.GetPreviousRegister() & 0xFFFF_0000_0000_0000);
					}
					for (ulong i = 0; i < (ulong)argRegs.Length; i++) {
						instructions.Add(new(
							IrInstr.IrKind.StoreParam,
							i | (argRegs[i].Item2),
							argRegs[i].Item1
						));
					}
					instructions.Add(new(
						IrInstr.IrKind.Call,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(call.Type!)),
						baseReg,
						(ulong)call.Arguments.Children.Count
					));
				} break;
			case BinOpNode bin: {
					var size = ctx.TypeCtx.GetSize(bin.Type!);
					instructions.AddRange(Generate(ctx, bin.LExpr));
					var leftReg = ctx.GetPreviousRegister();
					instructions.AddRange(Generate(ctx, bin.RExpr));
					var rightReg = ctx.GetPreviousRegister();
					switch (bin.OP) {
						case BinOpKind.Multiply:
							instructions.Add(new(
								IrInstr.IrKind.Multiply,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Divide:
							instructions.Add(new(
								IrInstr.IrKind.Divide,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Modulo:
							instructions.Add(new(
								IrInstr.IrKind.Modulo,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Add:
							instructions.Add(new(
								IrInstr.IrKind.Add,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Subtract:
							instructions.Add(new(
								IrInstr.IrKind.Subtract,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.Equal:
							instructions.Add(new(
								IrInstr.IrKind.Equal,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						case BinOpKind.NotEq:
							instructions.Add(new(
								IrInstr.IrKind.NotEq,
								ctx.GetNewRegister(size),
								leftReg,
								rightReg
							));
							break;
						default:
							throw new Exception($"Generating `BinOpKind.{bin.OP}` not implemented yet");
					}
				} break;
			case UnOpNode un: {
					var size = ctx.TypeCtx.GetSize(un.Type!);
					instructions.AddRange(Generate(ctx, un.Expr));
					var reg = ctx.GetPreviousRegister();
					switch (un.OP) {
						case UnOpKind.Not:
							instructions.Add(new(
								IrInstr.IrKind.Not,
								ctx.GetNewRegister(size),
								reg
							));
							break;
						case UnOpKind.Negative:
							instructions.Add(new(
								IrInstr.IrKind.Negative,
								ctx.GetNewRegister(size),
								reg
							));
							break;
						case UnOpKind.Positive:
							instructions.Add(new(
								IrInstr.IrKind.Positive,
								ctx.GetNewRegister(size),
								reg
							));
							break;
						default:
							throw new Exception($"Generating `UnOpKind.{un.OP}` not implemented yet");
					}
				} break;
			default:
				throw new Exception($"Generate(ctx, {expression.GetType().Name}) not implemented yet");
		}

		return instructions;
	}

	private List<IrInstr> Generate(Context ctx, LValueNode lVal, ulong sourceRegister) {
		List<IrInstr> instructions = new();

		switch (lVal) {
			case NamedLValueNode named: {
					ctx.SetVariable(named.Name, sourceRegister);
				} break;
			default:
				throw new Exception($"Generate(ctx, {lVal.GetType().Name}, sourceRegister) not implemented yet");
		}

		return instructions;
	}

	private List<IrInstr> Generate(Context ctx, StatementNode statement) {
		List<IrInstr> instructions = new();

		switch (statement) {
			case NoopStatementNode: break; // NOP
			case DeclareVarNode v: {
				instructions.AddRange(Generate(ctx, v.Expression));
				ctx.SetVariable(v.Name, ctx.GetPreviousRegister());
			} break;
			case ReassignNode reassign: {
				instructions.AddRange(Generate(ctx, reassign.Expr));
				ulong sourceRegister = ctx.GetPreviousRegister();
				instructions.AddRange(Generate(ctx, reassign.LVal, sourceRegister));
			} break;
			case DestructureNode d: {
				instructions.AddRange(Generate(ctx, d.Expression));
				ulong tupReg = ctx.GetPreviousRegister();
				ulong rollingOffset = 0;
				for (int i = 0; i < d.Names.Children.Count; i++) {
					var name = d.Names.Children[i];
					switch (name) {
						case NamedDestructureNode named: {
								var reg = ctx.GetNewRegister(ctx.TypeCtx.GetSize(named.Type!));
								instructions.Add(new(
									IrInstr.IrKind.LoadTupleSection,
									reg,
									tupReg,
									rollingOffset
								));
								ctx.SetVariable(named.Name, reg);
							} break;
						case HoleDestructureNode: break;
						default:
							throw new Exception($"Generating DestructureNode.{name.GetType().Name} not implemented yet");
					}
					rollingOffset += ctx.TypeCtx.GetSize(name.Type!);
				}
			} break;
			case ReturnStatementNode r: {
				instructions.AddRange(Generate(ctx, r.Expression));
				instructions.Add(new(
					IrInstr.IrKind.Return,
					ctx.GetPreviousRegister()
				));
			} break;
			case UnsafeStatementNode u: {
				// safety should have been checked by the typechecker, so we just ignore it and append all the instructions in this unsafe block
				foreach (var s in u.Body) {
					instructions.AddRange(Generate(ctx, s));
				}
			} break;
			case IfStatementNode i: {
				// IF
				instructions.AddRange(Generate(ctx, i.IfExpr));
				ulong exprReg = ctx.GetPreviousRegister();
				int jumpToNext = instructions.Count;
				List<int> jumpToEnd = new();
				const ulong DEFAULT_OFFSET = 0xFFFF_FFFF_FFFF_FFFF;
				instructions.Add(new(
					IrInstr.IrKind.JumpIfFalse,
					DEFAULT_OFFSET,
					exprReg
				));
				foreach (var s in i.IfBody) {
					instructions.AddRange(Generate(ctx, s));
				}
				jumpToEnd.Add(instructions.Count);
				instructions.Add(new(
					IrInstr.IrKind.JumpAlways,
					DEFAULT_OFFSET
				));
				var next = instructions[jumpToNext];
				instructions[jumpToNext] = new(next.Kind, (ulong)(instructions.Count - jumpToNext), next.Param1, next.Param2);

				// ELIF
				foreach (var e in i.Elifs) {
					instructions.AddRange(Generate(ctx, e.Expr));
					exprReg = ctx.GetPreviousRegister();
					jumpToNext = instructions.Count;
					instructions.Add(new(
						IrInstr.IrKind.JumpIfFalse,
						DEFAULT_OFFSET,
						exprReg
					));
					foreach (var s in e.Body) {
						instructions.AddRange(Generate(ctx, s));
					}
					jumpToEnd.Add(instructions.Count);
					instructions.Add(new(
						IrInstr.IrKind.JumpAlways,
						DEFAULT_OFFSET
					));
					next = instructions[jumpToNext];
					instructions[jumpToNext] = new(next.Kind, (ulong)(instructions.Count - jumpToNext), next.Param1, next.Param2);
				}

				// ELSE
				if (i.Else != null) {
					foreach (var s in i.Else.Body) {
						instructions.AddRange(Generate(ctx, s));
					}
				}

				// time to patch the jumpToEnd table
				foreach (var idx in jumpToEnd) {
					next = instructions[idx];
					instructions[idx] = new(next.Kind, (ulong)(instructions.Count - idx), next.Param1, next.Param2);
				}
			} break;
			default:
				throw new Exception($"Generate(ctx, {statement.GetType().Name}) not implemented yet");
		}

		return instructions;
	}

	private List<IrInstr> Generate(Context ctx, FunctionNode function) {
		List<IrInstr> instructions = new();

		// add arguments to registers
		ulong i = 0;
		foreach (var param in function.Parameters) {
			instructions.Add(new(
				IrInstr.IrKind.LoadArgument,
				ctx.GetNewRegister(ctx.TypeCtx.GetSize(param.Type.Type!)),
				((ulong)ctx.TypeCtx.GetSize(param.Type.Type!) << 48) | (ulong)(i++)
			));
			ctx.SetVariable(param.Name, ctx.GetPreviousRegister());
		}

		foreach (var statement in function.Body) {
			instructions.AddRange(Generate(ctx, statement));
		}

		return instructions;
	}

	public IrList Generate(TypedFileNode file) {
		List<IrInstr> instructions = new();

		Dictionary<string, int> functions = new();
		Context ctx = new(file.Platform, file.Functions.Select(f => f.Name).ToArray(), file.Context);

		foreach(var function in file.Functions) {
			int fnIdx = instructions.Count;
			instructions.AddRange(Generate(ctx, function));
			functions[function.Name] = fnIdx;
		}

		return new(file.Platform, instructions.AsReadOnly(), functions.AsReadOnly());
	}

	private class Context {
		public Context(Localizer.Platform platform, string[] functions, TypeChecker.Context typeCtx) {
			Platform = platform;
			Functions = functions;
			TypeCtx = typeCtx;
		}

		public Localizer.Platform Platform { get; }
		public TypeChecker.Context TypeCtx { get; }
		public string[] Functions { get; }

		private ulong currentRegister = 0;
		private ulong previousRegister = 0;
		private Dictionary<string, ulong> variables = new();

		public ulong GetNewRegister(ushort size) => previousRegister = currentRegister++ | (ulong)size << 48;
		public ulong GetPreviousRegister() => previousRegister;

		public ulong GetFunction(string name) => (ulong)Array.IndexOf(Functions, name);

		public void SetVariable(string name, ulong register) => variables[name] = register;
		public ulong GetVariable(string name) => variables.ContainsKey(name) ? variables[name] : 0xFFFF_FFFF_FFFF_FFFF;
	}
}
