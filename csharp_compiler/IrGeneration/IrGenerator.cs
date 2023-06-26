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
					ushort size = 0;
					IrParam.Register? prevreg = null;
					for (var i = 0; i < valRegs.Length; i++) {
						instructions.AddRange(Generate(ctx, tuple.Values.Children[i]));
						var reg = ctx.GetPreviousRegister();
						size += ctx.TypeCtx.GetSize(tuple.Values.Children[i].Type!);
						if (i == 0) {
							prevreg = reg;
						} else {
							var register = ctx.GetNewRegister(size);
							instructions.Add(new(
								IrInstr.IrKind.AppendTupleSection,
								ctx.GetNewRegister(size),
								prevreg!,
								reg
							));
						}
					}
				} break;
			case BoolLiteralNode boolean: {
					instructions.Add(new(
						IrInstr.IrKind.BoolLiteral,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(TypeChecker.boolean)),
						new IrParam.Bool(boolean.Value)
					));
				} break;
			case IntLiteralNode integer: {
					instructions.Add(new(
						IrInstr.IrKind.IntLiteral,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(TypeChecker.i32)),
						new IrParam.Int(integer.Value)
					));
				} break;
			case IntrinsicNode intrinsic: {
						var index = Array.IndexOf(ctx.Platform.Intrinsics.Keys.ToArray(), intrinsic.Name);
						instructions.Add(new(
							IrInstr.IrKind.LoadIntrinsic,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							new IrParam.Intrinsic((ulong)index)
						));
					} break;
			case VarLookupNode lookup: {
					var register = ctx.GetVariable(lookup.Name);
					if (register == null) {
						// we are looking up a function
						instructions.Add(new(
							IrInstr.IrKind.LoadFunction,
							ctx.GetNewRegister(ctx.Platform.PointerSize),
							new IrParam.Function((ulong)ctx.GetFunction(lookup.Name))
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
					var argRegs = new (IrParam.Register, ushort)[call.Arguments.Children.Count];
					for (var i = 0; i < call.Arguments.Children.Count; i++) {
						instructions.AddRange(Generate(ctx, call.Arguments.Children[i]));
						argRegs[i] = (ctx.GetPreviousRegister(), ctx.GetPreviousRegister().Size);
					}
					for (uint i = 0; i < argRegs.Length; i++) {
						instructions.Add(new(
							IrInstr.IrKind.StoreParam,
							new IrParam.Parameter(argRegs[i].Item2, i),
							argRegs[i].Item1
						));
					}
					instructions.Add(new(
						IrInstr.IrKind.Call,
						ctx.GetNewRegister(ctx.TypeCtx.GetSize(call.Type!)),
						baseReg,
						new IrParam.Count((ulong)call.Arguments.Children.Count)
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

	private List<IrInstr> Generate(Context ctx, LValueNode lVal, IrParam.Register sourceRegister) {
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
				var sourceRegister = ctx.GetPreviousRegister();
				instructions.AddRange(Generate(ctx, reassign.LVal, sourceRegister));
			} break;
			case DestructureNode d: {
				instructions.AddRange(Generate(ctx, d.Expression));
				var tupReg = ctx.GetPreviousRegister();
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
									new IrParam.Offset(rollingOffset)
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
				var condLabel = ctx.GetNewLabel("if_cond");
				var bodyLabel = ctx.GetNewLabel("if_body");
				var elseLabel = ctx.GetNewLabel("else");
				List<(IrParam.Label condition, IrParam.Label body)> elifLabels = new();
				// IF
				instructions.Add(new(
					IrInstr.IrKind.Label,
					condLabel
				));
				instructions.AddRange(Generate(ctx, i.IfExpr));
				var exprReg = ctx.GetPreviousRegister();
				var endLabel = ctx.GetNewLabel("if_end");
				var nextLabel = ctx.GetNewLabel("if_next");
				instructions.Add(new(
					IrInstr.IrKind.JumpIfFalse,
					nextLabel,
					exprReg
				));
				instructions.Add(new(
					IrInstr.IrKind.Label,
					bodyLabel
				));
				foreach (var s in i.IfBody) {
					instructions.AddRange(Generate(ctx, s));
				}
				instructions.Add(new(
					IrInstr.IrKind.JumpAlways,
					endLabel
				));
				instructions.Add(new(
					IrInstr.IrKind.Label,
					nextLabel
				));
				// ELIF
				foreach (var e in i.Elifs) {
					var cond = ctx.GetNewLabel("elif_cond");
					var body = ctx.GetNewLabel("elif_body");
					elifLabels.Add((cond, body));
					instructions.Add(new(
						IrInstr.IrKind.Label,
						cond
					));
					instructions.AddRange(Generate(ctx, e.Expr));
					exprReg = ctx.GetPreviousRegister();
					nextLabel = ctx.GetNewLabel("elif_next");
					instructions.Add(new(
						IrInstr.IrKind.JumpIfFalse,
						nextLabel,
						exprReg
					));
					instructions.Add(new(
						IrInstr.IrKind.Label,
						body
					));
					foreach (var s in e.Body) {
						instructions.AddRange(Generate(ctx, s));
					}
					instructions.Add(new(
						IrInstr.IrKind.JumpAlways,
						endLabel
					));
					instructions.Add(new(
						IrInstr.IrKind.Label,
						nextLabel
					));
				}

				// ELSE
				if (i.Else != null) {
					instructions.Add(new(
						IrInstr.IrKind.Label,
						elseLabel
					));
					foreach (var s in i.Else.Body) {
						instructions.AddRange(Generate(ctx, s));
					}
					instructions.Add(new(
						IrInstr.IrKind.JumpAlways,
						endLabel
					));
				}

				instructions.Add(new(
					IrInstr.IrKind.Label,
					endLabel
				));
			} break;
			default:
				throw new Exception($"Generate(ctx, {statement.GetType().Name}) not implemented yet");
		}

		return instructions;
	}

	private List<IrInstr> Generate(Context ctx, FunctionNode function) {
		List<IrInstr> instructions = new();
		instructions.Add(new(
			IrInstr.IrKind.Label,
			new IrParam.Label(function.Name)
		));

		// add arguments to registers
		uint i = 0;
		foreach (var param in function.Parameters) {
			instructions.Add(new(
				IrInstr.IrKind.LoadArgument,
				ctx.GetNewRegister(ctx.TypeCtx.GetSize(param.Type.Type!)),
				new IrParam.Argument(ctx.TypeCtx.GetSize(param.Type.Type!), i++)
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

		public ulong GetFunction(string name) => (ulong)Array.IndexOf(Functions, name);


		private uint currentRegister = 0;
		private IrParam.Register? previousRegister;

		public IrParam.Register GetNewRegister(ushort size) => previousRegister = new IrParam.Register(size, currentRegister++);
		public IrParam.Register GetPreviousRegister() => previousRegister ?? throw new Exception("no previous register");


		private Dictionary<string, IrParam.Register> variables = new();

		public void SetVariable(string name, IrParam.Register register) => variables[name] = register;
		public IrParam.Register? GetVariable(string name) => variables.ContainsKey(name) ? variables[name] : null;

		private Dictionary<string, int> labels = new();

		public IrParam.Label GetNewLabel(string name) => new IrParam.Label(labels.ContainsKey(name) ? $".{name}_{++labels[name]}" : $".{name}_{labels[name] = 0}");
	}
}
