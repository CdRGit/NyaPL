using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Parsing.Tree;

using Nyapl.Localizing;

using Nyapl.Typing;

namespace Nyapl.IrGeneration;

public class IrGenerator {
	private IrBlock Generate(IrBlock block, Context ctx, ExpressionNode expression) {
		switch (expression) {
			case TupleNode tuple: {
					var valRegs = new (ulong, ulong)[tuple.Values.Children.Count];
					ushort size = 0;
					IrParam.Register? prevreg = null;
					for (var i = 0; i < valRegs.Length; i++) {
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
							new IrParam.Local(lookup.Name)
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
					block.AddInstr(new(
						IrInstr.IrKind.Call,
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

	private IrBlock Generate(IrBlock block, Context ctx, LValueNode lVal, IrParam.Register sourceRegister) {
		switch (lVal) {
			case NamedLValueNode named: {
					block.AddInstr(new(
						IrInstr.IrKind.StoreLocal,
						new IrParam.Local(named.Name),
						sourceRegister
					));
					return block;
				}
			default:
				throw new Exception($"Generate(ctx, {lVal.GetType().Name}, sourceRegister) not implemented yet");
		}
	}

	private IrBlock Generate(IrBlock block, Context ctx, StatementNode statement) {
		switch (statement) {
			case NoopStatementNode: return block; // NOP
			case DeclareVarNode v: {
				block = Generate(block, ctx, v.Expression);
				block.AddInstr(new(
					IrInstr.IrKind.StoreLocal,
					new IrParam.Local(v.Name),
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
				for (int i = 0; i < d.Names.Children.Count; i++) {
					var name = d.Names.Children[i];
					switch (name) {
						case NamedDestructureNode named: {
								var reg = ctx.GetNewRegister(ctx.TypeCtx.GetSize(named.Type!));
								block.AddInstr(new(
									IrInstr.IrKind.LoadTupleSection,
									reg,
									tupReg,
									new IrParam.Offset(rollingOffset)
								));
								block.AddInstr(new(
									IrInstr.IrKind.StoreLocal,
									new IrParam.Local(named.Name),
									reg
								));
							} break;
						case HoleDestructureNode: break;
						default:
							throw new Exception($"Generating DestructureNode.{name.GetType().Name} not implemented yet");
					}
					rollingOffset += ctx.TypeCtx.GetSize(name.Type!);
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
				throw new Exception("stubbed out for my sanity, don't use if rn");
				/* uhhhhh yea not yet lmfao
				var preVars = ctx.GetVariables();
				Dictionary<IrParam.Label, Dictionary<string, IrParam.Register>> deltaVars = new();
				Dictionary<string, IrParam.Register> bodyDeltas = new();
				var condLabel = ctx.GetNewLabel("if_cond");
				var bodyLabel = ctx.GetNewLabel("if_body");
				var elseLabel = ctx.GetNewLabel("else");
				// IF
				ctx.SetVariables(preVars);
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
				bodyDeltas = new();
				foreach (var v in ctx.GetVariables()) {
					if (preVars.ContainsKey(v.Key) && preVars[v.Key] != v.Value)
						bodyDeltas.Add(v.Key, v.Value);
				}
				deltaVars[bodyLabel] = bodyDeltas;
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
					ctx.SetVariables(preVars);
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
					bodyDeltas = new();
					foreach (var v in ctx.GetVariables()) {
						if (preVars.ContainsKey(v.Key) && preVars[v.Key] != v.Value)
							bodyDeltas.Add(v.Key, v.Value);
					}
					deltaVars[body] = bodyDeltas;
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
					ctx.SetVariables(preVars);
					instructions.Add(new(
						IrInstr.IrKind.Label,
						elseLabel
					));
					foreach (var s in i.Else.Body) {
						instructions.AddRange(Generate(ctx, s));
					}
					bodyDeltas = new();
					foreach (var v in ctx.GetVariables()) {
						if (preVars.ContainsKey(v.Key) && preVars[v.Key] != v.Value)
							bodyDeltas.Add(v.Key, v.Value);
					}
					deltaVars[elseLabel] = bodyDeltas;
					instructions.Add(new(
						IrInstr.IrKind.JumpAlways,
						endLabel
					));
				}

				instructions.Add(new(
					IrInstr.IrKind.Label,
					endLabel
				));

				List<string> changedVars = new();
				foreach (var v in preVars) {
					Console.WriteLine(v);
				}
				foreach (var delta in deltaVars) {
					Console.WriteLine("-");
					foreach (var v in delta.Value) {
						Console.WriteLine(v);
						changedVars.Add(v.Key);
					}
				}
				changedVars = changedVars.Distinct().ToList();
				Console.WriteLine("changed vars:");
				foreach (var v in changedVars) {
					Console.Write(v);
					// need to add phi nodes for all these variables
					Console.WriteLine(":");
					Dictionary<IrParam.Label, IrParam.Register> registers = new();
					foreach (var label in deltaVars.Keys) {
						var vals = deltaVars[label];
						if (vals.ContainsKey(v)) {
							Console.WriteLine($"CHANGED in {label.Name}");
							registers[label] = vals[v];
						}
						else {
							// keep the same
							Console.WriteLine($"UNCHANGED in {label.Name}");
							registers[label] = preVars[v];
						}
					}
					var destReg = ctx.GetNewRegister(preVars[v].Size);
					var list = new IrParam[] {destReg}.Concat(registers.SelectMany(p => new[] {(IrParam)p.Key, (IrParam)p.Value})).ToArray();
					instructions.Add(new(
						IrInstr.IrKind.Phi,
						list
					));
					ctx.SetVariable(v, destReg);
				}*/
			}
			default:
				throw new Exception($"Generate(ctx, {statement.GetType().Name}) not implemented yet");
		}
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
				new IrParam.Local(param.Name),
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

		return new(file.Platform, ctx.GetBlocks(), functions.AsReadOnly());
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
		private IrParam.Register? previousRegister;

		public IrParam.Register GetNewRegister(ushort size) => previousRegister = new IrParam.Register(size, currentRegister++);
		public IrParam.Register GetPreviousRegister() => previousRegister ?? throw new Exception("no previous register");

		private Dictionary<string, int> labels = new();

		public IrParam.Label GetNewLabel(string name) => new IrParam.Label(labels.ContainsKey(name) ? $".{name}_{++labels[name]}" : $".{name}_{labels[name] = 0}");
	}
}
