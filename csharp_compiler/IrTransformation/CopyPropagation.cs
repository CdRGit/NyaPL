using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using Nyapl.IrGeneration;

namespace Nyapl.IrTransformation;

public class CopyPropagation {
	private IrInstr SimplePropagate(IrInstr instr, Context ctx) {
		if (instr.Params.Length == 0) return instr;

		var vals = new List<IrParam>();
		if (instr[0] is IrParam.Register r) {
			ctx.SetRegister(r, r);
		}
		vals.Add(instr[0]!);
		vals.AddRange(instr.Params.Skip(1).Select(p => ctx.Propagate(p)));
		return new(instr.Kind, vals.ToArray());
	}

	private IrBlock Transform(IrBlock source, Context ctx) {
		if (ctx.Finished(source)) return ctx.GetReplacement(source); // early out

		var block = ctx.GetReplacement(source);
		foreach (var instr in source.Instructions) {
			switch (instr.Kind) {
				case IrKind.LoadArguments:
					foreach (var param in instr.Params) {
						var r = (param as IrParam.Register)!;
						ctx.SetRegister(r, r);
					}
					block.AddInstr(instr);
					break;
				case IrKind.Phi: {
					var r = (instr[0] as IrParam.Register)!;
					ctx.SetRegister(r, r);
					block.AddInstr(instr);
				} break;
				case IrKind.Copy:
					switch (instr[0]) {
						case IrParam.Register r:
							// check the other value
							switch (instr[1]) {
								case IrParam.Int:
									ctx.SetRegister(r, r);
									block.AddInstr(instr);
									break;
								case IrParam.Register s:
									ctx.SetRegister(r, s);
									//block.AddInstr(instr);
									break;
								default:
									throw new Exception($"Copying from {instr[1]} not implemented yet");
							}
							break;
						default:
							throw new Exception($"Copying to {instr[0]} not implemented yet");
					}
					break;
				case IrKind.Intrinsic:
				case IrKind.IntrinsicImpure:
				case IrKind.LoadFunction:
				case IrKind.Call:
				case IrKind.CreateTuple:
					block.AddInstr(SimplePropagate(instr, ctx));
					break;
				case IrKind.BranchAlways:
				case IrKind.BranchBool:
				case IrKind.Return:
					block.AddInstr(new(instr.Kind, instr.Params.Select(p => ctx.Propagate(p)).ToArray()));
					break;
				default:
					throw new Exception($"Transform({instr}) not implemented yet");
			}
		}

		foreach (var connection in source.Outgoing) {
			block.AddConnection(Transform(connection.node, ctx), connection.label);
		}

		return block;
	}

	public IrResult Transform(IrResult input) {
		var ctx = new Context(input.UsedRegisters);

		var functions = new Dictionary<string, IrBlock>();

		foreach (var function in input.Functions) {
			functions[function.Key] = Transform(function.Value, ctx);
		}

		return new(input.Platform, ctx.Blocks, functions.AsReadOnly(), ctx.UsedRegisters);
	}

	class Context {
		public uint UsedRegisters { get; private set; }

		public ReadOnlyCollection<IrBlock> Blocks => blocks.Select(kv => kv.Value).ToList().AsReadOnly();
		Dictionary<int, IrBlock> blocks = new();

		public bool Finished(IrBlock source) => blocks.ContainsKey(source.ID);
		public IrBlock GetReplacement(IrBlock source) => Finished(source) ? blocks[source.ID] : (blocks[source.ID] = new(source.ID));

		public Context(uint usedRegisters) {
			UsedRegisters = usedRegisters;
		}

		Dictionary<uint, IrParam.Register> registers = new();

		public void SetRegister(IrParam.Register r1, IrParam.Register r2) {
			if (registers.ContainsKey(r2.Index))
				registers[r1.Index] = registers[r2.Index];
			else
				registers[r1.Index] = r2;
		}

		public IrParam Propagate(IrParam val) {
			if (val is IrParam.Register r)
				return registers[r.Index];
			return val;
		}
	}
}
