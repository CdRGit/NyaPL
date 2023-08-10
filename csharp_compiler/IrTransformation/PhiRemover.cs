using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.IrTransformation;

public class PhiRemover {
	IrBlock Transform(Context ctx, IrBlock source) {
		if (ctx.Replaced(source)) return ctx.Replace(source);
		IrBlock block = ctx.Replace(source);

		var phis = GetPhis(ctx, source);
		foreach (var child in source.Outgoing) {
			block.AddConnection(Transform(ctx, child.node), child.label);
		}

		foreach (var instr in source.Instructions) {
			switch (instr.Kind) {
				case IrKind.Phi:
					// delete
					break;
				// branches, here is where we add the pre-copies, as well as doing the block replacement
				case IrKind.BranchAlways:
					// add in the PHIs
					foreach (var phi in phis) {
						block.AddInstr(new(IrKind.Copy, phi.destination, phi.source));
					}
					block.AddInstr(new(IrKind.BranchAlways, new IrParam.Block(ctx.Replace((instr[0] as IrParam.Block)!.Blk))));
					break;
				case IrKind.BranchBool:
					// add in the PHIs
					foreach (var phi in phis) {
						block.AddInstr(new(IrKind.Copy, phi.destination, phi.source));
					}
					block.AddInstr(new(IrKind.BranchBool, instr[0], new IrParam.Block(ctx.Replace((instr[1] as IrParam.Block)!.Blk)), new IrParam.Block(ctx.Replace((instr[2] as IrParam.Block)!.Blk))));
					break;
				// trivial
				case IrKind.Copy:
				case IrKind.CreateTuple:
				case IrKind.LoadFunction:
				case IrKind.LoadArguments:
				case IrKind.Intrinsic:
				case IrKind.IntrinsicImpure:
				case IrKind.Call:
				case IrKind.Return:
					block.AddInstr(instr);
					break;
				default:
					throw new Exception($"Transform({instr}) not implemented yet");
			}
		}

		return block;
	}

	ReadOnlyCollection<(IrParam.Register source, IrParam.Register destination)> GetPhis(Context ctx, IrBlock node) {
		List<(IrParam.Register source, IrParam.Register destination)> phis = new();

		foreach (var child in node.Outgoing) {
			foreach (var instr in child.node.Instructions) {
				switch (instr.Kind) {
					case IrKind.Phi:
						for (int i = 1; i < instr.Params.Length; i += 2) {
							if ((instr[i] as IrParam.Block)!.Blk.ID == node.ID)
								phis.Add(((instr[i+1] as IrParam.Register)!, (instr[0] as IrParam.Register)!));
						}
						break;
					default:
						// not PHI, ignore
						break;
				}
			}
		}

		return phis.AsReadOnly();
	}

	public IrResult Transform(IrResult input) {
		var ctx = new Context();
		var functions = new Dictionary<string, IrBlock>();

		foreach (var function in input.Functions) {
			functions[function.Key] = Transform(ctx, function.Value);
		}

		return new(input.Platform, ctx.Blocks, functions.AsReadOnly(), input.UsedRegisters);
	}

	public class Context {
		Dictionary<int, IrBlock> blocks = new();
		public ReadOnlyCollection<IrBlock> Blocks => blocks.Values.ToList().AsReadOnly();

		public bool Replaced(IrBlock source) => blocks.ContainsKey(source.ID);
		public IrBlock Replace(IrBlock source) => Replaced(source) ? blocks[source.ID] : blocks[source.ID] = new(source.ID);
	}
}
