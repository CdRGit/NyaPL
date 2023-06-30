using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.IrTransformation;

public class Mem2Reg {
	private IrInstr? Transform(Context ctx, IrInstr instr) {
		switch (instr.Kind) {
			case IrInstr.IrKind.IntLiteral:
			case IrInstr.IrKind.LoadFunction:
			case IrInstr.IrKind.LoadIntrinsic:
			case IrInstr.IrKind.StoreParam:
			case IrInstr.IrKind.LoadArgument:
			case IrInstr.IrKind.Add:
			case IrInstr.IrKind.Multiply:
			case IrInstr.IrKind.NotEq:
			case IrInstr.IrKind.Call:
			case IrInstr.IrKind.CallImpure:
			case IrInstr.IrKind.Return:
				return instr;
			case IrInstr.IrKind.LoadLocal:
				if (ctx.HasLocal((instr[1] as IrParam.Local)!)) {
					var newInstr = new IrInstr(
						IrInstr.IrKind.Copy,
						instr[0],
						ctx.GetLocal((instr[1] as IrParam.Local)!)
					);
					return newInstr;
				}
				else {
					ctx.SetLocal((instr[1] as IrParam.Local)!, (instr[0] as IrParam.Register)!);
					return instr;
				}
			case IrInstr.IrKind.StoreLocal:
				ctx.SetLocal((instr[0] as IrParam.Local)!, (instr[1] as IrParam.Register)!);
				return null;
			case IrInstr.IrKind.BranchAlways: {
					var newInstr = new IrInstr(
						IrInstr.IrKind.BranchAlways,
						new IrParam.Block(ctx.Replace((instr[0] as IrParam.Block)!.Blk))
					);
					return newInstr;
				}
			case IrInstr.IrKind.BranchBool: {
					var newInstr = new IrInstr(
						IrInstr.IrKind.BranchBool,
						instr[0],
						new IrParam.Block(ctx.Replace((instr[1] as IrParam.Block)!.Blk)),
						new IrParam.Block(ctx.Replace((instr[2] as IrParam.Block)!.Blk))
					);
					return newInstr;
				}
			default:
				throw new Exception($"Transform(Context, IrInstr {instr}) not implemented yet");
		}
	}

	private IrBlock Transform(Context ctx, IrBlock block, IrBlock newBlock) {
		if (!newBlock.InstructionComplete) {
			foreach (var instr in block.Instructions) {
				var newInstr = Transform(ctx, instr);
				if (newInstr.HasValue)
					newBlock.AddInstr(newInstr.Value);
			}
		}

		newBlock.MarkInstructionComplete();
		ctx.ClearLocals();

		foreach(var child in block.Outgoing) {
			if (!newBlock.Outgoing.Any(b => b.node.ID == child.node.ID && b.label == child.label)) {
				IrBlock newChild = ctx.Replace(child.node);
				newBlock.AddConnection(newChild, child.label);
				Transform(ctx, child.node, newChild);
			}
		}

		return newBlock;
	}

	public IrResult Transform(IrResult lastPass) {
		var ctx = new Context(lastPass.UsedRegisters);

		var functions = new Dictionary<string, IrBlock>();

		// transform all the functions
		foreach (var function in lastPass.Functions) {
			ctx.ClearLocals();
			var newBlock = ctx.Replace(function.Value);
			functions[function.Key] = newBlock;
			Transform(ctx, function.Value, newBlock);
		}

		return new(lastPass.Platform, ctx.GetBlocks(), functions.AsReadOnly(), ctx.UsedRegisters);
	}

	private class Context {
		public Context(uint usedRegisters) {
			UsedRegisters = usedRegisters;
		}

		private Dictionary<string, IrParam.Register> locals = new();

		public void ClearLocals() => locals = new();
		public bool HasLocal(IrParam.Local local) => locals.ContainsKey(local.Name);
		public void SetLocal(IrParam.Local local, IrParam.Register register) {
			locals[local.Name] = register;
		}
		public IrParam.Register GetLocal(IrParam.Local local) {
			return locals[local.Name];
		}

		private Dictionary<int, IrBlock> replacements = new();

		public IrBlock Replace(IrBlock previous) {
			if (replacements.ContainsKey(previous.ID)) return replacements[previous.ID];
			IrBlock newBlock = new(previous.ID);
			replacements[previous.ID] = newBlock;
			return newBlock;
		}
		public bool Replaced(IrBlock previous) {
			return replacements.ContainsKey(previous.ID);
		}

		public ReadOnlyCollection<IrBlock> GetBlocks() => replacements.Values.ToList().AsReadOnly();

		public uint UsedRegisters { get; private set; }
	}
}
