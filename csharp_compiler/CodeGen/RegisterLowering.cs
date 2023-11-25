using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public static class RegisterLowering {
	public static (ReadOnlyDictionary<string, IrBlock>, T) LowerRegisters<T>(ReadOnlyDictionary<string, IrBlock> functions, Func<IrInstr, T, Func<IrBlock, IrBlock>, IEnumerable<IrInstr>> transformation) where T : new() {
		T t = new();
		return (functions.Select(kv => {
			var lowered = LowerRegisters<T>(kv.Value, transformation, t, new());
			IrPrinter.DrawGraph(kv.Key, lowered, "lowered");
			return (kv.Key, lowered);
		}).ToDictionary(tup => tup.Item1, tup => tup.Item2).AsReadOnly(), t);
	}

	static IrBlock LowerRegisters<T>(IrBlock source, Func<IrInstr, T, Func<IrBlock, IrBlock>, IEnumerable<IrInstr>> transformation, T t, Context ctx) where T : new() {
		if (ctx.Visited(source)) return ctx.Replace(source);
		var block = ctx.Replace(source);
		ctx.Visit(source);
		// assume there's only one basic block
		// let's assert on this assumption
		// if (source.Incoming.Count != 0 || source.Outgoing.Count != 0) throw new Exception("only handling lone basic blocks for now");

		var instructions = source.Instructions.SelectMany(i => transformation(i, t, ctx.Replace));
		foreach (var instr in instructions) block.AddInstr(instr);

		foreach (var connection in source.Outgoing) {
			block.AddConnection(LowerRegisters(connection.node, transformation, t, ctx), connection.label);
		}

		return block;
	}

	class Context {
		HashSet<int> visited = new();
		public bool Visited(IrBlock block) => visited.Contains(block.ID);
		public void Visit(IrBlock block) => visited.Add(block.ID);

		Dictionary<int, IrBlock> replacements = new();
		public IrBlock Replace(IrBlock block) => replacements.ContainsKey(block.ID) ? replacements[block.ID] : replacements[block.ID] = new(block.ID);
	}
}
