using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public static class RegisterAllocation {
	public static IrBlock Allocate(IrBlock block) {
		throw new Exception("TODO!");
	}

	class Context {
		HashSet<int> visited = new();
		public bool Visited(IrBlock block) => visited.Contains(block.ID);
		public void Visit(IrBlock block) => visited.Add(block.ID);

		Dictionary<int, IrBlock> replacements = new();
		public IrBlock Replace(IrBlock block) => replacements.ContainsKey(block.ID) ? replacements[block.ID] : replacements[block.ID] = new(block.ID);
	}
}
