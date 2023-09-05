using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public static class RegisterAllocation {
	public static Context<C, N> Allocate<R, C, N>(IrBlock block, R registers) where R : IRegisterSet<C, N> where C : System.Enum where N : System.Enum {
		var ctx = new Context<C, N>();
		Allocate<R, C, N>(block, registers, ctx);
		return ctx;
	}

	static void Allocate<R, C, N>(IrBlock block, R registers, Context<C, N> ctx) where R : IRegisterSet<C, N> where C : System.Enum where N : System.Enum {
		if (ctx.Visited(block)) return;
		ctx.Visit(block);
		// assume there's only one basic block
		// let's assert on this assumption
		if (block.Incoming.Count != 0 || block.Outgoing.Count != 0) throw new Exception("only handling lone basic blocks for now");

		// it's graphing time
		// or live range time
		var liveRanges = CalculateLiveRanges(block);
		// *now* it's graphing time
		// or, well, classifying time (a bit less fun but still pretty fun)
		var classifiedRanges = ClassifyRanges<R, C, N>(liveRanges, registers);
		// is it graphing time yet?
		// I think it's graphing time
		foreach (var range in classifiedRanges) {
			var availableRegisters = registers.GetRegisterCount(range.Key);
			Console.WriteLine($"{range.Key}: {availableRegisters}");

			var vertices = range.Value.ToArray();
			var edges = new List<(int, int)>();
			var degrees = new int[vertices.Length];
			var adjacent = new List<int>[vertices.Length];
			for (int i = 0; i < degrees.Length; ++i) {
				degrees[i] = 0;
				adjacent[i] = new();
			}
			for (int i = 0; i < vertices.Length; ++i) {
				for (int j = i + 1; j < vertices.Length; ++j) {
					var u = vertices[i];
					var v = vertices[j];
					if ((u.Item1.Final >= v.Item1.First) && (v.Item1.Final >= u.Item1.First)) {
						edges.Add((i, j));
						degrees[i]++;
						degrees[j]++;
						adjacent[i].Add(j);
						adjacent[j].Add(i);
					}
				}
			}
			// time for the actual algorithm
			Stack<int> stack = new();
			while (degrees.Any(d => d < availableRegisters && d > -1)) {
				var currentIdx = 0;
				while (degrees[currentIdx] >= availableRegisters || degrees[currentIdx] <= -1) ++currentIdx;
				foreach (var neighbour in adjacent[currentIdx]) {
					--degrees[neighbour];
				}
				degrees[currentIdx] = -1;
				stack.Push(currentIdx);
			}
			if (degrees.Any(d => d >= availableRegisters)) throw new Exception("Stack spilling not implemented yet!");

			var names = new N[degrees.Length];
			var usable = registers.GetOrderedRegisters(range.Key);
			var allocated = new HashSet<int>();
			while (stack.Count != 0) {
				var current = stack.Pop();
				var available = usable.Except(adjacent[current].Where(allocated.Contains).Select(i => names[i])).ToArray();
				if (available.Length == 0) throw new Exception("Stack spilling not implemented yet!");
				allocated.Add(current);
				names[current] = available[0];
			}
			for (int i = 0; i < names.Length; ++i) {
				ctx.SetName(vertices[i].Item2, (range.Key, names[i]));
			}
		}
	}

	static ReadOnlyDictionary<C, ReadOnlyCollection<(LiveRange, IrParam.Register)>> ClassifyRanges<R, C, N>(ReadOnlyDictionary<uint, (LiveRange, IrParam.Register)> liveRanges, R registers)
	where R : IRegisterSet<C, N> where C : System.Enum where N : System.Enum {
		Dictionary<C, List<(LiveRange, IrParam.Register)>> classified = new();

		foreach (var range in liveRanges) {
			var regClass = registers.Classify(range.Value.Item2);

			if (!registers.NeedsAllocation(regClass)) continue;

			if (!classified.ContainsKey(regClass))
				classified[regClass] = new();

			classified[regClass].Add(range.Value);
		}

		return classified.Select(p => (k: p.Key, v: p.Value.AsReadOnly())).ToDictionary(p => p.k, p => p.v).AsReadOnly();
	}

	static ReadOnlyDictionary<uint, (LiveRange, IrParam.Register)> CalculateLiveRanges(IrBlock block) {
		var ranges = new Dictionary<uint, (LiveRange, IrParam.Register)>();

		for (uint i = 0; i < block.Instructions.Count; i++) {
			var instr = block.Instructions[(int)i];
			foreach (var param in instr.Params) {
				if (param is IrParam.Register r) {
					if (!ranges.ContainsKey(r.Index))
						ranges[r.Index] = (new(i), r);
					var range = ranges[r.Index];
					range.Item1.Final = i;
					ranges[r.Index] = range;
				}
			}
		}

		return ranges.AsReadOnly();
	}

	struct LiveRange {
		public LiveRange(uint line) {
			first = final = line;
		}

		public uint First { get => first; set => first = value; }
		uint first;
		public uint Final { get => Math.Max(first, final - 1); set => final = value; }
		uint final;

		public override string ToString() => $"{{{First}, {Final}}}";
	}

	public class Context<C, N> where N : System.Enum where C : System.Enum{
		HashSet<int> visited = new();
		public bool Visited(IrBlock block) => visited.Contains(block.ID);
		public void Visit(IrBlock block) => visited.Add(block.ID);

		Dictionary<uint, (C, N)> names = new();
		public void SetName(IrParam.Register r, (C, N) name) => names[r.Index] = name;
		public (C, N)? GetName(IrParam.Register r) => names.ContainsKey(r.Index) ? names[r.Index] : null;
	}
}

public interface IRegisterSet<Classification, Names> where Classification : System.Enum where Names : System.Enum {
	Classification Classify(IrParam.Register r);
	int GetRegisterCount(Classification c);

	Names[] GetOrderedRegisters(Classification c);

	bool NeedsAllocation(Classification c);
}
