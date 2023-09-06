using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public static class RegisterAllocation {
	public static Context<C, N> Allocate<R, C, N>(IrBlock block, R registers) where R : IRegisterSet<C, N> where C : System.Enum where N : System.Enum {
		var ctx = new Context<C, N>();
		var exits = GetExits(block);
		foreach (var classification in registers.Classes()) {
			Console.WriteLine(classification);
			var graph = new InterferenceGraph<C>();
			graph.classification = classification;
			graph.vertices = new();
			graph.edges = new();
			foreach (var exit in exits) {
				Console.WriteLine(exit.ID);
				var live_vals = new HashSet<uint>();
				var visited = new HashSet<IrBlock>();
				var doubly_visited = new HashSet<IrBlock>();
				CollectInterferencesFromBlock<R,C,N>(exit, registers, live_vals, visited, doubly_visited, graph);
			}
			//throw new Exception("TODO!");
			Allocate(graph, registers, ctx);
		}
		//throw new Exception("TODO!");
		return ctx;
	}

	static void CollectInterferencesFromBlock<R,C,N>(
		IrBlock b, R registers,
		HashSet<uint> live_vals,
		HashSet<IrBlock> visited, HashSet<IrBlock> doubly_visited,
		InterferenceGraph<C> g) where R : IRegisterSet<C, N> where C : System.Enum where N : System.Enum
	{
		if (visited.Contains(b)) {
			if (doubly_visited.Contains(b)) {
				return;
			}
			else {
				doubly_visited.Add(b);
				Console.WriteLine($"[{b.ID}] (revisit)");
			}
		} else {
			visited.Add(b);
			Console.WriteLine($"[{b.ID}]");
		}

		HashSet<uint> alive_next = new();
		HashSet<uint> dying_next = new();
		for (int i = b.Instructions.Count - 1; i >= 0; --i) {
			var instr = b.Instructions[i];
			Console.WriteLine($" {i}: {instr}");
			foreach (var reg in live_vals) {
				Console.WriteLine($"  {reg} is live");
			}
			foreach (var parameter in instr.Params) {
				if (parameter is IrParam.Register r) {
					if (!live_vals.Contains(r.Index)) {
						alive_next.Add(r.Index);
						if (!g.vertices.ContainsKey(r.Index)) {
							var vertex = new InterferenceGraph<C>.Vertex();
							vertex.register = r;
							vertex.degree = 0;
							vertex.adjacent = new();
							g.vertices[r.Index] = vertex;
						}
						Console.WriteLine($"++{r.Index} is live");
					}
					g.vertices[r.Index].adjacent.UnionWith(live_vals.Select(v => g.vertices[v]));
					foreach (var reg in live_vals) {
						g.edges.Add((reg, r.Index));
						g.vertices[reg].adjacent.Add(g.vertices[r.Index]);
					}
				}
			}
			if (instr.Kind != IrKind.Return && instr.Kind != IrKind.BranchBool) {
				if (instr[0] is IrParam.Register r) {
					dying_next.Add(r.Index);
					Console.WriteLine($"--{r.Index} is live");
				}
			}
			live_vals.UnionWith(alive_next);
			live_vals.ExceptWith(dying_next);
			alive_next.Clear();
			dying_next.Clear();
		}

		foreach (var parent in b.Incoming) {
			var live_vals_copy = new HashSet<uint>();
			live_vals_copy.UnionWith(live_vals);
			CollectInterferencesFromBlock<R,C,N>(parent, registers, live_vals_copy, visited, doubly_visited, g);
		}
	}

	static ReadOnlyCollection<IrBlock> GetExits(IrBlock block) {
		Stack<IrBlock> next = new();
		HashSet<IrBlock> visited = new();
		List<IrBlock> exits = new();
		next.Push(block);
		while (next.Count != 0) {
			IrBlock current = next.Pop();
			if (visited.Contains(current)) continue;
			visited.Add(current);
			if (current.HasReturn) {
				exits.Add(current);
			}
			foreach (var connection in current.Outgoing) {
				next.Push(connection.node);
			}
		}
		return exits.AsReadOnly();
	}
/*
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
			var graph = new InterferenceGraph<C>();
			graph.classification = range.Key;
			var graphVertices = new List<InterferenceGraph<C>.Vertex>();
			for (int i = 0; i < vertices.Length; ++i) {
				var r = vertices[i];
				graphVertices.Add(new() {register = r.Item2, degree = degrees[i]});
			}
			for (int i = 0; i < vertices.Length; ++i) {
				var current = graphVertices[i];
				current.adjacent = adjacent[i].Select(j => graphVertices[j]).ToArray();
				graphVertices[i] = current;
			}
			graph.vertices = graphVertices.ToArray();
			graph.edges = edges.ToArray();
			Allocate<R, C, N>(graph, registers, ctx);
		}
	}*/

	static void Allocate<R, C, N>(InterferenceGraph<C> graph, R registers, Context<C, N> ctx) where R : IRegisterSet<C, N> where C : System.Enum where N : System.Enum {
		var usable = registers.GetOrderedRegisters(graph.classification);
		var availableRegisters = usable.Length;
		Stack<InterferenceGraph<C>.Vertex> stack = new();
		while (graph.vertices.Values.Any(v => v.degree < availableRegisters && v.degree > -1)) {
			var currentIdx = 0u;
			while (graph.vertices[currentIdx].degree >= availableRegisters || graph.vertices[currentIdx].degree <= -1) ++currentIdx;
			graph.vertices[currentIdx].adjacent.Select(v => --v.degree).ToArray();
			var cur = graph.vertices[currentIdx];
			cur.degree = -1;
			graph.vertices[currentIdx] = cur;
			stack.Push(graph.vertices[currentIdx]);
		}
		if (graph.vertices.Values.Any(v => v.degree >= availableRegisters)) throw new Exception("Stack spilling not implemented yet!");

		var names = new Dictionary<IrParam.Register, N>();
		var allocated = new HashSet<IrParam.Register>();
		while (stack.Count != 0) {
			var current = stack.Pop();
			var available = usable.Except(current.adjacent.Select(v => v.register).Where(allocated.Contains).Select(i => names[i])).ToArray();
			if (available.Length == 0) throw new Exception("Stack spilling not implemented yet!");
			allocated.Add(current.register);
			names[current.register] = available[0];
		}
		foreach (var i in graph.vertices.Keys) {
			var r = graph.vertices[i].register;
			ctx.SetName(r, (graph.classification, names[r]));
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

	struct InterferenceGraph<C> where C : System.Enum {
		public C classification;
		public Dictionary<uint, Vertex> vertices;
		public HashSet<(uint, uint)> edges;

		public struct Vertex {
			public IrParam.Register register;
			public int degree;
			public HashSet<Vertex> adjacent;
		}
	}

	public class Context<C, N> where N : System.Enum where C : System.Enum {
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

	Classification[] Classes();

	bool NeedsAllocation(Classification c);
}
