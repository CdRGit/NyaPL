using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.IrTransformation;

public class RegisterAllocation {
	bool After((int, int) first, (int, int) second) =>
		first.Item1 > second.Item1 || (first.Item1 == second.Item1 && first.Item2 > second.Item2);

	IrBlock Transform(Context ctx, IrBlock source, ReadOnlyDictionary<IrParam.Register, IrParam.Register> replacements) {
		if (ctx.Completed(source)) return ctx.Replace(source);
		var block = ctx.Replace(source);


		for (int i = 0; i < source.Instructions.Count; i++) {
			var instr = source.Instructions[i];
			block.AddInstr(new(instr.Kind, instr.Params.Select(p =>
				(p is IrParam.Register r) ? replacements[r]
				: (p is IrParam.Block b) ? new IrParam.Block(ctx.Replace(b.Blk))
				: p).ToArray()));
		}

		ctx.Complete(source);

		foreach (var connection in source.Outgoing) {
			block.AddConnection(Transform(ctx, connection.node, replacements), connection.label);
		}

		return block;
	}

	Lifetime FindLifetime((int node, int instr) initial, (int node, int instr) final, ReadOnlyCollection<IrBlock> blocks) {
		var blocksById = blocks.ToDictionary(b => b.ID, b => b);
		var visited = new HashSet<int>();

		var includesFinal = false;

		var todo = new Queue<int>();
		todo.Enqueue(final.Item1);
		while (todo.Count != 0) {
			var current = todo.Dequeue();
			if (visited.Contains(current)) {
				if (current == final.node) includesFinal = true; // revisiting final? add it to the survived nodes!
				continue; // skip over visited
			}
			if (current == initial.node) continue; // skip over the end goal

			visited.Add(current);

			foreach (var b in blocksById[current].Incoming)
				todo.Enqueue(b.ID);
		}

		if (!includesFinal)
			visited.Remove(final.node);

		return new(initial, final, visited);
	}

	ReadOnlyDictionary<IrParam.Register, Lifetime> LifetimeByRegister(ReadOnlyCollection<LifetimeEvent> lifetimeEvents, ReadOnlyCollection<IrBlock> blocks) {
		Dictionary<IrParam.Register, (int, int)> first = new();
		Dictionary<IrParam.Register, (int, int)> last = new();

		foreach (var lifetime in lifetimeEvents) {
			if (!first.ContainsKey(lifetime.register) || !After(lifetime.location, first[lifetime.register])) {
				first[lifetime.register] = lifetime.location;
			}
			if (!last.ContainsKey(lifetime.register) || After(lifetime.location, last[lifetime.register])) {
				last[lifetime.register] = lifetime.location;
			}
		}

		Dictionary<IrParam.Register, Lifetime> byRegister = new();
		foreach (var f in first) {
			byRegister[f.Key] = FindLifetime(f.Value, last[f.Key], blocks);
		}

		return byRegister.AsReadOnly();
	}

	ReadOnlyCollection<LifetimeEvent> FindLifetimeEvents(Context ctx, IrBlock node, HashSet<int> visited) {
		List<LifetimeEvent> lifetimeEvents = new();
		if (visited.Contains(node.ID)) return lifetimeEvents.AsReadOnly();
		visited.Add(node.ID);

		for (int i = 0; i < node.Instructions.Count; i++) {
			var instr = node.Instructions[i];
			foreach (var p in instr.Params) {
				if (p is IrParam.Register r) {
					lifetimeEvents.Add(new(r, (node.ID, i)));
				}
			}
		}

		foreach (var c in node.Outgoing) {
			lifetimeEvents.AddRange(FindLifetimeEvents(ctx, c.node, visited));
		}

		return lifetimeEvents.AsReadOnly();
	}

	ReadOnlyDictionary<IrParam.Register, IrParam.Register> ReplaceRegisters(ReadOnlyDictionary<IrParam.Register, Lifetime> lifetimes) {
		List<IrParam.Register> existingRegisters = new();
		Dictionary<IrParam.Register, List<Lifetime>> overlaps = new();
		Dictionary<IrParam.Register, IrParam.Register> replacements = new();
		uint regIndex = 0;

		IrParam.Register AddReg(ushort size) {
			var reg = new IrParam.Register(size, regIndex++);
			existingRegisters.Add(reg);
			overlaps[reg] = new();
			return reg;
		}

		IrParam.Register GetReg(IrParam.Register reg) {
			foreach (var existing in existingRegisters.Where(r => r.Size == reg.Size)) {
				var lifetime = lifetimes[reg];
				if (!overlaps[existing].Any(l => {
						var overlap = l.Overlaps(lifetime);
						Console.WriteLine($"Overlaps({lifetime}, {l}) = {overlap}");
						return overlap;
					})) {
					overlaps[existing].Add(lifetimes[reg]);
					return existing;
				}
			}
			var replacement = AddReg(reg.Size);
			overlaps[replacement].Add(lifetimes[reg]);
			return replacement;
		}

		AddReg(0);

		foreach (var reg in lifetimes.Keys) {
			var replacement = GetReg(reg);
			replacements[reg] = replacement;
		}

		return replacements.AsReadOnly();
	}

	public IrResult Transform(IrResult input) {
		var functions = new Dictionary<string, IrBlock>();

		var ctx = new Context();
		foreach (var function in input.Functions) {
			var lifetimeEvents = FindLifetimeEvents(ctx, function.Value, new HashSet<int>());

			var registerLifetimes = LifetimeByRegister(lifetimeEvents, input.Blocks);

			var registerReplacements = ReplaceRegisters(registerLifetimes);
			functions[function.Key] = Transform(ctx, function.Value, registerReplacements);
		}

		return new(input.Platform, ctx.Blocks, functions.AsReadOnly(), 0);
	}

	class Context {
		Dictionary<int, IrBlock> blocks = new();
		public ReadOnlyCollection<IrBlock> Blocks => blocks.Values.ToList().AsReadOnly();
		public bool Replaced(IrBlock source) => blocks.ContainsKey(source.ID);
		public IrBlock Replace(IrBlock source) => Replaced(source) ? blocks[source.ID] : blocks[source.ID] = new(source.ID);

		HashSet<int> completed = new();
		public bool Completed(IrBlock source) => completed.Contains(source.ID);
		public void Complete(IrBlock source) => completed.Add(source.ID);
	}

	readonly struct Lifetime {
		public Lifetime ((int, int) initial, (int, int) final, IEnumerable<int> aliveForAllOf) {
			this.initial = initial;
			this.final = final;
			this.aliveForAllOf = new HashSet<int>(aliveForAllOf);
		}

		public readonly (int node, int instr) initial;
		public readonly (int node, int instr) final;
		public readonly HashSet<int> aliveForAllOf;

		public override string ToString() => $"{initial} : {final}{(aliveForAllOf.Count != 0 ? $" : [{string.Join(", ", aliveForAllOf)}]" : "")}";

		public bool Overlaps(Lifetime other, bool tryOtherWayAround = true) {
			// check "aliveForAllOf"s
			if (aliveForAllOf.Contains(other.initial.node) || aliveForAllOf.Contains(other.final.node)) return true;

			// now comes the tricky part
			// is my "final" after or equal to their "initial"?
			if (final.node == other.initial.node) {
				if (final.instr >= other.initial.instr) return true; // yeag
			}
			// do our "final"s match? if so we overlap for sure
			if (final.node == other.final.node) {
				if (final.instr == other.final.instr) return true; // yeag
			}
			// do our "initial"s match? if so we overlap for sure
			if (initial.node == other.initial.node) {
				if (initial.instr == other.initial.instr) return true; // yeag
			}

			if (tryOtherWayAround) return other.Overlaps(this, false); // try the other way around if we must


			// done?
			return false;
			throw new Exception($"TODO");
		}
	}

	struct LifetimeEvent {
		public LifetimeEvent(IrParam.Register r, (int, int) l) {
			register = r;
			location = l;
		}

		public IrParam.Register register;
		public (int, int) location;
		public override string ToString() => $"{register,-15}: {location}";
	}
}
