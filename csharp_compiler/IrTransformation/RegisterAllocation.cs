using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.IrTransformation;

public class RegisterAllocation {
	bool After((int, int) first, (int, int) second) =>
		first.Item1 > second.Item1 || (first.Item1 == second.Item1 && first.Item2 > second.Item2);

	IrBlock Transform(Context ctx, IrBlock source, ReadOnlyDictionary<IrParam.Register, ((int, int) createLoc, (int, int) lastUsage)> lifetimes) {
		if (ctx.Completed(source)) return ctx.Replace(source);
		var block = ctx.Replace(source);


		for (int i = 0; i < source.Instructions.Count; i++) {
			var now = (source.ID, i);
			var instr = source.Instructions[i];
			switch (instr.Kind) {
				case IrKind.CreateTuple:
				case IrKind.Call:
				case IrKind.CallImpure:
				case IrKind.Intrinsic:
				case IrKind.IntrinsicImpure:
					var ps = new List<IrParam>();
					var arguments = instr.Params.Skip(1).Select(p => (p is IrParam.Register r) ? ctx.Find(r) : p).ToArray();

					foreach (var lifetime in lifetimes) {
						if (lifetime.Value.lastUsage == now && lifetime.Value.createLoc != now) ctx.Release(lifetime.Key);
					}

					ps.Add(ctx.Claim((instr[0] as IrParam.Register)!));
					ps.AddRange(arguments);
					block.AddInstr(new(instr.Kind, ps.ToArray()));

					foreach (var lifetime in lifetimes) {
						if (lifetime.Value.lastUsage == now && lifetime.Value.createLoc == now) ctx.Release(lifetime.Key);
					}
					break;
				case IrKind.Copy:
					var src = instr[1] is IrParam.Register r ? ctx.Find(r) : instr[1];
					foreach (var lifetime in lifetimes) {
						if (lifetime.Value.lastUsage == now && lifetime.Value.createLoc != now) ctx.Release(lifetime.Key);
					}
					var dst = ctx.Claim((instr[0] as IrParam.Register)!);
					foreach (var lifetime in lifetimes) {
						if (lifetime.Value.lastUsage == now && lifetime.Value.createLoc == now) ctx.Release(lifetime.Key);
					}
					if (src != dst)
						block.AddInstr(new(IrKind.Copy, dst, src));
					break;
				case IrKind.Return:
					block.AddInstr(new(IrKind.Return, ctx.Find((instr[0] as IrParam.Register)!)));
					foreach (var lifetime in lifetimes) {
						if (lifetime.Value.lastUsage == now && lifetime.Value.createLoc != now) ctx.Release(lifetime.Key);
					}
					break;
				case IrKind.LoadArguments:
					block.AddInstr(new(IrKind.LoadArguments, instr.Params.Select(p => (p is IrParam.Register r) ? ctx.Claim(r) : p).ToArray()));
					break;
				case IrKind.LoadFunction:
					block.AddInstr(new(IrKind.LoadFunction, ctx.Claim((instr[0] as IrParam.Register)!), instr[1]));
					break;
				case IrKind.BranchAlways:
					block.AddInstr(new(IrKind.BranchAlways, new IrParam.Block(ctx.Replace((instr[0] as IrParam.Block)!.Blk))));
					break;
				case IrKind.BranchBool:
					block.AddInstr(new(IrKind.BranchBool,
						ctx.Find((instr[0] as IrParam.Register)!),
						new IrParam.Block(ctx.Replace((instr[1] as IrParam.Block)!.Blk)),
						new IrParam.Block(ctx.Replace((instr[2] as IrParam.Block)!.Blk))
					));
					foreach (var lifetime in lifetimes) {
						if (lifetime.Value.lastUsage == now && lifetime.Value.createLoc != now) ctx.Release(lifetime.Key);
					}
					break;
				default:
					throw new Exception($"Transform({instr}) not implemented yet");
			}
		}

		ctx.Complete(source);

		foreach (var connection in source.Outgoing) {
			block.AddConnection(Transform(ctx, connection.node, lifetimes), connection.label);
		}

		return block;
	}

	ReadOnlyDictionary<IrParam.Register, ((int, int) createLoc, (int, int) lastUsage)> LifetimeByRegister(ReadOnlyCollection<LifetimeInfo> lifetimes) {
		Dictionary<IrParam.Register, (int, int)> created = new();
		Dictionary<IrParam.Register, (int, int)> used = new();

		foreach (var lifetime in lifetimes) {
			switch (lifetime.type) {
				case LifetimeInfo.Type.CREATION:
					if (!created.ContainsKey(lifetime.register) || !After(lifetime.location, created[lifetime.register])) {
						created[lifetime.register] = lifetime.location;
					}
					break;
				case LifetimeInfo.Type.USAGE:
					if (!used.ContainsKey(lifetime.register) || After(lifetime.location, used[lifetime.register])) {
						used[lifetime.register] = lifetime.location;
					}
					break;
			}
		}

		Dictionary<IrParam.Register, ((int, int), (int, int))> byRegister = new();
		foreach (var create in created) {
			byRegister[create.Key] = (create.Value, used.ContainsKey(create.Key) ? used[create.Key] : create.Value);
		}

		return byRegister.AsReadOnly();
	}

	ReadOnlyCollection<LifetimeInfo> FindLifetimes(Context ctx, IrBlock node, HashSet<int> visited) {
		List<LifetimeInfo> lifetimes = new();
		if (visited.Contains(node.ID)) return lifetimes.AsReadOnly();
		visited.Add(node.ID);

		for (int i = 0; i < node.Instructions.Count; i++) {
			var instr = node.Instructions[i];
			switch (instr.Kind) {
				case IrKind.CreateTuple:
				case IrKind.LoadFunction:
				case IrKind.Copy:
				case IrKind.Call:
				case IrKind.CallImpure:
				case IrKind.Intrinsic:
				case IrKind.IntrinsicImpure:
					lifetimes.Add(new((instr[0] as IrParam.Register)!, LifetimeInfo.Type.CREATION, (node.ID, i)));
					foreach (var p in instr.Params.Skip(1))
						if (p is IrParam.Register r)
							lifetimes.Add(new(r, LifetimeInfo.Type.USAGE, (node.ID, i)));
					break;
				case IrKind.LoadArguments:
					lifetimes.AddRange(instr.Params.Select(p => new LifetimeInfo((p as IrParam.Register)!, LifetimeInfo.Type.CREATION, (node.ID, i))));
					break;
				case IrKind.Return:
				case IrKind.BranchBool:
					lifetimes.Add(new((instr[0] as IrParam.Register)!, LifetimeInfo.Type.USAGE, (node.ID, i)));
					break;
				case IrKind.BranchAlways:
					break;
				default:
					throw new Exception($"FindLifetimes({instr}) not implemented yet");
			}
		}

		foreach (var c in node.Outgoing) {
			lifetimes.AddRange(FindLifetimes(ctx, c.node, visited));
		}

		return lifetimes.AsReadOnly();
	}

	public IrResult Transform(IrResult input) {
		var functions = new Dictionary<string, IrBlock>();

		var ctx = new Context();
		foreach (var function in input.Functions) {
			ctx.ClearRegisters();
			var lifetimes = FindLifetimes(ctx, function.Value, new HashSet<int>());

			var registerLifetimes = LifetimeByRegister(lifetimes);
			functions[function.Key] = Transform(ctx, function.Value, registerLifetimes);
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

		List<IrParam.Register> availableRegisters = new();
		uint registerIdx = 0;
		Dictionary<uint, IrParam.Register> activeRegisters = new();
		public void ClearRegisters() {
			availableRegisters = new();
			registerIdx = 0;
		}

		public void Release(IrParam.Register req) {
			availableRegisters.Add(activeRegisters[req.Index]);
		}

		public IrParam.Register Claim(IrParam.Register req) {
			if (availableRegisters.Any(r => r.Size == req.Size)) {
				var reg = availableRegisters.OrderBy(r => r.Index).First(r => r.Size == req.Size);
				availableRegisters.Remove(reg);
				activeRegisters[req.Index] = reg;
				return reg;
			} else {
				return activeRegisters[req.Index] = new(req.Size, registerIdx++);
			}
		}

		public IrParam.Register Find(IrParam.Register req) {
			return activeRegisters[req.Index];
		}
	}

	struct LifetimeInfo {
		public LifetimeInfo(IrParam.Register r, Type t, (int, int) l) {
			register = r;
			type = t;
			location = l;
		}

		public IrParam.Register register;
		public Type type;
		public (int, int) location;
		public enum Type {
			USAGE,
			CREATION
		}

		public override string ToString() => $"{register,-15}: {type,-8} @ {location}";
	}
}
