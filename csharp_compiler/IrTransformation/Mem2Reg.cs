using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.IrTransformation;

public class Mem2Reg {
	private ReadOnlyDictionary<int, ReadOnlyCollection<IrBlock>> CalculateFrontiers(IrBlock entry, ReadOnlyCollection<IrBlock> nodes) {
		Dictionary<int, IrBlock> dominators = new();
		IrBlock Intersect(IrBlock b1, IrBlock b2) {
			IrBlock finger1 = b1;
			IrBlock finger2 = b2;
			while (finger1.ID != finger2.ID) {
				while (finger1.ID > finger2.ID) {
					finger1 = dominators[finger1.ID];
				}
				while (finger2.ID > finger1.ID) {
					finger2 = dominators[finger2.ID];
				}
			}
			return finger1;
		}

		dominators[entry.ID] = entry;
		bool changed = true;
		while (changed) {
			changed = false;
			foreach (var node in nodes) {
				if (node == entry) continue; // skip entry
				IrBlock new_idom = node.Incoming.Where(p => dominators.ContainsKey(p.ID)).First();
				foreach (var p in node.Incoming) {
					if (p == new_idom) continue; // skip the first one we already have
					if (dominators.ContainsKey(p.ID)) {
						new_idom = Intersect(p, new_idom);
					}
				}
				if (!dominators.ContainsKey(node.ID) || dominators[node.ID] != new_idom) {
					dominators[node.ID] = new_idom;
					changed = true;
				}
			}
		}

		var frontiers = new Dictionary<int, List<IrBlock>>();

		foreach (var b in nodes) {
			frontiers[b.ID] = new();
		}

		foreach (var b in nodes) {
			if (b.Incoming.Count >= 2) {
				foreach (var p in b.Incoming) {
					var runner = p;
					while (runner != dominators[b.ID]) {
						frontiers[runner.ID].Add(b);
						runner = dominators[runner.ID];
					}
				}
			}
		}

		foreach (var b in nodes) {
			b.Frontier = frontiers[b.ID].AsReadOnly();
		}

		return frontiers.ToDictionary(p => p.Key, p => p.Value.AsReadOnly()).AsReadOnly();
	}

	private ReadOnlyDictionary<string, (ushort, ReadOnlyCollection<(IrBlock, IrParam.Register)>)> CalculateVariables(ReadOnlyCollection<IrBlock> nodes) {
		var variables = new Dictionary<string, (ushort, List<(IrBlock, IrParam.Register)>)>();

		foreach (var b in nodes) {
			foreach (var instr in b.Instructions) {
				if (instr.Kind == IrInstr.IrKind.StoreLocal) {
					var name = (instr[0] as IrParam.Local)!.Name;

					if (!variables.ContainsKey(name))
						variables[name] = ((instr[0] as IrParam.Local)!.Size, new());

					var idx = variables[name].Item2.IndexOf(variables[name].Item2.FirstOrDefault(blk => blk.Item1 == b));

					if (idx == -1)
						variables[name].Item2.Add((b, (instr[1] as IrParam.Register)!));
					else
						variables[name].Item2[idx] = (b, (instr[1] as IrParam.Register)!);
				}
			}
		}

		return variables.ToDictionary(p => p.Key, p => (p.Value.Item1, p.Value.Item2.DistinctBy(b => b.Item1).ToList().AsReadOnly())).AsReadOnly();
	}

	private ReadOnlyCollection<IrBlock> CalculateRequiredPhiFunctions(
		ReadOnlyCollection<IrBlock> nodes,
		ReadOnlyDictionary<int, ReadOnlyCollection<IrBlock>> frontiers,
		ReadOnlyDictionary<string, (ushort, ReadOnlyCollection<(IrBlock, IrParam.Register)>)> variableDeclarations,
		string variable
	) {
		// calculate DF+
		var iteratedDominanceFrontier = new HashSet<IrBlock>();
		var prevCount = iteratedDominanceFrontier.Count;

		do {
			prevCount = iteratedDominanceFrontier.Count;
			foreach (var decl in variableDeclarations[variable].Item2) {
				iteratedDominanceFrontier.UnionWith(frontiers[decl.Item1.ID]);
			}
			foreach (var b in iteratedDominanceFrontier.ToList()) {
				iteratedDominanceFrontier.UnionWith(frontiers[b.ID]);
			}
		}
		while (prevCount != iteratedDominanceFrontier.Count);
		Console.WriteLine($"DF+[{variable}]");
		foreach (var b in iteratedDominanceFrontier) {
			Console.WriteLine($" {b.ID}");
		}

		return iteratedDominanceFrontier.ToList().AsReadOnly();
	}

	private ReadOnlyDictionary<string, (ushort, ReadOnlyCollection<(IrBlock, IrParam.Register)>)> PropagateVariables(
		ReadOnlyCollection<IrBlock> nodes,
		ReadOnlyDictionary<string, (ushort, ReadOnlyCollection<(IrBlock, IrParam.Register)>)> variableDeclarations,
		ReadOnlyCollection<(string Key, ReadOnlyCollection<(IrBlock b, IrParam.Register reg)> Value)> phiFunctions,
		IrBlock entry
	) {
		var propagated = new Dictionary<string, (ushort, List<(IrBlock, IrParam.Register)>)>();

		foreach (var p in variableDeclarations)
			propagated.Add(p.Key, (p.Value.Item1, p.Value.Item2.ToList()));

		foreach (var phi in phiFunctions) {
			Console.WriteLine(phi);
			foreach (var p in phi.Value) {
				if (!propagated[phi.Key].Item2.Any(pair => pair.Item1.ID == p.b.ID))
					propagated[phi.Key].Item2.Add(p);
			}
		}

		// propagated is filled with all the defined values now, time to propagate
		HashSet<IrBlock> done = new();
		Queue<IrBlock> next = new();
		next.Enqueue(entry);
		while (next.Count != 0) {
			var current = next.Dequeue();
			if (done.Contains(current)) continue; // skip repeats
			done.Add(current);

			foreach (var downstream in current.Outgoing.Select(p => p.node)) {
				foreach (var variable in propagated.Keys) {
					// propagate this variable from here
					if (propagated[variable].Item2.Any(p => p.Item1.ID == downstream.ID)) continue; // variable already exists
					if (!propagated[variable].Item2.Any(p => p.Item1.ID == current.ID)) continue;   // we do not have this variable, out of scope
					propagated[variable].Item2.Add((downstream, propagated[variable].Item2.First(p => p.Item1.ID == current.ID).Item2));
				}
				next.Enqueue(downstream);
			}
		}

		return propagated.ToDictionary(p => p.Key, p => (p.Value.Item1, p.Value.Item2.DistinctBy(b => b.Item1).ToList().AsReadOnly())).AsReadOnly();
	}

	private IrBlock Transform(
		Context ctx,
		IrBlock node,
		ReadOnlyDictionary<string, (ushort, ReadOnlyCollection<(IrBlock, IrParam.Register)>)> propagatedVariables,
		ReadOnlyCollection<(string Key, ReadOnlyCollection<(IrBlock b, IrParam.Register reg)> Value)> phiFunctions
	) {
		if (ctx.Transformed(node)) return ctx.Replace(node); // early return for already processed nodes

		IrBlock replacement = ctx.Replace(node);

		// add phi functions
		foreach (var phi in phiFunctions) {
			foreach (var pair in phi.Value) {
				if (pair.b.ID == node.ID) {
					// relevant for us
					// the register was already chosen
					ctx.SetLocal(new (phi.Key, pair.reg.Size), pair.reg);
					List<IrParam> arguments = new();
					arguments.Add(pair.reg);
					foreach (var i in node.Incoming) {
						arguments.Add(new IrParam.Block(i));
						arguments.Add(propagatedVariables[phi.Key].Item2.First(p => p.Item1.ID == i.ID).Item2);
					}
					replacement.AddInstr(new(IrInstr.IrKind.Phi, arguments.ToArray()));
				}
			}
		}

		foreach (var instr in node.Instructions) {
			switch (instr.Kind) {
				case IrInstr.IrKind.IntLiteral:
				case IrInstr.IrKind.BoolLiteral:

				case IrInstr.IrKind.Add:
				case IrInstr.IrKind.Multiply:
				case IrInstr.IrKind.NotEq:
				case IrInstr.IrKind.Equal:

				case IrInstr.IrKind.LoadIntrinsic:
				case IrInstr.IrKind.LoadFunction:
				case IrInstr.IrKind.LoadArgument:
				case IrInstr.IrKind.LoadTupleSection:

				case IrInstr.IrKind.StoreParam:
				case IrInstr.IrKind.AppendTupleSection:

				case IrInstr.IrKind.CallImpure:
				case IrInstr.IrKind.Call:
				case IrInstr.IrKind.Return:
					replacement.AddInstr(instr);
					break;

				case IrInstr.IrKind.BranchBool:
					replacement.AddInstr(new(IrInstr.IrKind.BranchBool,
						instr[0],
						new IrParam.Block(ctx.Replace((instr[1] as IrParam.Block)!.Blk)),
						new IrParam.Block(ctx.Replace((instr[2] as IrParam.Block)!.Blk)))
					);
					break;

				case IrInstr.IrKind.BranchAlways:
					replacement.AddInstr(new(IrInstr.IrKind.BranchAlways,
						new IrParam.Block(ctx.Replace((instr[0] as IrParam.Block)!.Blk))
						));
					break;

				case IrInstr.IrKind.LoadLocal:
					var reg = ctx.GetLocal((instr[1] as IrParam.Local)!);
					replacement.AddInstr(new(IrInstr.IrKind.Copy, instr[0], reg));
					break;

				case IrInstr.IrKind.StoreLocal:
					ctx.SetLocal((instr[0] as IrParam.Local)!, (instr[1] as IrParam.Register)!);
					break;

				default:
					throw new Exception($"Transforming {instr} not supported yet");
			}
		}

		ctx.MarkTransformed(node);

		foreach (var connection in node.Outgoing) {
			replacement.AddConnection(Transform(ctx, connection.node, propagatedVariables, phiFunctions), connection.label);
		}


		return replacement;
	}

	public IrResult Transform(IrResult lastPass) {
		var ctx = new Context(lastPass.UsedRegisters);

		var functions = new Dictionary<string, IrBlock>();

		foreach (var fn in lastPass.Functions) {
			var nodes = new List<IrBlock>();
			var todo = new Queue<IrBlock>();
			nodes.Add(fn.Value);
			todo.Enqueue(fn.Value);
			while (todo.Count != 0) {
				var current = todo.Dequeue();
				foreach (var child in current.Outgoing.Select(p => p.node)) {
					if (!nodes.Contains(child)) {
						todo.Enqueue(child);
						nodes.Add(child);
					}
				}
			}
			var frontiers = CalculateFrontiers(fn.Value, nodes.AsReadOnly());
			var variableDeclarations = CalculateVariables(nodes.AsReadOnly());

			var phiFunctions = variableDeclarations
				.Where(v => v.Value.Item2.Count != 1)
				.Select(v => (v.Key, CalculateRequiredPhiFunctions(nodes.AsReadOnly(), frontiers, variableDeclarations, v.Key)
					.Select(b => (b, reg: ctx.GetNewRegister(v.Value.Item1))).ToList().AsReadOnly())
				)
				.ToList().AsReadOnly();
			foreach (var p in phiFunctions) {
				Console.WriteLine($"Φ({p.Item1}) [{string.Join(", ", p.Item2.Select(p => (p.b.ID, p.reg)))}]");
			}
			foreach (var v in variableDeclarations) {
				Console.WriteLine($"{v.Key} declared in [{string.Join(", ", v.Value.Item2.Select(n => (n.Item1.ID, n.Item2)))}]");
			}

			var propagatedVariables = PropagateVariables(nodes.AsReadOnly(), variableDeclarations, phiFunctions, fn.Value);
			foreach (var v in propagatedVariables) {
				Console.WriteLine($"{v.Key} propagated into [{string.Join(", ", v.Value.Item2.Select(n => (n.Item1.ID, n.Item2)))}]");
			}

			/*
			var locals = propagatedVariables.SelectMany(p => p.Value.Item2.Select(v => (v, p.Key))).GroupBy(
				s => s.Item1.Item1,
				s => (s.Item2, s.Item1.Item2),
				(k, v) => (k, v: v.ToDictionary(p => p.Item1, p => p.Item2).AsReadOnly())
			).ToList().AsReadOnly();
			foreach (var l in locals) {
				l.k.SetLocals(l.v);
			}
			*/

			functions[fn.Key] = Transform(ctx, fn.Value, propagatedVariables, phiFunctions);
		}

		return new(lastPass.Platform, lastPass.Blocks, functions.AsReadOnly(), /*.Select(p => (p.Key, ctx.Replace(p.Value))).ToDictionary(p => p.Item1, p => p.Item2).AsReadOnly(),*/ ctx.UsedRegisters);
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
		public ReadOnlyDictionary<string, IrParam.Register> GetLocals() => locals.AsReadOnly();

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

		private HashSet<int> transformed = new();
		public void MarkTransformed(IrBlock block) => transformed.Add(block.ID);
		public bool Transformed(IrBlock block) => transformed.Contains(block.ID);

		public void ClearBlocks() => replacements = new();

		public ReadOnlyCollection<IrBlock> GetBlocks() => replacements.Values.ToList().AsReadOnly();

		public uint UsedRegisters { get; private set; }
		private IrParam.Register? previousRegister;

		public IrParam.Register GetNewRegister(ushort size) => previousRegister = new IrParam.Register(size, UsedRegisters++);
		public IrParam.Register GetPreviousRegister() => previousRegister ?? throw new Exception("no previous register");
	}
}
