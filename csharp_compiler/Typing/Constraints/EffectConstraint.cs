using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Effects;

namespace Nyapl.Typing.Constraints;

public abstract class EffectConstraint {
	public SourceLoc Location { get; protected init; }

	protected HashSet<int> metasUsed = new();
	public bool UsesMeta(int id) => metasUsed.Contains(id);

	public class AtMost : EffectConstraint {
		public Effect Effect { get; }
		public ReadOnlyCollection<Effect> Allowed { get; }

		public AtMost(SourceLoc location, Effect effect, ReadOnlyCollection<Effect> allowed) {
			Location = location;
			Effect = effect;
			{
				if (effect is MetaEffect m) metasUsed.Add(m.ID);
			}
			Allowed = allowed;
			foreach (var e in allowed) {
				if (e is MetaEffect m) metasUsed.Add(m.ID);
			}
		}

		public override string ToString() => $"AtMost({Effect}, [{string.Join(" ", Allowed)}])";
	}

	public class Equivalent : EffectConstraint {
		public ReadOnlyCollection<Effect> Left { get; }
		public ReadOnlyCollection<Effect> Right { get; }

		public Equivalent(SourceLoc location, ReadOnlyCollection<Effect> left, ReadOnlyCollection<Effect> right) {
			Location = location;
			Left = left;
			Right = right;
			foreach (var e in left) {
				if (e is MetaEffect m) metasUsed.Add(m.ID);
			}
			foreach (var e in right) {
				if (e is MetaEffect m) metasUsed.Add(m.ID);
			}
		}

		public override string ToString() => $"Equivalent([{string.Join(" ", Left)}], [{string.Join(" ", Right)}])";
	}
}
