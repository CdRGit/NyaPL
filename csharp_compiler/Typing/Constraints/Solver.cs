using System;
using System.Linq;
using System.Collections.Generic;

using Nyapl.Typing.Types;
using Nyapl.Typing.Effects;

namespace Nyapl.Typing.Constraints;

public class Solver {
	private EffectComparer effectComparer = new();
	private TypeComparer typeComparer = new();

	private List<EffectConstraint> revisitE = new();
	private List<TypeConstraint> revisitT = new();

	private void Solve(TypeChecker.Context ctx, MetaEffect m, IEnumerable<Effect> effects) {
		ctx.SetMetaEffect(m.ID, effects.ToList().AsReadOnly());
		// scan through the revisit list and solve any in there
		List<EffectConstraint> needUpdating = revisitE.Where(c => c.UsesMeta(m.ID)).ToList();
		revisitE = revisitE.Except(needUpdating).ToList();
		foreach (var constraint in needUpdating) Solve(ctx, constraint);
	}

	private void Solve(TypeChecker.Context ctx, EffectConstraint.Equivalent eq) {
		var left = eq.Left.SelectMany(ctx.GetFlatEffect);
		var right = eq.Right.SelectMany(ctx.GetFlatEffect);

		var l = left.Except(right, effectComparer).Distinct(effectComparer);
		var r = right.Except(left, effectComparer).Distinct(effectComparer);

		var lC = l.Count();
		var rC = r.Count();

		if (lC == 0 && rC == 0) return; // success

		if (lC == 0) {
			// does r only have metas
			if (r.All(e => e is MetaEffect)) {
				// all these metas are zero
				foreach (var e in r) {
					if (e is MetaEffect m) {
						Solve(ctx, m, Enumerable.Empty<Effect>());
					} else {
						throw new Exception("Invalid State");
					}
				}
				return; // success
			} else {
				throw new TypeError(eq.Location, $"Effects [{string.Join(" ", left)}] and [{string.Join(" ", right)}] are not equivalent");
			}
		}
		if (rC == 0) {
			// does l only have metas
			if (l.All(e => e is MetaEffect)) {
				// all these metas are zero
				foreach (var e in l) {
					if (e is MetaEffect m) {
						Solve(ctx, m, Enumerable.Empty<Effect>());
					} else {
						throw new Exception("Invalid State");
					}
				}
				return; // success
			} else {
				throw new TypeError(eq.Location, $"Effects [{string.Join(" ", left)}] and [{string.Join(" ", right)}] are not equivalent");
			}
		}

		// sides still have values
		// do either have metas
		if (!r.Any(e => e is MetaEffect) && !l.Any(e => e is MetaEffect))
			throw new TypeError(eq.Location, $"Effects [{string.Join(" ", left)}] and [{string.Join(" ", right)}] are not equivalent");

		// is either side one meta variable
		if (lC == 1 && l.Any(e => e is MetaEffect)) {
			Solve(ctx, (l.First() as MetaEffect)!, r);
		}
		if (rC == 1 && r.Any(e => e is MetaEffect)) {
			Solve(ctx, (r.First() as MetaEffect)!, l);
		}

		// add a new constraint back to the list with only these values left
		revisitE.Add(new EffectConstraint.Equivalent(eq.Location, l.ToList().AsReadOnly(), r.ToList().AsReadOnly()));
	}

	public void Solve(TypeChecker.Context ctx, EffectConstraint constraint) {
		switch (constraint) {
			case EffectConstraint.AtMost atMost: {
				var effects = ctx.GetFlatEffect(atMost.Effect);
				// if we still have metas laying around
				if (effects.Any(e => e is MetaEffect)) {
					// add the constraint to the "check later" list
					revisitE.Add(constraint);
					return;
				}
				// no more metas on the left, time to check
				if(effects.Except(atMost.Allowed, effectComparer).Any())
					throw new TypeError(atMost.Location, $"Effects [{string.Join(" ", effects)}] are not a subset of [{string.Join(" ", atMost.Allowed)}]");
				return;
			}
			case EffectConstraint.Equivalent equivalent: Solve(ctx, equivalent); return;
			default:
				throw new Exception($"Solving Constraint: {constraint.GetType().Name} not implemented yet.");
		}
	}

	public void NotifyMetaSolved(TypeChecker.Context ctx, int id) {
		// meta has been solved
		List<TypeConstraint> needUpdating = revisitT.Where(c => c.UsesMeta(id)).ToList();
		revisitT = revisitT.Except(needUpdating).ToList();
		foreach (var constraint in needUpdating) Solve(ctx, constraint);
	}

	public void Solve(TypeChecker.Context ctx, TypeConstraint constraint) {
		switch (constraint) {
			case TypeConstraint.Equal eq: {
				var newL = Resolve(ctx, eq.Left);
				var newR = Resolve(ctx, eq.Right);
				if (newL is Meta mL) {
					ctx.Solve(eq.Location, mL.ID, newR);
				}
				else if (newR is Meta mR) {
					ctx.Solve(eq.Location, mR.ID, newL);
				} else {
					if (!typeComparer.Equals(newL, newR)) throw new TypeError(eq.Location, $"{newL} != {newR}");
				}
				return; // success
			}
			case TypeConstraint.BinOp bin: {
				// check if all three types derive properly
				var newLeft   = Resolve(ctx, bin.Left);
				var newRight  = Resolve(ctx, bin.Right);
				// add new to revisit list to try again later
				if (HasMeta(ctx, newLeft) || HasMeta(ctx, newRight))
					revisitT.Add(new TypeConstraint.BinOp(bin.Location, newLeft, newRight, bin.Result, bin.OP, ctx));
				else {
					if (!ctx.GetBinOperators().Where((v) => {
						if (v.Item1 == bin.OP && typeComparer.Equals(v.Item2, newLeft) && typeComparer.Equals(v.Item3, newRight)) {
							ctx.Unify(bin.Location, v.Item4, bin.Result);
							return true;
						}
						return false;
					}).Any()) throw new TypeError(bin.Location, $"No operator `{bin.OP}` exists where `{newLeft}` {bin.OP} `{newRight}` = `{bin.Result}`");
					// make sure the constraint is possible
				}
				return;
			}
			default:
				throw new Exception($"Solving Constraint: {constraint.GetType().Name} not implemented yet.");
		}
	}

	private bool HasMeta(TypeChecker.Context ctx, Typ type) {
		if (type is Meta m) {
			return true;
		}
		if (type is Apply a) {
			return HasMeta(ctx, a.BaseType) || a.ParameterTypes.Select(s => HasMeta(ctx, s)).Any(b => b);
		}
		if (type is Function f) {
			return false;
		}
		if (type is Trivial tr) {
			return false;
		}

		throw new Exception($"HasMeta({type.GetType().Name}) not implemented yet");
	}

	private Typ Resolve(TypeChecker.Context ctx, Typ type) {
		if (type is Meta m) {
			var t = ctx.Force(m);
			if (t == type) return t;
			return Resolve(ctx, t);
		}
		if (type is Apply a) {
			return new Apply(Resolve(ctx, a.BaseType), a.ParameterTypes.Select(s => Resolve(ctx, s)).ToList().AsReadOnly());
		}
		if (type is Function f) {
			return f;
		}
		if (type is Trivial tr) {
			return tr;
		}

		throw new Exception($"Resolve({type.GetType().Name}) not implemented yet");
	}

	private class EffectComparer : IEqualityComparer<Effect> {
		public bool Equals(Effect? e1, Effect? e2) {
			if (e1 == null && e2 == null) {
				return true;
			}
			if (e1 == null || e2 == null) {
				return false;
			}
			if (e1 is NamedEffect n1 && e2 is NamedEffect n2) {
				return n1.Name == n2.Name;
			}
			if (e1 is MetaEffect m1 && e2 is MetaEffect m2) {
				return m1.ID == m2.ID;
			}
			return false;
		}

		public int GetHashCode(Effect e) {
			if (e is NamedEffect n) return n.Name.GetHashCode();
			if (e is MetaEffect m) return m.ID.GetHashCode();
			throw new Exception($"GetHashCode({e.GetType().Name}) not implemented yet");
		}
	}

	private class TypeComparer : IEqualityComparer<Typ> {
		public bool Equals(Typ? t1, Typ? t2) {
			if (t1 == null && t2 == null) {
				return true;
			}
			if (t1 == null || t2 == null) {
				return false;
			}
			if (t1 is Trivial n1 && t2 is Trivial n2) {
				return n1.Name == n2.Name;
			}
			if (t1 is Meta m1 && t2 is Meta m2) {
				return m1.ID == m2.ID;
			}
			return false;
		}

		public int GetHashCode(Typ t) {
			if (t is Trivial n) return n.Name.GetHashCode();
			if (t is Meta m) return m.ID.GetHashCode();
			throw new Exception($"GetHashCode({t.GetType().Name}) not implemented yet");
		}
	}
}
