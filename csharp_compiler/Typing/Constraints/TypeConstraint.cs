using System;
using System.Linq;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

using Nyapl.Parsing.Tree;

namespace Nyapl.Typing.Constraints;

public abstract class TypeConstraint {
	public SourceLoc Location { get; protected init; }

	protected HashSet<int> metasUsed = new();
	public bool UsesMeta(int id) => metasUsed.Contains(id);

	protected void AddMetasForType(TypeChecker.Context ctx, Typ t) {
		switch (ctx.Force(t)) {
			case Meta m:
				metasUsed.Add(m.ID);
				break;
			case Intrinsic:
			case Trivial:
			case Function:
				break;
			case Apply a:
				AddMetasForType(ctx, a.BaseType);
				foreach (var p in a.ParameterTypes) AddMetasForType(ctx, p);
				break;
			default: throw new Exception($"AddMetasForType() not implemented yet for {t.GetType().Name}");
		}
	}

	public class Equal : TypeConstraint {
		public Typ Left { get; }
		public Typ Right { get; }

		public Equal(SourceLoc location, Typ left, Typ right, TypeChecker.Context ctx) {
			Location = location;
			Left = left;
			Right = right;
			AddMetasForType(ctx, left);
			AddMetasForType(ctx, right);
		}
	}

	public class BinOp : TypeConstraint {
		public Typ Left { get; }
		public Typ Right { get; }
		public Typ Result { get; }
		public BinOpKind OP { get; }

		public BinOp(SourceLoc location, Typ left, Typ right, Typ result, BinOpKind op, TypeChecker.Context ctx) {
			Location = location;
			Left = left;
			Right = right;
			Result = result;
			OP = op;
			AddMetasForType(ctx, left);
			AddMetasForType(ctx, right);
			AddMetasForType(ctx, result);
		}
	}
}
