using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class HoleExpressionNode : ExpressionNode {
	public HoleExpressionNode(SourceLoc location, Typ? type = null) {
		Location = location;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		return children.AsReadOnly();
	}

	public override string ToString() => $"Hole{(Type == null ? "" : $" :{Type}")}";
}
