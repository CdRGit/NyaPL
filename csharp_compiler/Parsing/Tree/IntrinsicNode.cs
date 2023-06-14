using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class IntrinsicNode : ExpressionNode {
	public string Name { get; }

	public IntrinsicNode(SourceLoc location, string name, Typ? type = null) {
		Location = location;
		Name = name;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		return children.AsReadOnly();
	}

	public override string ToString() => $"Intrinsic {Name}{(Type == null ? "" : $" :{Type}")}";
}
