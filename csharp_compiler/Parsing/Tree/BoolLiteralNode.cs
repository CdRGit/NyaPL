using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class BoolLiteralNode : ExpressionNode {
	public bool Value { get; }

	public BoolLiteralNode(SourceLoc location, bool value, Typ? type = null) {
		Location = location;
		Value = value;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		return children.AsReadOnly();
	}

	public override string ToString() => $"BoolLiteral {Value}{(Type == null ? "" : $" :{Type}")}";
}
