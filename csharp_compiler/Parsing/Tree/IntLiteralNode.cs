using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class IntLiteralNode : ExpressionNode {
	public ulong Value { get; }

	public IntLiteralNode(SourceLoc location, ulong value, Typ? type = null) {
		Location = location;
		Value = value;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		return children.AsReadOnly();
	}

	public override string ToString() => $"IntLiteral {Value}{(Type == null ? "" : $" :{Type}")}";
}
