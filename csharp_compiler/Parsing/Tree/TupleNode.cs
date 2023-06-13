using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class TupleNode : ExpressionNode {
	public AstListNode<ExpressionNode> Values { get; }

	public TupleNode(SourceLoc location, AstListNode<ExpressionNode> values, Typ? type = null) {
		Location = location;
		Values = values;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		children.Add(Values);
		return children.AsReadOnly();
	}

	public override string ToString() => $"Tuple{(Type == null ? "" : $" :{Type}")}";
}
