using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class CallNode : ExpressionNode {
	public ExpressionNode BaseExpr { get; }
	public AstListNode<ExpressionNode> Arguments { get; }

	public CallNode(SourceLoc location, ExpressionNode baseExpr, AstListNode<ExpressionNode> arguments, Typ? type = null) {
		Location = location;
		BaseExpr = baseExpr;
		Arguments = arguments;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		children.Add(BaseExpr);
		children.Add(Arguments);
		return children.AsReadOnly();
	}

	public override string ToString() => $"Call{(Type == null ? "" : $" :{Type}")}";
}
