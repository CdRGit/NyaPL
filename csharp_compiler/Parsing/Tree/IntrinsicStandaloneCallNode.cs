using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class IntrinsicStandaloneCallNode : StatementNode, ITypedNode {
	public IntrinsicNode BaseExpr { get; }
	public AstListNode<ExpressionNode> Arguments { get; }
	public Typ? Type { get; set; }

	public IntrinsicStandaloneCallNode(SourceLoc location, IntrinsicNode baseExpr, AstListNode<ExpressionNode> arguments, Typ? type = null) {
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

	public override string ToString() => $"IntrinsicStandaloneCall{(Type == null ? "" : $" :{Type}")}";
}
