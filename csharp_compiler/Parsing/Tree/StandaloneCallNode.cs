using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class StandaloneCallNode : StatementNode, ITypedNode {
	public LValueNode BaseExpr { get; }
	public AstListNode<ExpressionNode> Arguments { get; }
	public Typ? Type { get; set; }

	public StandaloneCallNode(SourceLoc location, LValueNode baseExpr, AstListNode<ExpressionNode> arguments, Typ? type = null) {
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

	public override string ToString() => $"StandaloneCall{(Type == null ? "" : $" :{Type}")}";
}
