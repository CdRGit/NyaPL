using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class ElseStatementNode : StatementNode {
	public AstListNode<StatementNode> Body { get; }

	public ElseStatementNode(SourceLoc location, AstListNode<StatementNode> body) {
		Location = location;
		Body = body;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Body);

		return children.AsReadOnly();
	}

	public override string ToString() => "Else";
}
