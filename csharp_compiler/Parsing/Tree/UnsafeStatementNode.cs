using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class UnsafeStatementNode : StatementNode {
	public AstListNode<SideEffectNode> Effects { get; }
	public AstListNode<StatementNode> Body { get; }

	public UnsafeStatementNode(SourceLoc location, AstListNode<SideEffectNode> effects, AstListNode<StatementNode> body) {
		Location = location;
		Effects = effects;
		Body = body;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Effects);
		children.Add(Body);

		return children.AsReadOnly();
	}

	public override string ToString() => "Unsafe";
}
