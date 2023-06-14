using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class IfStatementNode : StatementNode {
	public ExpressionNode IfExpr { get; }
	public AstListNode<StatementNode> IfBody { get; }
	public ReadOnlyCollection<ElifStatementNode> Elifs { get; }
	public ElseStatementNode? Else { get; }

	public IfStatementNode(SourceLoc location, ExpressionNode ifExpr, AstListNode<StatementNode> ifBody, ReadOnlyCollection<ElifStatementNode> elifs, ElseStatementNode? @else) {
		Location = location;
		IfExpr = ifExpr;
		IfBody = ifBody;
		Elifs = elifs;
		Else = @else;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(IfExpr);
		children.Add(IfBody);
		children.AddRange(Elifs);
		if (Else != null)
			children.Add(Else);

		return children.AsReadOnly();
	}

	public override string ToString() => "If";
}
