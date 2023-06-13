using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class DeclareVarNode : StatementNode {
	public string Name { get; }
	public TypeNode Type { get; }
	public ExpressionNode Expression { get; }

	public DeclareVarNode(SourceLoc location, string name, TypeNode type, ExpressionNode expression) {
		Location = location;
		Name = name;
		Type = type;
		Expression = expression;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
	
		children.Add(Type);
		children.Add(Expression);

		return children.AsReadOnly();
	}

	public override string ToString() => $"DeclareVar {Name}";
}
