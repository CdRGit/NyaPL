using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class FunctionNode : StatementNode {
	public string Name { get; }
	public AstListNode<SideEffectNode> SideEffects { get; }
	public AstListNode<ParameterNode>  Parameters { get; }
	public TypeNode                    ReturnType { get; }
	public AstListNode<StatementNode>  Body { get; }

	public FunctionNode(SourceLoc location, string name, AstListNode<SideEffectNode> sideEffects, AstListNode<ParameterNode> parameters, TypeNode returnType, AstListNode<StatementNode> body) {
		Location = location;
		Name = name;
		SideEffects = sideEffects;
		Parameters = parameters;
		ReturnType = returnType;
		Body = body;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();
		children.Add(SideEffects);
		children.Add(Parameters);
		children.Add(ReturnType);
		children.Add(Body);
		return children.AsReadOnly();
	}

	public override string ToString() => $"Function {Name}";
}
