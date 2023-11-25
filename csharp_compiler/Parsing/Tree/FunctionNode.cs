using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class FunctionNode : StatementNode, ITypedNode {
	public string Name { get; }
	public AstListNode<SideEffectNode> SideEffects { get; }
	public AstListNode<ParameterNode>  Parameters { get; }
	public TypeNode                    ReturnType { get; }
	public AstListNode<StatementNode>  Body { get; }

	public Typ                         Type { get; set; }

	public FunctionNode(SourceLoc location, string name, AstListNode<SideEffectNode> sideEffects, AstListNode<ParameterNode> parameters, TypeNode returnType, AstListNode<StatementNode> body, Typ? type = null) {
		Location = location;
		Name = name;
		SideEffects = sideEffects;
		Parameters = parameters;
		ReturnType = returnType;
		Body = body;
		Type = type;
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
