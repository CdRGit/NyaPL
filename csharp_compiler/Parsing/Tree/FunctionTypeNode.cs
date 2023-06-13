using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

namespace Nyapl.Parsing.Tree;

public class FunctionTypeNode : TypeNode {
	public AstListNode<SideEffectNode> Effects { get; }
	public AstListNode<TypeNode> Parameters { get; }
	public TypeNode ReturnType { get; }

	public FunctionTypeNode(SourceLoc location, AstListNode<SideEffectNode> effects, AstListNode<TypeNode> parameters, TypeNode returnType, Typ? type = null) {
		Location = location;
		Effects = effects;
		Parameters = parameters;
		ReturnType = returnType;
		Type = type;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() {
		List<AstNode> children = new();

		children.Add(Effects);
		children.Add(Parameters);
		children.Add(ReturnType);

		return children.AsReadOnly();
	}

	public override string ToString() => $"FunctionType{(Type == null ? "" : $" :{Type}")}";
}
