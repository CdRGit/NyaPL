using System.Linq;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;

namespace Nyapl.Parsing.Tree;

public class AstListNode<T> : AstNode, IEnumerable<T> where T : AstNode {
	public ReadOnlyCollection<T> Children { get; }

	public AstListNode(SourceLoc location, ReadOnlyCollection<T> children) {
		Location = location;
		Children = children;
	}

	public override ReadOnlyCollection<AstNode> GetChildren() => Children.Cast<AstNode>().ToList().AsReadOnly();

	public override string ToString() => $"List<{typeof(T).Name}>";

	public IEnumerator<T> GetEnumerator() => Children.GetEnumerator();

	IEnumerator IEnumerable.GetEnumerator() => Children.GetEnumerator();
}
