using System.Collections.ObjectModel;

using Nyapl.Typing.Effects;

namespace Nyapl.Typing.Types;

public class Function : Typ {
	public ReadOnlyCollection<Effect> Effects { get; }

	public Function(ReadOnlyCollection<Effect> effects) {
		Effects = effects;
	}

	public override string ToString() => $"Function[{string.Join("", Effects)}]";
}
