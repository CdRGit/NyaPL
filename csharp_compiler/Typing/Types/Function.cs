using System.Collections.ObjectModel;

using Nyapl.Typing.Effects;

namespace Nyapl.Typing.Types;

public class Function : Typ {
	public ReadOnlyCollection<Effect> Effects { get; }
	public bool? IsIntrinsic { get; set; }

	public Function(ReadOnlyCollection<Effect> effects, bool? isIntrinsic) {
		Effects = effects;
		IsIntrinsic = isIntrinsic;
	}

	public override string ToString() => $"Function[{string.Join("", Effects)}]";
}
