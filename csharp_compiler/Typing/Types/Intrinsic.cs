namespace Nyapl.Typing.Types;

public class Intrinsic : Typ {
	public IntrinsicType Type { get; }

	public Intrinsic(IntrinsicType type) {
		Type = type;
	}

	public override string ToString() => $"Intrinsic({Type})";
}

public enum IntrinsicType {
	// unsigned integers
	
	// signed integers
	I32,
	// other
	Tuple,
	Bool,
}
