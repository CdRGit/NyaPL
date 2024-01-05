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
	U8,
	U16,
	U32,
	U64,
	// signed integers
	I8,
	I16,
	I32,
	I64,
	// other
	Tuple,
	Ptr,
	Bool,
}
