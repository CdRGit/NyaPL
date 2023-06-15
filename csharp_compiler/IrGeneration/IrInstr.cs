namespace Nyapl.IrGeneration;

public readonly struct IrInstr {
	public IrKind Kind { get; }
	public ulong Param0  { get; }
	public ulong Param1  { get; }
	public ulong Param2  { get; }

	public IrInstr(IrKind kind, ulong param0 = 0, ulong param1 = 0, ulong param2 = 0) {
		Kind = kind;
		Param0 = param0;
		Param1 = param1;
		Param2 = param2;
	}

	public override string ToString() => $"{Kind} {Param0} {Param1} {Param2}";

	public enum IrKind {
		Return,
		StoreParam,
		Call,

		Copy,
		LoadArgument,
		LoadFunction,
		LoadIntrinsic,

		IntLiteral,

		Multiply,
		Add,
	}
}
