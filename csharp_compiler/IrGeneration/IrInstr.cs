using System.Linq;

namespace Nyapl.IrGeneration;

public readonly struct IrInstr {
	public IrKind Kind { get; }
	public ulong[] Params { get; }
	public ulong? this[int i] { get => Params.Length > i ? Params[i] : null; }

	public IrInstr(IrKind kind, params ulong?[] param) {
		Kind = kind;
		Params = param.Where(i => i != null).Select(i => i!.Value).ToArray();
	}

	private static string ParamToString(ulong param) {
		string tostr = $"{param:X16}";
		return tostr.Substring(0, 4) + ":" + tostr.Substring(4, 4) + ":" + tostr.Substring(8, 4) + ":" + tostr.Substring(12, 4);

	}

	public override string ToString() => $"{Kind,20} {string.Join(", ", Params.Select(p => ParamToString(p)))}";

	public enum IrKind {
		StoreParam,
		AppendTupleSection,

		Copy,
		LoadArgument,
		LoadTupleSection,
		LoadFunction,
		LoadIntrinsic,

		IntLiteral,
		BoolLiteral,

		JumpIfFalse,
		JumpAlways,
		Call,
		Return,

		Multiply,
		Divide,
		Modulo,
		Add,
		Subtract,

		Equal,
		NotEq,

		Not,
		Positive,
		Negative,
	}
}
