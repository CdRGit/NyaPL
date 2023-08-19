using System.Linq;

namespace Nyapl.IrGeneration;

public readonly struct IrInstr {
	public IrKind Kind { get; }
	public IrParam[] Params { get; }
	public IrParam? this[int i] { get => Params.Length > i ? Params[i] : null; }

	public IrInstr(IrKind kind, params IrParam?[] param) {
		Kind = kind;
		Params = param.Where(i => i != null).Select(i => i!).ToArray();
	}

	public override string ToString() => $"{Kind,20} {string.Join(" ", Params.Select(p => $"{p,-20}"))}";
}

public enum IrKind {
	Phi,

	CreateTuple,

	Copy,
	LoadArguments,
	LoadTupleSection,
	LoadFunction,
	LoadLocal,
	StoreLocal,

	BranchBool,
	BranchAlways,

	Call,
	CallImpure,
	Return,

	Intrinsic,
	IntrinsicImpure,
}

public abstract class IrParam {
	public class Local : IrParam {
		public string Name { get; }
		public ushort Size { get; }
		public Local(string name, ushort size) {
			Name = name;
			Size = size;
		}
		public override string ToString() => $"Local({Size}, {Name})";
	}
	public class Register : IrParam {
		public ushort Size { get; }
		public uint Index { get; }
		public Register(ushort size, uint index) {
			Size = size;
			Index = index;
		}
		public override string ToString() => $"Register({Size}, {Index})";

		public class Dying : Register {
			public Dying(ushort size, uint index) : base(size, index) {}

			public override string ToString() => $"DyingRegister({Size}, {Index})";
		}
	}
	public class Count : IrParam {
		public ulong Value { get; }
		public Count(ulong value) {
			Value = value;
		}
		public override string ToString() => $"Count({Value})";
	}
	public class Offset : IrParam {
		public ulong Value { get; }
		public Offset(ulong value) {
			Value = value;
		}
		public override string ToString() => $"Offset({Value})";
	}
	public class Bool : IrParam {
		public bool Value { get; }
		public Bool(bool value) {
			Value = value;
		}
		public override string ToString() => $"Bool({Value})";
	}
	public class Int : IrParam {
		public ulong Value { get; }
		public Int(ulong value) {
			Value = value;
		}
		public override string ToString() => $"Int({Value})";
	}
	public class Intrinsic : IrParam {
		public ulong Index { get; }
		public Intrinsic(ulong index) {
			Index = index;
		}
		public override string ToString() => $"Intrinsic({Index})";
	}
	public class Function : IrParam {
		public ulong Index { get; }
		public Function(ulong index) {
			Index = index;
		}
		public override string ToString() => $"Function({Index})";
	}
	public class Block : IrParam {
		public IrBlock Blk { get; }
		public Block(IrBlock blk) {
			Blk = blk;
		}
		public override string ToString() => $"Block({Blk})";
	}
	public class IntrinsicOp : IrParam {
		public IrOpKind Kind { get; }
		public IntrinsicOp(IrOpKind kind) {
			Kind = kind;
		}
		public override string ToString() => $"IntrinsicOperator({Kind})";
	}
}

public enum IrOpKind {
	Multiply_signed,
	Divide_signed,
	Modulo_signed,

	Add_signed,
	Subtract_signed,

	Equal_integer,
	NotEq_integer,

	Equal_bool,
	NotEq_bool,

	Not_bool,

	Negative_signed,
	Positive_signed,
}
