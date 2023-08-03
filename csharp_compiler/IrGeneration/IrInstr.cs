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

	public enum IrKind {
		Phi,

		CreateTuple,

		Copy,
		LoadArguments,
		LoadTupleSection,
		LoadFunction,
		LoadIntrinsic,

		LoadLocal,
		StoreLocal,

		IntLiteral,
		BoolLiteral,

		BranchBool,
		BranchAlways,

		Call,
		CallImpure,
		Return,

		Intrinsic,
		IntrinsicImpure,
	}
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
		public OpKind Kind { get; }
		public IntrinsicOp(OpKind kind) {
			Kind = kind;
		}
		public override string ToString() => $"IntrinsicOperator({Kind})";

		public static IntrinsicOp Multiply => new(OpKind.Multiply);
		public static IntrinsicOp Divide   => new(OpKind.Divide);
		public static IntrinsicOp Modulo   => new(OpKind.Modulo);

		public static IntrinsicOp Add      => new(OpKind.Add);
		public static IntrinsicOp Subtract => new(OpKind.Subtract);

		public static IntrinsicOp Equal    => new(OpKind.Equal);
		public static IntrinsicOp NotEq    => new(OpKind.NotEq);

		public static IntrinsicOp Not      => new(OpKind.Not);
		public static IntrinsicOp Positive => new(OpKind.Positive);
		public static IntrinsicOp Negative => new(OpKind.Negative);

		public enum OpKind {
			Multiply,
			Divide,
			Modulo,

			Add,
			Subtract,

			Equal,
			NotEq,

			Not,
			Negative,
			Positive,
		}
	}
}
