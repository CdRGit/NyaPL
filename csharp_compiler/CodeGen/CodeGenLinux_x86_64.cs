using System;
using System.IO;
using System.Linq;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.Typing.Types;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public class CodeGenLinux_x86_64 : ICodeGen {
	class VSDRLA_Context {
		uint registersInUse = 0;
		Dictionary<uint, IrParam.Register[]> tuples = new();

		Dictionary<uint, IrParam.Register> replacements = new();

		public IrParam.Register MakeNewRegister(Typ t) => new(t, registersInUse++);

		public void SetTuple(IrParam.Register tuplereg, IEnumerable<IrParam.Register> parameters) {
			tuples[tuplereg.Index] = parameters.ToArray();
		}

		public IEnumerable<IrParam.Register> SimplifyRegister(IrParam.Register register) {
			if (tuples.ContainsKey(register.Index)) return tuples[register.Index];
			return new[] { (ReplaceRegister(register) as IrParam.Register)! };
		}

		public IrParam ReplaceRegister(IrParam param) {
			if (param is IrParam.Register r) {
				if (replacements.ContainsKey(r.Index)) {
					return replacements[r.Index];
				} else {
					return replacements[r.Index] = MakeNewRegister(r.Type);
				}
			} else return param;
		}
		public IrInstr ReplaceRegisters(IrInstr instr) => new(instr.Kind, instr.Params.Select(i => ReplaceRegister(i)).ToArray());
	}

	private IEnumerable<IrInstr> VerySimpleDumbRegisterLoweringAlgorithm(IrInstr instr, VSDRLA_Context ctx, Func<IrBlock, IrBlock> replace) {
		switch (instr.Kind) {
			case IrKind.LoadArguments:
				if (instr.Params.Length != 0) throw new Exception("VSDRLA for arguments has not been implemented yet, skill issue ♡");
				yield return instr;
				yield break;
			case IrKind.CreateTuple: {
				var args = instr.Params.Skip(1).SelectMany(p => ctx.SimplifyRegister((p as IrParam.Register)!));
				ctx.SetTuple((instr[0] as IrParam.Register)!, args);
				yield break;
			}
			case IrKind.LoadTupleSection: {
				var baseReg = (instr[1] as IrParam.Register)!;
				var valReg = ctx.SimplifyRegister(baseReg).ToArray()[(instr[2] as IrParam.Offset)!.Value];
				var destReg = ctx.ReplaceRegister((instr[0] as IrParam.Register)!);
				yield return new(IrKind.Copy, destReg, valReg);
				yield break;
			}
			case IrKind.Return: {
				var dest = (instr[0] as IrParam.Register)!;
				switch (dest.Type) {
					case Intrinsic i: {
							switch (i.Type) {
								case IntrinsicType.I32:
									yield return ctx.ReplaceRegisters(instr);
									break;
								default:
									throw new Exception($"VSDRLA for intrinsic '{i.Type}' not implemented yet");
							}
						} break;
					default:
						throw new Exception($"VSDRLA for type '{dest.Type}' not implemented yet");
				}
				yield break;
			}
			case IrKind.Intrinsic:
			case IrKind.Copy: {
				var dest = (instr[0] as IrParam.Register)!;
				switch (dest.Type) {
					case Intrinsic i: {
							switch (i.Type) {
								case IntrinsicType.I32:
									yield return ctx.ReplaceRegisters(instr);
									break;
								default:
									throw new Exception($"VSDRLA for intrinsic '{i.Type}' not implemented yet");
							}
						} break;
					default:
						throw new Exception($"VSDRLA for type '{dest.Type}' not implemented yet");
				}
				yield break;
			}
			default:
				throw new Exception($"VSDRLA({instr}) not implemented yet");
		}
		throw new Exception("TODO!");
	}

	public void Generate(string filePath, ReadOnlyDictionary<string, IrBlock> functions) {
		(functions, var ctx) = RegisterLowering.LowerRegisters<VSDRLA_Context>(functions, VerySimpleDumbRegisterLoweringAlgorithm);

		using (StreamWriter asmWriter = new($"{filePath}.asm")) {
			asmWriter.WriteLine("section .text");
			asmWriter.WriteLine("global _start");
			asmWriter.WriteLine("_start:");
			asmWriter.WriteLine("    xor ebp, ebp");
			asmWriter.WriteLine("    xor eax, eax");
			asmWriter.WriteLine("    call main");
			asmWriter.WriteLine("    mov edi, eax");
			asmWriter.WriteLine("    mov eax, 60");
			asmWriter.WriteLine("    syscall");
			asmWriter.WriteLine("");
			foreach (var func in functions) {
				asmWriter.WriteLine($"{MangleFunction(func.Key)}:");
				var allocCtx = RegisterAllocation.Allocate<x86_64_Registers, x86_64_Classes, x86_64_Names>(func.Value, new ());
				var machineIR = GetMIR(func.Value, ctx, allocCtx);
				WriteMIR(asmWriter, machineIR);
			}
		}
		Process.Start("nasm", new[] {"-felf64", $"{filePath}.asm"}).WaitForExit();
		Process.Start("ld", new[] {$"{filePath}.o"}).WaitForExit();
	}

	private void WriteMIR(StreamWriter asmWriter, ReadOnlyCollection<MIR> machineIR) {
		foreach (var instr in machineIR) {
			switch (instr.kind) {
				case MIR.Kind.Preamble:
					asmWriter.WriteLine("    push rbp");
					asmWriter.WriteLine("    mov rbp, rsp");
					break;
				case MIR.Kind.Label:
					asmWriter.WriteLine($"{instr.strVal}:");
					break;
				case MIR.Kind.CopyInteger: {
					var regName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    mov {regName}, {instr.int1Val}");
				} break;
				case MIR.Kind.CopyRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    mov {dstName}, {srcName}");
				} break;
				case MIR.Kind.XorInteger: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    xor {dstName}, {srcName}");
				} break;
				case MIR.Kind.AddInteger: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    add {dstName}, {srcName}");
				} break;
				case MIR.Kind.SubInteger: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    sub {dstName}, {srcName}");
				} break;
				case MIR.Kind.IMulInteger: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    imul {dstName}, {srcName}");
				} break;
				case MIR.Kind.IDivInteger: {
					var srcName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    idiv {srcName}");
				} break;
				case MIR.Kind.Return: {
					if (instr.reg0Val != x86_64_Names.RAX) {
						var regName = GetRegName(instr.reg0Val, instr.int0Val);
						asmWriter.WriteLine($"    mov {GetRegName(x86_64_Names.RAX, instr.int0Val)}, {regName}");
					}
					asmWriter.WriteLine("    mov rsp, rbp");
					asmWriter.WriteLine("    pop rbp");
					asmWriter.WriteLine("    ret");
				} break;
				case MIR.Kind.PushRegister: {
					var regName = GetRegName(instr.reg0Val, 64);
					asmWriter.WriteLine($"    push {regName}");
				} break;
				case MIR.Kind.PopRegister: {
					var regName = GetRegName(instr.reg0Val, 64);
					asmWriter.WriteLine($"    pop {regName}");
				} break;
				default:
					throw new Exception($"WriteMIR(MIR.Kind.{instr.kind}) not implemented yet");
			}
		}
	}

	private static Dictionary<x86_64_Names, Dictionary<ulong, string>> regNames = new() {
		{ x86_64_Names.RAX, new() {
			{64, "rax"},
			{32, "eax"},
		}},
		{ x86_64_Names.RDX, new() {
			{64, "rdx"},
			{32, "edx"},
		}},
		{ x86_64_Names.RDI, new() {
			{64, "rax"},
			{32, "eax"},
		}},
		{ x86_64_Names.R10, new() {
			{64, "r10"},
			{32, "r10d"},
		}},
		{ x86_64_Names.R11, new() {
			{64, "r11"},
			{32, "r11d"},
		}},
	};
	private string GetRegName(x86_64_Names name, ulong bitCount) {
		if (!regNames.ContainsKey(name)) throw new Exception($"GetRegName({name}, {bitCount}) not ready yet: name");
		var reg = regNames[name];
		if (!reg.ContainsKey(bitCount)) throw new Exception($"GetRegName({name}, {bitCount}) not ready yet: bitCount");
		return reg[bitCount];
	}

	private int BitSize(Typ type) {
		switch (type) {
			case Intrinsic i: {
					switch (i.Type) {
						case IntrinsicType.I32:
							return 32;
						default:
							throw new Exception($"BitSize for intrinsic '{i.Type}' not implemented yet");
					}
				} break;
			default:
				throw new Exception($"BitSize for type '{type}' not implemented yet");
		}
	}

	private bool Signed(Typ type) {
		switch (type) {
			case Intrinsic i: {
					switch (i.Type) {
						case IntrinsicType.I32:
							return true;
						default:
							throw new Exception($"Signed for intrinsic '{i.Type}' not implemented yet");
					}
				} break;
			default:
				throw new Exception($"Signed for type '{type}' not implemented yet");
		}
	}

	private string MangleFunction(string functionName) {
		return functionName.Replace("/", "~");
	}

	private ReadOnlyCollection<MIR> GetMIR(IrBlock block, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		var ctx = new Context();
		return GetMIR(block, ctx, lowererContext, allocContext);
	}

	private ReadOnlyCollection<MIR> GetMIR(IrBlock block, Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		if (ctx.Visited(block)) return new List<MIR>().AsReadOnly();
		ctx.Visit(block);
		List<MIR> data = new();
		data.Add(MIR.Label($".block_{block.ID}"));
		foreach (var instr in block.Instructions) {
			switch (instr.Kind) {
				case IrKind.Copy: {
					var reg = (instr[0] as IrParam.Register)!;
					var regName = allocContext.GetName(reg);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, dest) = regName.Value;
					switch (regClass) {
						case x86_64_Classes.Integer: {
							var src = instr[1]!;
							switch (src) {
								case IrParam.Int i: {
									data.Add(MIR.CopyInteger(dest, BitSize(reg.Type), i.Value));
								} break;
								case IrParam.Register r: {
									regName = allocContext.GetName(r);
									if (regName == null) throw new Exception($"Register {r} not named");
									var (_, srcReg) = regName.Value;
									if (srcReg == dest) break; // noop
									data.Add(MIR.CopyRegister(dest, BitSize(reg.Type), srcReg));
								} break;
								default:
									throw new Exception($"GetMIR(Copy.Src: {src}) not implemented yet");
							}
						} break;
						default:
							throw new Exception($"GetMIR(Copy.Dest: {regClass}) not implemented yet");
					}
					break;
				}
				case IrKind.LoadArguments:
					if (instr.Params.Length != 0) throw new Exception("cannot generate MIR for non-zero argument count functions");
					data.Add(MIR.Preamble());
					break;
				case IrKind.Return: {
					var reg = (instr[0] as IrParam.Register)!;
					var regName = allocContext.GetName(reg);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, src) = regName.Value;
					data.Add(MIR.Return(src, BitSize(reg.Type)));
				} break;
				case IrKind.Intrinsic: {
					var regName = allocContext.GetName((instr[0] as IrParam.Register)!);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, dest) = regName.Value;
					var intrinsicOp = (instr[1] as IrParam.IntrinsicOp)!;
					switch (intrinsicOp.Kind) {
						case IrOpKind.Add: {
								var r = (instr[2] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src1Reg) = regName.Value;
								r = (instr[3] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src2Reg) = regName.Value;
								switch (regClass) {
									case x86_64_Classes.Integer: {
											// see if we can do a simple 2-addr int add
											if (src1Reg == dest) {
												data.Add(MIR.AddInteger(dest, BitSize(r.Type), src2Reg));
											} else if (src2Reg == dest) {
												data.Add(MIR.AddInteger(dest, BitSize(r.Type), src1Reg));
											} else {
												int bitCount = BitSize(r.Type);
												data.Add(MIR.PushRegister(src1Reg));
												data.Add(MIR.AddInteger(src1Reg, bitCount, src2Reg));
												data.Add(MIR.CopyRegister(dest, bitCount, src1Reg));
												data.Add(MIR.PopRegister(src1Reg));
											}
										} break;
									default:
										throw new Exception($"{regClass} add not implemented yet");
								}
							} break;
						case IrOpKind.Subtract: {
								var r = (instr[2] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src1Reg) = regName.Value;
								r = (instr[3] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src2Reg) = regName.Value;
								switch (regClass) {
									case x86_64_Classes.Integer: {
											// see if we can do a simple 2-addr int sub
											if (src1Reg == dest) {
												data.Add(MIR.SubInteger(dest, BitSize(r.Type), src2Reg));
											} else {
												int bitCount = BitSize(r.Type);
												data.Add(MIR.PushRegister(src1Reg));
												data.Add(MIR.SubInteger(src1Reg, bitCount, src2Reg));
												data.Add(MIR.CopyRegister(dest, bitCount, src1Reg));
												data.Add(MIR.PopRegister(src1Reg));
											}
										} break;
									default:
										throw new Exception($"{regClass} subtract not implemented yet");
								}
							} break;
						case IrOpKind.Multiply: {
								var r = (instr[2] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src1Reg) = regName.Value;
								r = (instr[3] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src2Reg) = regName.Value;
								switch (regClass) {
									case x86_64_Classes.Integer: {
											// sign vs unsign
											var signed = Signed(r.Type);
											if (signed) {
												// imul
												int bitCount = BitSize(r.Type);
												data.Add(MIR.PushRegister(src1Reg));
												data.Add(MIR.IMulInteger(src1Reg, bitCount, src2Reg));
												data.Add(MIR.CopyRegister(dest, bitCount, src1Reg));
												data.Add(MIR.PopRegister(src1Reg));
											} else {
												throw new Exception("unsigned int multiply not implemented yet");
											}
										} break;
									default:
										throw new Exception($"{regClass} add not implemented yet");
								}
							} break;
						case IrOpKind.Divide: {
								var r = (instr[2] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src1Reg) = regName.Value;
								r = (instr[3] as IrParam.Register)!;
								regName = allocContext.GetName(r);
								if (regName == null) throw new Exception($"Register {r} not named");
								var (_, src2Reg) = regName.Value;
								switch (regClass) {
									case x86_64_Classes.Integer: {
											// sign vs unsign
											var signed = Signed(r.Type);
											if (signed) {
												// idiv
												int bitCount = BitSize(r.Type);
												if (dest != x86_64_Names.RAX)
													data.Add(MIR.PushRegister(x86_64_Names.RAX));
												data.Add(MIR.PushRegister(x86_64_Names.RDX));

												data.Add(MIR.XorInteger(x86_64_Names.RDX, bitCount, x86_64_Names.RDX));
												if (src1Reg != x86_64_Names.RAX)
													data.Add(MIR.CopyRegister(x86_64_Names.RAX, bitCount, src1Reg));
												data.Add(MIR.IDivInteger(bitCount, src2Reg));
												if (dest != x86_64_Names.RAX)
													data.Add(MIR.CopyRegister(dest, bitCount, x86_64_Names.RAX));

												data.Add(MIR.PopRegister(x86_64_Names.RDX));
												if (dest != x86_64_Names.RAX)
													data.Add(MIR.PopRegister(x86_64_Names.RAX));
											} else {
												throw new Exception("unsigned int division not implemented yet");
											}
										} break;
									default:
										throw new Exception($"{regClass} add not implemented yet");
								}
							} break;
						default:
							throw new Exception($"GetMIR(Intrinsic.Kind: {intrinsicOp}) not implemented yet");
					}
					// done
				} break;
				default:
					throw new Exception($"GetMIR(IrKind.{instr.Kind}) not implemented yet");
			}
		}
		return data.AsReadOnly();
	}

	class Context {
		HashSet<IrBlock> visited = new();
		public void Visit(IrBlock block) => visited.Add(block);
		public bool Visited(IrBlock block) => visited.Contains(block);
	}

	struct MIR {
		public static MIR Label(string label_text) => new() {
			kind = Kind.Label,
			strVal = label_text,
		};

		public static MIR Preamble() => new() {
			kind = Kind.Preamble,
		};

		public static MIR Return(x86_64_Names src, int bitCount) => new() {
			kind = Kind.Return,
			reg0Val = src,
			int0Val = (ulong)bitCount,
		};

		public static MIR CopyInteger(x86_64_Names dest, int bitCount, ulong val) => new() {
			kind = Kind.CopyInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			int1Val = val,
		};

		public static MIR CopyRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.CopyRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR XorInteger(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.XorInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR AddInteger(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.AddInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR SubInteger(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.SubInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR IMulInteger(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.IMulInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR IDivInteger(int bitCount, x86_64_Names src) => new() {
			kind = Kind.IDivInteger,
			reg0Val = src,
			int0Val = (ulong)bitCount,
		};

		public static MIR PushRegister(x86_64_Names src) => new() {
			kind = Kind.PushRegister,
			reg0Val = src,
		};

		public static MIR PopRegister(x86_64_Names dest) => new() {
			kind = Kind.PopRegister,
			reg0Val = dest,
		};

		public Kind kind;
		public enum Kind {
			Label,
			Preamble,
			CopyInteger,
			CopyRegister,

			XorInteger,

			AddInteger,
			SubInteger,
			IMulInteger,
			IDivInteger,

			PushRegister,
			PopRegister,

			Return,
		}

		public x86_64_Names reg0Val;
		public x86_64_Names reg1Val;
		public string strVal;
		public ulong int0Val;
		public ulong int1Val;
	}

	class x86_64_Registers : IRegisterSet<x86_64_Classes, x86_64_Names> {
		public x86_64_Classes Classify(IrParam.Register r) {
			switch (r.Type) {
				case Intrinsic i:
					switch (i.Type) {
						case IntrinsicType.I32:
							return x86_64_Classes.Integer;
						default:
							throw new Exception($"Classify for intrinsic '{i.Type}' not implemented yet");
					} break;
			}
			throw new Exception("TODO!");
		}

		public x86_64_Names[] GetOrderedRegisters(x86_64_Classes c) {
			switch (c) {
				case x86_64_Classes.Integer:
					{
						return new[] {
							x86_64_Names.R11,
							x86_64_Names.R10,
							x86_64_Names.RAX,
							x86_64_Names.RDI,
							x86_64_Names.RSI,
							x86_64_Names.RDX,
							x86_64_Names.RCX,
							x86_64_Names.R8,
							x86_64_Names.R9,
							x86_64_Names.RBX,
							x86_64_Names.R12,
							x86_64_Names.R13,
							x86_64_Names.R14,
							x86_64_Names.R15
						};
					}
			}
			throw new Exception("TODO!");
		}

		public bool NeedsAllocation(x86_64_Classes c) {
			return c != x86_64_Classes.Undecidable;
		}
		public int GetRegisterCount(x86_64_Classes c) {
			return GetOrderedRegisters(c).Length;
		}
	}

	enum x86_64_Classes {
		Undecidable, // for tuple registers left over in parameters
		Integer,
	}

	enum x86_64_Names {
		// Integer
		RAX,
		RBX,
		RCX,
		RDX,
		RSI,
		RDI,
		// not general purpose, but leaving them in for the sake of rember
		// RBP,
		// RSP,
		R8,
		R9,
		R10,
		R11,
		R12,
		R13,
		R14,
		R15,
	}
}
