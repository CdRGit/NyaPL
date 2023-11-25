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

		private bool IsSimpleType(Typ type) {
			switch (type) {
				case Intrinsic i: {
					switch (i.Type) {
						case IntrinsicType.I32:
							return true;
						default:
							throw new Exception($"Cannot determine simplicity of intrinsic {i.Type}");
					}
				} break;
				case Apply a: {
					switch (a.BaseType) {
						case Function:
							return true;
						case Intrinsic i: {
							switch (i.Type) {
								case IntrinsicType.Tuple:
									return false;
								default:
									throw new Exception($"Cannot determine simplicity of apply intrinsic {i.Type}");
							}
						} break;
						default:
							throw new Exception($"Cannot determine simplicity of apply {a.BaseType}");
					}
				} break;
			}
			throw new Exception($"TODO: {type}");
		}

		private IEnumerable<IrParam.Register> Flatten(IrParam param) {
			switch (param) {
				case IrParam.Register r:
					return new[] {r};
				case IrParam.CompositeRegister c:
					return c.Registers;
				default:
					throw new Exception("UNREACHABLE");
			}
		}

		private IrParam MakeRegister(Typ type, IrParam.Register? register) {
			if (IsSimpleType(type)) return MakeNewRegister(type); // trivial case handled

			switch (type) {
				case Apply a: {
					switch (a.BaseType) {
						case Intrinsic i: {
							switch (i.Type) {
								case IntrinsicType.Tuple: {
									if (register != null && tuples.ContainsKey(register!.Index)) {
										return new IrParam.CompositeRegister(tuples[register!.Index], type);
									}
									// it's suffering time
									// loop through all argument types and map those into their relevant types
									var args = a.ParameterTypes.SelectMany(t => Flatten(MakeRegister(t, null))).ToArray();
									if (register != null) {
										SetTuple(register, args);
									}
									return new IrParam.CompositeRegister(args, type);
								} break;
								default:
									throw new Exception($"Cannot create composite register for apply intrinsic {i.Type}");
							}
						} break;
					}
				} break;
			}
			throw new Exception($"TODO: {type}");
		}

		public IrParam ReplaceWithCompositeRegister(IrParam param) {
			if (param is IrParam.Register r) {
				if (IsSimpleType(r.Type)) {
					return ReplaceRegister(param);
				} else {
					// here we fuckign go
					return MakeRegister(r.Type, r);
				}
			} else return param;
		}
		public IrInstr ReplaceWithCompositeRegisters(IrInstr instr) => new(instr.Kind, instr.Params.Select(i => ReplaceWithCompositeRegister(i)).ToArray());
	}

	enum MachineSpecificIrInstrs {
	}

	private IEnumerable<IrInstr> VerySimpleDumbRegisterLoweringAlgorithm(IrInstr instr, VSDRLA_Context ctx, Func<IrBlock, IrBlock> replace) {
		switch (instr.Kind) {
			case IrKind.LoadArguments:
				yield return ctx.ReplaceWithCompositeRegisters(instr);
				yield break;
			case IrKind.Call: {
				yield return ctx.ReplaceWithCompositeRegisters(instr);
				yield break;
			}
			case IrKind.Return: {
				yield return ctx.ReplaceWithCompositeRegisters(instr);
				yield break;
			}
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
				Console.WriteLine($"{baseReg.Index}[{(instr[2] as IrParam.Offset)!.Value}] -> {valReg}");
				yield break;
			}
			case IrKind.LoadFunction:
			case IrKind.Intrinsic:
			case IrKind.Copy: {
				var dest = (instr[0] as IrParam.Register)!;
				switch (dest.Type) {
					case Intrinsic i: {
							switch (i.Type) {
								case IntrinsicType.I32:
								case IntrinsicType.Bool:
									yield return ctx.ReplaceRegisters(instr);
									break;
								default:
									throw new Exception($"VSDRLA for intrinsic '{i.Type}' not implemented yet");
							}
						} break;
					case Apply a: {
							switch (a.BaseType) {
								case Function f:
									yield return ctx.ReplaceRegisters(instr);
									break;
								default:
									throw new Exception($"VSDRLA for apply '{a.BaseType}' not implemented yet");
							}
						} break;
					default:
						throw new Exception($"VSDRLA for type '{dest.Type}' not implemented yet");
				}
				yield break;
			}
			case IrKind.Phi: {
				var dest = (instr[0] as IrParam.Register)!;
				switch (dest.Type) {
					case Intrinsic i: {
							switch (i.Type) {
								case IntrinsicType.I32:
									yield return ctx.ReplaceRegisters(new IrInstr(instr.Kind, instr.Params.Select(p => p is IrParam.Block b ? new IrParam.Block(replace(b.Blk)) : p).ToArray()));
									break;
								case IntrinsicType.Bool:
									yield return ctx.ReplaceRegisters(new IrInstr(instr.Kind, instr.Params.Select(p => p is IrParam.Block b ? new IrParam.Block(replace(b.Blk)) : p).ToArray()));
									break;
								default:
									throw new Exception($"VSDRLA for intrinsic '{i.Type}' not implemented yet");
							}
						} break;
					default:
						throw new Exception($"VSDRLA for type '{dest.Type}' not implemented yet");
				}
			} yield break;
			case IrKind.BranchAlways:
			case IrKind.BranchBool: {
				yield return ctx.ReplaceRegisters(new IrInstr(instr.Kind, instr.Params.Select(p => p is IrParam.Block b ? new IrParam.Block(replace(b.Blk)) : p).ToArray()));
			} yield break;
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
		Process.Start("ld", new[] {$"{filePath}.o", "-o", $"{filePath}"}).WaitForExit();
	}

	private void WriteMIR(StreamWriter asmWriter, ReadOnlyCollection<MIR> machineIR) {
		foreach (var instr in machineIR) {
			switch (instr.kind) {
				case MIR.Kind.Preamble:
					asmWriter.WriteLine("    push rbp");
					asmWriter.WriteLine("    mov rbp, rsp");
					break;
				case MIR.Kind.Comment:
					asmWriter.WriteLine($"; {instr.strVal}");
					break;
				case MIR.Kind.Label:
					asmWriter.WriteLine($"{instr.strVal}:");
					break;
				case MIR.Kind.JumpNE:
					asmWriter.WriteLine($"    jne {instr.strVal}");
					break;
				case MIR.Kind.JumpE:
					asmWriter.WriteLine($"    je {instr.strVal}");
					break;
				case MIR.Kind.Jump:
					asmWriter.WriteLine($"    jmp {instr.strVal}");
					break;
				case MIR.Kind.MovInteger: {
					var regName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    mov {regName}, {instr.int1Val}");
				} break;
				case MIR.Kind.CmoveInteger: {
					var regName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    cmove {regName}, {instr.int1Val}");
				} break;
				case MIR.Kind.MovRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    mov {dstName}, {srcName}");
				} break;
				case MIR.Kind.MovRegisterIndirect: {
					var dstName = GetRegName(instr.reg0Val, 64);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					long offset = (long)instr.int1Val;
					asmWriter.WriteLine($"    mov {dstName}[{offset}], {srcName}");
				} break;
				case MIR.Kind.MovIndirectRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, 64);
					long offset = (long)instr.int1Val;
					asmWriter.WriteLine($"    mov {dstName}, {srcName}[{offset}]");
				} break;
				case MIR.Kind.MovLabel: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    mov {dstName}, {instr.strVal}");
				} break;
				case MIR.Kind.SetNE: {
					var regName = GetRegName(instr.reg0Val, 8);
					asmWriter.WriteLine($"    setne {regName}");
				} break;
				case MIR.Kind.TestRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    test {dstName}, {srcName}");
				} break;
				case MIR.Kind.CmpRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    cmp {dstName}, {srcName}");
				} break;
				case MIR.Kind.XorRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    xor {dstName}, {srcName}");
				} break;
				case MIR.Kind.AddRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    add {dstName}, {srcName}");
				} break;
				case MIR.Kind.AddInteger: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    add {dstName}, {instr.int1Val}");
				} break;
				case MIR.Kind.SubRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    sub {dstName}, {srcName}");
				} break;
				case MIR.Kind.SubInteger: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    sub {dstName}, {instr.int1Val}");
				} break;
				case MIR.Kind.IMulRegister: {
					var dstName = GetRegName(instr.reg0Val, instr.int0Val);
					var srcName = GetRegName(instr.reg1Val, instr.int0Val);
					asmWriter.WriteLine($"    imul {dstName}, {srcName}");
				} break;
				case MIR.Kind.IDivRegister: {
					var srcName = GetRegName(instr.reg0Val, instr.int0Val);
					asmWriter.WriteLine($"    idiv {srcName}");
				} break;
				case MIR.Kind.CallRegister: {
					var regName = GetRegName(instr.reg0Val, 64);
					asmWriter.WriteLine($"    call {regName}");
				} break;
				case MIR.Kind.Return: {
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
			{16, "ax"},
			{8,  "al"},
		}},
		{ x86_64_Names.RBX, new() {
			{64, "rbx"},
			{32, "ebx"},
			{16, "bx"},
			{8,  "bl"},
		}},
		{ x86_64_Names.RCX, new() {
			{64, "rcx"},
			{32, "ecx"},
			{16, "cx"},
			{8,  "cl"},
		}},
		{ x86_64_Names.RDX, new() {
			{64, "rdx"},
			{32, "edx"},
			{16, "dx"},
			{8,  "dl"},
		}},
		{ x86_64_Names.RSI, new() {
			{64, "rsi"},
			{32, "esi"},
			{16, "si"},
			{8,  "sil"},
		}},
		{ x86_64_Names.RDI, new() {
			{64, "rdi"},
			{32, "edi"},
			{16, "di"},
			{8,  "dil"},
		}},
		{ x86_64_Names.R8, new() {
			{64, "r8"},
			{32, "r8d"},
			{16, "r8w"},
			{8,  "r8b"},
		}},
		{ x86_64_Names.R9, new() {
			{64, "r9"},
			{32, "r9d"},
			{16, "r9w"},
			{8,  "r9b"},
		}},
		{ x86_64_Names.R10, new() {
			{64, "r10"},
			{32, "r10d"},
			{16, "r10w"},
			{8,  "r10b"},
		}},
		{ x86_64_Names.R11, new() {
			{64, "r11"},
			{32, "r11d"},
			{16, "r11w"},
			{8,  "r11b"},
		}},
		{ x86_64_Names.R12, new() {
			{64, "r12"},
			{32, "r12d"},
			{16, "r12w"},
			{8,  "r12b"},
		}},
		{ x86_64_Names.R13, new() {
			{64, "r13"},
			{32, "r13d"},
			{16, "r13w"},
			{8,  "r13b"},
		}},
		{ x86_64_Names.R14, new() {
			{64, "r14"},
			{32, "r14d"},
			{16, "r14w"},
			{8,  "r14b"},
		}},
		{ x86_64_Names.R15, new() {
			{64, "r15"},
			{32, "r15d"},
			{16, "r15w"},
			{8,  "r15b"},
		}},
		{ x86_64_Names.RSP, new() {
			{64, "rsp"},
			{32, "esp"},
			{16, "sp"},
			{8,  "spl"},
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
			case Apply a: {
					switch (a.BaseType) {
						case Function:
							return 64;
						default:
							throw new Exception($"BitSize for Apply '{a.BaseType}' not implemented yet");
					}
				}
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
		List<MIR> data = new();
		data.Add(MIR.Comment($"registers: "));
		foreach (var pair in allocContext.Names) {
			data.Add(MIR.Comment($"{pair.Key}: {pair.Value}"));
		}
		data.AddRange(GetMIR(block, ctx, lowererContext, allocContext));
		return data.AsReadOnly();
	}

	private void HandlePhiCopies(List<MIR> data, IrBlock sourceBlock, IrBlock targetBlock, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		foreach (var instr in targetBlock.Instructions) {
			if (instr.Kind != IrKind.Phi) break;
			var reg = (instr[0] as IrParam.Register)!;
			var regName = allocContext.GetName(reg);
			if (regName == null) throw new Exception($"Register {instr[0]} not named");
			var (regClass, dest) = regName.Value;
			for (int i = 1; i < instr.Params.Length; i+=2) {
				if ((instr[i] as IrParam.Block)!.Blk == sourceBlock) {
					var r = (instr[i + 1] as IrParam.Register)!;
					regName = allocContext.GetName(r);
					if (regName == null) throw new Exception($"Register {r} not named");
					var (_, srcReg) = regName.Value;
					if (srcReg == dest) break; // noop
					data.Add(MIR.Comment($"Φ: {reg} <- {r}"));
					data.Add(MIR.MovRegister(dest, BitSize(reg.Type), srcReg));
					break;
				}
			}
		}
	}

	enum SysV_Classes {
		NO_CLASS,
		INTEGER,
		SSE,
		SSEUP,
		X87,
		X87UP,
		COMPLEX_X87,
		MEMORY,
	}

	struct Eightbyte {
		public Typ[] fieldTypes;

		public Eightbyte(Typ type) {
			fieldTypes = new[] { type };
		}

		public override string ToString() {
			return $"EB: [{string.Join(", ", (IEnumerable<Typ>)fieldTypes)}]";
		}
	}
	private SysV_Classes SysV_Classify_Eightbytes(Eightbyte[] vals) {
		if (vals.Length > 4) return SysV_Classes.MEMORY;
		var classes = new SysV_Classes[vals.Length];
		for (int i = 0; i < classes.Length; i++) {
			classes[i] = SysV_Classes.NO_CLASS;
		}
		for (int i = 0; i < vals.Length; i++) {
			Console.WriteLine($"{i}");
			foreach (var t in vals[i].fieldTypes) {
				var c = SysV_Classify(t).Item1;
				Console.WriteLine($"  {c}");
				if (classes[i] == c) {
					// all good, let's go
				} else if (classes[i] == SysV_Classes.NO_CLASS) {
					classes[i] = c;
				} else {
					throw new Exception("TODO!");
				}
			}
		}
		if (classes.Any(c => c == SysV_Classes.MEMORY)) return SysV_Classes.MEMORY;
		// x87 stuff, we do not care
		// sse, we do not care
		return classes[0];
	}

	private (int, int) ByteData(Typ type) {
		switch (type) {
			case Intrinsic i: {
				switch (i.Type) {
					case IntrinsicType.I32:
						return (4, 4);
					default:
						throw new Exception($"TODO: Intrinsic[{i.Type}]");
				}
			} break;
			case Apply a: {
				switch (a.BaseType) {
					case Function:
						return (8, 8);
					default:
						throw new Exception($"TODO: Apply[{a.BaseType}]");
				}
			} break;
			default:
				throw new Exception($"TODO: {type}");
		}
	}

	private Eightbyte[] SplitUp(IEnumerable<Typ> types) {
		List<Eightbyte> vals = new();
		int currOffset = 0;
		List<Typ> eightbyteTypes = new();
		foreach (var t in types) {
			var (size, align) = ByteData(t);
			Console.WriteLine($"({size}, {align})");
			if (currOffset % align != 0) {
				currOffset += align - currOffset % align;
			}
			if (currOffset >= 8) {
				Console.WriteLine($"bump");
				vals.Add(new Eightbyte {fieldTypes = eightbyteTypes.ToArray()});
				eightbyteTypes = new();
				currOffset = 0;
			}
			Console.WriteLine($"{currOffset}");
			currOffset += size;
			eightbyteTypes.Add(t);
		}
		if (eightbyteTypes.Count != 0) {
			vals.Add(new Eightbyte {fieldTypes = eightbyteTypes.ToArray()});
		}
		return vals.ToArray();
	}

	private (SysV_Classes, Eightbyte[] eightBytes) SysV_Classify(Typ type) {
		switch (type) {
			case Apply a: {
				switch (a.BaseType) {
					case Function:
						return (SysV_Classes.INTEGER, new[] {new Eightbyte(type)});
					case Intrinsic i: {
						switch (i.Type) {
							case IntrinsicType.Tuple:
								var split = SplitUp(a.ParameterTypes);
								return (SysV_Classify_Eightbytes(split), split);
							default:
								throw new Exception($"Cannot classify Apply[Intrinsic[{i.Type}]] yet");
						}
					} break;
					default:
						throw new Exception($"Cannot classify Apply[{a.BaseType}] yet");
				}
			} break;
			case Intrinsic i: {
				switch (i.Type) {
					case IntrinsicType.I32:
						return (SysV_Classes.INTEGER, new[] {new Eightbyte(type)});
					default:
						throw new Exception($"Cannot classify Intrinsic[{i.Type}] yet");
				}
			} break;
			default:
				throw new Exception($"Cannot classify '{type}' type yet");
		}
	}

	private SysV_Classes SysV_Classify(IrParam p) {
		Console.WriteLine(p);
		switch (p) {
			case IrParam.Register r:
				return SysV_Classify(r.Type).Item1;
			case IrParam.CompositeRegister c:
				return SysV_Classify(c.Type).Item1;
			default:
				throw new Exception("Cannot classify non-registers");
		}
		throw new Exception("TODO!");
	}

	private static x86_64_Names[] callee_preserved = {
		x86_64_Names.RBX,
		x86_64_Names.R12,
		x86_64_Names.R13,
		x86_64_Names.R14,
		x86_64_Names.R15,
	};

	private static x86_64_Names[] caller_preserved = {
		x86_64_Names.RAX,
		x86_64_Names.RCX,
		x86_64_Names.RDX,
		x86_64_Names.RSI,
		x86_64_Names.RDI,
		x86_64_Names.R8,
		x86_64_Names.R9,
		x86_64_Names.R10,
		x86_64_Names.R11,
	};

	private static Dictionary<SysV_Classes, x86_64_Names[]> sysv_argument_registers = new() {
		{SysV_Classes.INTEGER, new [] {x86_64_Names.RDI, x86_64_Names.RSI, x86_64_Names.RDX, x86_64_Names.RCX, x86_64_Names.R8, x86_64_Names.R9}}
	};

	private (Typ ret, IEnumerable<Typ> args) ArgTypes(Apply functionType) {
		if (functionType == null) throw new ArgumentNullException(nameof(functionType));
		if (!(functionType.BaseType is Function)) throw new ArgumentException(nameof(functionType), "Expected Apply<Function>");

		return (functionType.ParameterTypes[0], functionType.ParameterTypes.Skip(1));
	}

	private ulong Align(ulong val, ulong align) {
		if (val % align == 0) return val;

		return val + (align - (val % align));
	}

	private (ulong size, bool retOnStack) GetRequiredStackPassSize(Typ returnType, IEnumerable<Typ> args) {
		// get the required size for the return type on-stack
		var retClass = SysV_Classify(returnType);

		ulong returnSize;
		bool retOnStack;
		switch (retClass.Item1) {
			case SysV_Classes.INTEGER:
				if (retClass.eightBytes.Length > 2) {
					returnSize = (ulong)retClass.eightBytes.Length * 8;
					retOnStack = true;
				} else {
					returnSize = 0;
					retOnStack = false;
				}
				break;
			default:
				throw new Exception($"TODO! return of class {retClass}");
		}
		returnSize = Align(returnSize, 8);
		// got it, we also now know if we're passing the return value on-stack or in register
		// 5 registers if we're returning on stack because a `(int, int) -> memory` effectively gets rewritten to a `(memory*, int, int) -> memory*`
		ulong availableIntegerRegisters = retOnStack ? 5ul : 6ul;

		// get the required size for the arguments on-stack
		var argClasses = args.Select(a => SysV_Classify(a));
		ulong argSize = 0;

		foreach (var arg in argClasses) {
			switch (arg.Item1) {
				case SysV_Classes.INTEGER: {
					if ((ulong)arg.eightBytes.Length <= availableIntegerRegisters) {
						availableIntegerRegisters -= (ulong)arg.eightBytes.Length;
					} else {
						throw new Exception($"TODO! stack-passing of argument {arg}");
					}
				} break;
				default:
					throw new Exception($"TODO! passing of argument {arg}");
			}
		}

		argSize = Align(argSize, 8);
		return (returnSize + argSize, retOnStack);
	}

	private ulong GetRequiredStackSpillSize(IEnumerable<Typ> args) {
		var argClasses = args.Select(a => SysV_Classify(a));
		ulong argSize = 0;

		foreach (var arg in argClasses) {
			argSize += (ulong)arg.eightBytes.Length * 8;
		}

		argSize = Align(argSize, 8);
		return argSize;
	}

	private IEnumerable<x86_64_Names> GetRegsUsedForReturn(IrInstr instr, Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		var returnVal = instr[0];
		switch (returnVal) {
			case IrParam.Register r: {
				var rName = allocContext.GetName(r);
				if (rName == null) throw new Exception($"Register {r} not named");
				return new[] { rName.Value.Item2 };
			} break;
			default: throw new Exception($"TODO! GetRegsUsedForReturn for a return type of {returnVal.GetType()}");
		}
	}

	private IEnumerable<IrParam.Register> Flatten(IrParam param) {
		switch (param) {
			case IrParam.CompositeRegister c:
				return c.Registers;
			case IrParam.Register r:
				return new[] { r };
			default:
				throw new Exception($"Could not flatten {param}");
		}
	}

	private IEnumerable<MIR> SpillRegisters(
			(Typ ret, IEnumerable<Typ> args) argTypes, IrInstr instr,
			Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext)
	{
		var paramRegs = instr.Params.Skip(2);
		ulong offset = 0;
		foreach (var (type, reg) in argTypes.args.Zip(paramRegs)) {
			var classed = SysV_Classify(type);
			var flattened = Flatten(reg).ToArray();
			int flattenedIdx = 0;
			foreach (var eb in classed.eightBytes) {
				ulong internalOffset = 0;
				foreach (var t in eb.fieldTypes) {
					var r = flattened[flattenedIdx++];
					var rName = allocContext.GetName(r);
					if (rName == null) throw new Exception($"Register {r} not named");
					yield return MIR.Comment($"Spilling {rName.Value.Item2} [{r}] onto stack");
					yield return MIR.MovRegisterIndirect(x86_64_Names.RSP, BitSize(t), rName.Value.Item2, (long)(offset + internalOffset));
					internalOffset += (ulong)(BitSize(t) / 8);
				}
				offset += 8;
			}
		}
	}

	static readonly x86_64_Names[] INT_ARG_REGS = {x86_64_Names.RDI, x86_64_Names.RSI, x86_64_Names.RDX, x86_64_Names.RCX, x86_64_Names.R8, x86_64_Names.R9};
	private IEnumerable<MIR> PickupArguments(
			(Typ ret, IEnumerable<Typ> args) argTypes, bool retOnStack, ulong spillSize, IrInstr instr,
			Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext)
	{
		int intArgRegsIdx = retOnStack ? 1 : 0;

		var paramRegs = instr.Params.Skip(2);
		ulong srcOffset = 0;
		ulong dstOffset = 0;
		foreach (var (type, reg) in argTypes.args.Zip(paramRegs)) {
			var classed = SysV_Classify(type);
			int eightBytesIn = 0;
			foreach (var eb in classed.eightBytes) {
				switch (classed.Item1) {
					case SysV_Classes.INTEGER:
						if (intArgRegsIdx + classed.eightBytes.Length - eightBytesIn < INT_ARG_REGS.Length) {
							yield return MIR.Comment($"Picking up INTEGER argument eight-byte into {INT_ARG_REGS[intArgRegsIdx]}");
							yield return MIR.MovIndirectRegister(INT_ARG_REGS[intArgRegsIdx++], 64, x86_64_Names.RSP, (long)srcOffset);
						} else {
							throw new Exception("stack-passing");
						}
						break;
					default:
						throw new Exception($"PickupArguments({classed})");
				}
				eightBytesIn++;
				srcOffset += 8;
			}
		}
	}

	static readonly x86_64_Names[] INT_RET_REGS = {x86_64_Names.RAX, x86_64_Names.RDX};
	private IEnumerable<MIR> CollectReturnValue((Typ ret, IEnumerable<Typ> args) argTypes, IrParam resultReg, bool retOnStack,
			Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext
	) {
		var classed = SysV_Classify(argTypes.ret);
		switch (classed.Item1) {
			case SysV_Classes.INTEGER: {
				if (retOnStack) {
					throw new Exception("stack-passing");
				} else {
					yield return MIR.Comment($"Return data in registers, reserving a little bit of stack space for handling");
					yield return MIR.SubInteger(x86_64_Names.RSP, 64, (ulong)classed.eightBytes.Length * 8);

					// spill it back onto the stack
					for (int i = 0; i < classed.eightBytes.Length; i++) {
						yield return MIR.Comment($"spill {INT_RET_REGS[i]}");
						yield return MIR.MovRegisterIndirect(x86_64_Names.RSP, 64, INT_RET_REGS[i], i * 8);
					}

					// pick it back up
					var flattened = Flatten(resultReg).ToArray();
					int flattenedIdx = 0;
					ulong offset = 0;
					foreach (var eb in classed.eightBytes) {
						ulong internalOffset = 0;
						foreach (var t in eb.fieldTypes) {
							var r = flattened[flattenedIdx++];
							var rName = allocContext.GetName(r);
							if (rName == null) throw new Exception($"Register {r} not named");
							yield return MIR.Comment($"Picking up {rName.Value.Item2} [{r}] from stack");
							yield return MIR.MovIndirectRegister(rName.Value.Item2, BitSize(t), x86_64_Names.RSP, (long)(offset + internalOffset));
							internalOffset += (ulong)(BitSize(t) / 8);
						}
						offset += 8;
					}

					yield return MIR.Comment($"Return data handled");
					yield return MIR.AddInteger(x86_64_Names.RSP, 64, (ulong)classed.eightBytes.Length * 8);
				}
			} break;
			default:
				throw new Exception($"CollectReturnValue({classed})");
		}
	}

	private IEnumerable<MIR> GenerateCall(IrInstr instr, Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		List<MIR> data = new();

		var addrReg = (instr[1] as IrParam.Register)!;
		var addrName = allocContext.GetName(addrReg);
		if (addrName == null) throw new Exception($"Register {addrReg} not named");
		Console.WriteLine(addrName.Value.Item2);

		var argRegs = instr.Params.Skip(2);

		var argTypes = ArgTypes((addrReg.Type as Apply)!);

		(ulong stackPassSize, bool retOnStack) = GetRequiredStackPassSize(argTypes.ret, argTypes.args);

		if (stackPassSize > 0) throw new Exception("I don't want to deal with stack-passing yet");

		ulong stackSpillSize = GetRequiredStackSpillSize(argTypes.args);
		data.Add(MIR.Comment($"Required stack size: {stackPassSize} for argument passing, {stackSpillSize} for argument spilling, return value passed [{(retOnStack ? "in memory" : "in registers")}]"));

		Console.WriteLine($"pass: {stackPassSize}, spill: {stackSpillSize}, return on stack: {retOnStack}");
		var regsUsedByReturn = GetRegsUsedForReturn(instr, ctx, lowererContext, allocContext);
		var regsToPreserve = allocContext.Names.Values.Select(pair => pair.Item2)
			.Where(name => caller_preserved.Contains(name))
			.Append(x86_64_Names.RBX).Distinct()
			.Where(name => !regsUsedByReturn.Contains(name));

		foreach (var retReg in regsUsedByReturn) {
			data.Add(MIR.Comment($"{retReg} is used by return, and therefore not preserved"));
		}
		foreach (var reg in regsToPreserve) {
			data.Add(MIR.Comment($"Preserving {reg}..."));
			data.Add(MIR.PushRegister(reg));
		}
		data.Add(MIR.Comment($"Reserving {stackPassSize} + {stackSpillSize} = {stackPassSize + stackSpillSize} bytes on stack..."));
		data.Add(MIR.SubInteger(x86_64_Names.RSP, 64, stackPassSize + stackSpillSize));

		// spilling registers into the spill-space
		data.AddRange(SpillRegisters(argTypes, instr, ctx, lowererContext, allocContext));

		data.Add(MIR.Comment($"Moving call target into RBX"));
		// move call-target into RBX
		data.Add(MIR.MovRegister(x86_64_Names.RBX, 64, addrName.Value.Item2));

		// time to pick up the spilled memory into argument registers
		data.AddRange(PickupArguments(argTypes, retOnStack, stackSpillSize, instr, ctx, lowererContext, allocContext));

		data.Add(MIR.Comment($"Cleaning up the spill-zone ({stackSpillSize} bytes)"));
		data.Add(MIR.AddInteger(x86_64_Names.RSP, 64, stackSpillSize));

		data.Add(MIR.Comment($"Actual call"));
		data.Add(MIR.CallRegister(x86_64_Names.RBX));

		// let's collect the result to our target register(s)
		data.AddRange(CollectReturnValue(argTypes, instr[0]!, retOnStack, ctx, lowererContext, allocContext));

		// clear pass space
		data.Add(MIR.Comment($"Cleaning up the pass-zone ({stackPassSize} bytes)"));
		data.Add(MIR.AddInteger(x86_64_Names.RSP, 64, stackPassSize));

		// pick back up the registers
		foreach (var reg in regsToPreserve.Reverse()) {
			data.Add(MIR.Comment($"Restoring {reg}..."));
			data.Add(MIR.PopRegister(reg));
		}

		return data;
	}

	private Typ GetParamType(IrParam p) {
		switch (p) {
			case IrParam.CompositeRegister c:
				return c.Type;
			case IrParam.Register r:
				return r.Type;
			default:
				throw new Exception($"TODO! GetParamType({p})");
		}
	}

	private IEnumerable<MIR> SpillEntryParameters(bool retOnStack, (Typ ret, IEnumerable<Typ> args) argTypes, IrInstr instr, Context ctx,
			VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext)
	{
		int intArgRegsIdx = retOnStack ? 1 : 0;

		var paramRegs = instr.Params.Skip(1);
		ulong srcOffset = 0;
		ulong dstOffset = 0;
		foreach (var (type, reg) in argTypes.args.Zip(paramRegs)) {
			var classed = SysV_Classify(type);
			int eightBytesIn = 0;
			foreach (var eb in classed.eightBytes) {
				switch (classed.Item1) {
					case SysV_Classes.INTEGER:
						if (intArgRegsIdx + classed.eightBytes.Length - eightBytesIn < INT_ARG_REGS.Length) {
							yield return MIR.Comment($"Spilling INTEGER argument eight-byte from {INT_ARG_REGS[intArgRegsIdx]}");
							yield return MIR.MovRegisterIndirect(x86_64_Names.RSP, 64, INT_ARG_REGS[intArgRegsIdx++], (long)dstOffset);
						} else {
							throw new Exception("stack-passing");
						}
						break;
					default:
						throw new Exception($"PickupArguments({classed})");
				}
				eightBytesIn++;
				dstOffset += 8;
			}
		}
	}

	private IEnumerable<MIR> PickupEntryParameters(
			(Typ ret, IEnumerable<Typ> args) argTypes, IrInstr instr,
			Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext)
	{
		var paramRegs = instr.Params.Skip(1);
		ulong offset = 0;
		foreach (var (type, reg) in argTypes.args.Zip(paramRegs)) {
			var classed = SysV_Classify(type);
			var flattened = Flatten(reg).ToArray();
			int flattenedIdx = 0;
			foreach (var eb in classed.eightBytes) {
				ulong internalOffset = 0;
				foreach (var t in eb.fieldTypes) {
					var r = flattened[flattenedIdx++];
					var rName = allocContext.GetName(r);
					if (rName == null) throw new Exception($"Register {r} not named");
					yield return MIR.Comment($"Picking up {rName.Value.Item2} [{r}] from stack");
					yield return MIR.MovIndirectRegister(rName.Value.Item2, BitSize(t), x86_64_Names.RSP, (long)(offset + internalOffset));
					internalOffset += (ulong)(BitSize(t) / 8);
				}
				offset += 8;
			}
		}
	}

	private IEnumerable<MIR> GenerateLoadArguments(IrInstr instr, Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		List<MIR> data = new();
		data.Add(MIR.Preamble());

		var argRegs = instr.Params.Skip(1);

		var argTypes = ArgTypes(((instr[0] as IrParam.IrType)!.Type as Apply)!);
		Console.WriteLine(argTypes.Item2.Count());

		(_, bool retOnStack) = GetRequiredStackPassSize(argTypes.ret, argTypes.args);

		ulong stackSpillSize = GetRequiredStackSpillSize(argTypes.args);
		data.Add(MIR.Comment($"Reserving {stackSpillSize} bytes on stack..."));
		data.Add(MIR.SubInteger(x86_64_Names.RSP, 64, stackSpillSize));

		data.Add(MIR.Comment($"Spilling arguments onto stack..."));
		data.AddRange(SpillEntryParameters(retOnStack, argTypes, instr, ctx, lowererContext, allocContext));

		data.Add(MIR.Comment($"Picking up arguments from stack..."));
		data.AddRange(PickupEntryParameters(argTypes, instr, ctx, lowererContext, allocContext));

		data.Add(MIR.Comment($"Cleaning up {stackSpillSize} bytes"));
		data.Add(MIR.AddInteger(x86_64_Names.RSP, 64, stackSpillSize));

		return data;
	}

	private IEnumerable<MIR> SpillReturnRegisters(
			Typ type, IrInstr instr,
			Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext)
	{
		var reg = instr[0];
		ulong offset = 0;
		var classed = SysV_Classify(type);
		var flattened = Flatten(reg).ToArray();
		int flattenedIdx = 0;
		foreach (var eb in classed.eightBytes) {
			ulong internalOffset = 0;
			foreach (var t in eb.fieldTypes) {
				var r = flattened[flattenedIdx++];
				var rName = allocContext.GetName(r);
				if (rName == null) throw new Exception($"Register {r} not named");
				yield return MIR.Comment($"Spilling {rName.Value.Item2} [{r}] onto stack");
				yield return MIR.MovRegisterIndirect(x86_64_Names.RSP, BitSize(t), rName.Value.Item2, (long)(offset + internalOffset));
				internalOffset += (ulong)(BitSize(t) / 8);
			}
			offset += 8;
		}
	}

	private IEnumerable<MIR> PickupReturnRegisters(
			Typ type, bool retOnStack, IrInstr instr,
			Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext)
	{
		int intRetRegsIdx = 0;

		var reg = instr[0];
		ulong srcOffset = 0;
		ulong dstOffset = 0;
		var classed = SysV_Classify(type);
		int eightBytesIn = 0;
		foreach (var eb in classed.eightBytes) {
			switch (classed.Item1) {
				case SysV_Classes.INTEGER:
					if (retOnStack) {
						throw new Exception("stack-passing");
					} else {
						yield return MIR.Comment($"Picking up INTEGER return value eight-byte into {INT_RET_REGS[intRetRegsIdx]}");
						yield return MIR.MovIndirectRegister(INT_RET_REGS[intRetRegsIdx++], 64, x86_64_Names.RSP, (long)srcOffset);
					}
					break;
				default:
					throw new Exception($"PickupArguments({classed})");
			}
			eightBytesIn++;
			srcOffset += 8;
		}
	}

	private IEnumerable<MIR> GenerateReturn(IrInstr instr, Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		List<MIR> data = new();
		Typ type = GetParamType(instr[0]!);
		var classed = SysV_Classify(type);
		ulong stackSpillSize = (ulong)classed.eightBytes.Length * 8;

		data.Add(MIR.Comment($"Reserving {stackSpillSize} bytes on stack..."));
		data.Add(MIR.SubInteger(x86_64_Names.RSP, 64, stackSpillSize));

		data.AddRange(SpillReturnRegisters(type, instr, ctx, lowererContext, allocContext));
		data.AddRange(PickupReturnRegisters(type, classed.eightBytes.Length > 2, instr, ctx, lowererContext, allocContext));

		data.Add(MIR.Comment($"Clearing up {stackSpillSize} bytes"));
		data.Add(MIR.AddInteger(x86_64_Names.RSP, 64, stackSpillSize));

		data.Add(MIR.Return);
		return data;
		throw new Exception("TODO!");
					var reg = (instr[0] as IrParam.Register)!;
					var regName = allocContext.GetName(reg);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, src) = regName.Value;
					data.Add(MIR.MovRegister(x86_64_Names.RAX, BitSize(reg.Type), src));
					foreach (var pair in allocContext.Names.Values.Distinct().Where(pair => callee_preserved.Contains(pair.Item2))) {
						data.Add(MIR.Comment($"Restore: {pair.Item2}"));
						data.Add(MIR.PopRegister(pair.Item2));
					}
					data.Add(MIR.Return);
	}

	private ReadOnlyCollection<MIR> GetMIR(IrBlock block, Context ctx, VSDRLA_Context lowererContext, RegisterAllocation.Context<x86_64_Classes, x86_64_Names> allocContext) {
		if (ctx.Visited(block)) return new List<MIR>().AsReadOnly();
		ctx.Visit(block);
		List<MIR> data = new();
		data.Add(MIR.Label($".block_{block.ID}"));
		foreach (var instr in block.Instructions) {
			data.Add(MIR.Comment($"{instr}"));
			switch (instr.Kind) {
				case IrKind.Call:
					data.AddRange(GenerateCall(instr, ctx, lowererContext, allocContext));
					break;
				case IrKind.LoadArguments:
					data.AddRange(GenerateLoadArguments(instr, ctx, lowererContext, allocContext));
					break;
				case IrKind.Return:
					data.AddRange(GenerateReturn(instr, ctx, lowererContext, allocContext));
					break;

				// handled in branches
				case IrKind.Phi: continue;

				case IrKind.BranchBool: {
					var reg = (instr[0] as IrParam.Register)!;
					var regName = allocContext.GetName(reg);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, src) = regName.Value;
					var trueBlock = (instr[2] as IrParam.Block)!.Blk;
					var falseBlock = (instr[1] as IrParam.Block)!.Blk;
					data.Add(MIR.TestRegister(src, 8, src));
					data.Add(MIR.JumpNE($".block_{block.ID}_branchbool_true"));
					// copies for false block, if needed
					HandlePhiCopies(data, block, falseBlock, allocContext);
					data.Add(MIR.Jump($".block_{falseBlock.ID}"));
					data.Add(MIR.Label($".block_{block.ID}_branchbool_true"));
					// copies for true block, if needed
					HandlePhiCopies(data, block, trueBlock, allocContext);
					data.Add(MIR.Jump($".block_{trueBlock.ID}"));
					data.AddRange(GetMIR(trueBlock, ctx, lowererContext, allocContext));
					data.AddRange(GetMIR(falseBlock, ctx, lowererContext, allocContext));
				} break;
				case IrKind.BranchAlways: {
					var targetBlock = (instr[0] as IrParam.Block)!.Blk;
					// copies for target block, if needed
					HandlePhiCopies(data, block, targetBlock, allocContext);
					data.Add(MIR.Jump($".block_{targetBlock.ID}"));
					data.AddRange(GetMIR(targetBlock, ctx, lowererContext, allocContext));
				} break;
				case IrKind.LoadFunction: {
					var reg = (instr[0] as IrParam.Register)!;
					var regName = allocContext.GetName(reg);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, dest) = regName.Value;
					data.Add(MIR.MovLabel(dest, BitSize(reg.Type), (instr[1] as IrParam.Function)!.Name));
				} break;
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
									data.Add(MIR.MovInteger(dest, BitSize(reg.Type), i.Value));
								} break;
								case IrParam.Bool b: {
									data.Add(MIR.MovInteger(dest, 8, b.Value ? 1ul : 0ul));
								} break;
								case IrParam.Register r: {
									regName = allocContext.GetName(r);
									if (regName == null) throw new Exception($"Register {r} not named");
									var (_, srcReg) = regName.Value;
									if (srcReg == dest) break; // noop
									data.Add(MIR.MovRegister(dest, BitSize(reg.Type), srcReg));
								} break;
								default:
									throw new Exception($"GetMIR(Copy.Src: {src}) not implemented yet");
							}
						} break;
						default:
							throw new Exception($"GetMIR(Copy.Dest: {regClass}) not implemented yet");
					}
				} break;
				case IrKind.Intrinsic: {
					var regName = allocContext.GetName((instr[0] as IrParam.Register)!);
					if (regName == null) throw new Exception($"Register {instr[0]} not named");
					var (regClass, dest) = regName.Value;
					var intrinsicOp = (instr[1] as IrParam.IntrinsicOp)!;
					switch (intrinsicOp.Kind) {
						case IrOpKind.NotEq: {
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
											data.Add(MIR.CmpRegister(src1Reg, BitSize(r.Type), src2Reg));
											data.Add(MIR.SetNE(dest));
										} break;
									default:
										throw new Exception($"{regClass} not-eq not implemented yet");
								}
							} break;
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
												data.Add(MIR.AddRegister(dest, BitSize(r.Type), src2Reg));
											} else if (src2Reg == dest) {
												data.Add(MIR.AddRegister(dest, BitSize(r.Type), src1Reg));
											} else {
												int bitCount = BitSize(r.Type);
												data.Add(MIR.PushRegister(src1Reg));
												data.Add(MIR.AddRegister(src1Reg, bitCount, src2Reg));
												data.Add(MIR.MovRegister(dest, bitCount, src1Reg));
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
												data.Add(MIR.SubRegister(dest, BitSize(r.Type), src2Reg));
											} else {
												int bitCount = BitSize(r.Type);
												data.Add(MIR.PushRegister(src1Reg));
												data.Add(MIR.SubRegister(src1Reg, bitCount, src2Reg));
												data.Add(MIR.MovRegister(dest, bitCount, src1Reg));
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
												data.Add(MIR.IMulRegister(src1Reg, bitCount, src2Reg));
												data.Add(MIR.MovRegister(dest, bitCount, src1Reg));
												data.Add(MIR.PopRegister(src1Reg));
											} else {
												throw new Exception("unsigned int multiply not implemented yet");
											}
										} break;
									default:
										throw new Exception($"{regClass} multiply not implemented yet");
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

												data.Add(MIR.XorRegister(x86_64_Names.RDX, bitCount, x86_64_Names.RDX));
												if (src1Reg != x86_64_Names.RAX)
													data.Add(MIR.MovRegister(x86_64_Names.RAX, bitCount, src1Reg));
												data.Add(MIR.IDivRegister(bitCount, src2Reg));
												if (dest != x86_64_Names.RAX)
													data.Add(MIR.MovRegister(dest, bitCount, x86_64_Names.RAX));

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
						case IrOpKind.Modulo: {
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
												data.Add(MIR.PushRegister(x86_64_Names.RAX));
												if (dest != x86_64_Names.RDX)
													data.Add(MIR.PushRegister(x86_64_Names.RDX));

												data.Add(MIR.MovRegister(x86_64_Names.RAX, bitCount, src1Reg));
												data.Add(MIR.XorRegister(x86_64_Names.RDX, bitCount, x86_64_Names.RDX));
												data.Add(MIR.IDivRegister(bitCount, src2Reg));
												if (dest != x86_64_Names.RDX)
													data.Add(MIR.MovRegister(dest, bitCount, x86_64_Names.RDX));

												if (dest != x86_64_Names.RDX)
													data.Add(MIR.PopRegister(x86_64_Names.RDX));
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

		public static MIR Comment(string label_text) => new() {
			kind = Kind.Comment,
			strVal = label_text,
		};

		public static MIR Preamble() => new() {
			kind = Kind.Preamble,
		};

		public static MIR CallRegister(x86_64_Names tgt) => new() {
			kind = Kind.CallRegister,
			reg0Val = tgt,
		};

		public static MIR Return => new() {
			kind = Kind.Return,
		};

		public static MIR Jump(string label_text) => new() {
			kind = Kind.Jump,
			strVal = label_text,
		};

		public static MIR JumpE(string label_text) => new() {
			kind = Kind.JumpE,
			strVal = label_text,
		};

		public static MIR JumpNE(string label_text) => new() {
			kind = Kind.JumpNE,
			strVal = label_text,
		};

		public static MIR MovInteger(x86_64_Names dest, int bitCount, ulong val) => new() {
			kind = Kind.MovInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			int1Val = val,
		};

		public static MIR CmoveInteger(x86_64_Names dest, int bitCount, ulong val) => new() {
			kind = Kind.CmoveInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			int1Val = val,
		};

		public static MIR MovRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.MovRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR MovRegisterIndirect(x86_64_Names dest, int bitCount, x86_64_Names src, long offset) => new() {
			kind = Kind.MovRegisterIndirect,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
			int1Val = (ulong)offset,
		};

		public static MIR MovIndirectRegister(x86_64_Names dest, int bitCount, x86_64_Names src, long offset) => new() {
			kind = Kind.MovIndirectRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
			int1Val = (ulong)offset,
		};

		public static MIR MovLabel(x86_64_Names dest, int bitCount, string label_text) => new() {
			kind = Kind.MovLabel,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			strVal = label_text,
		};

		public static MIR SetNE(x86_64_Names dest) => new() {
			kind = Kind.SetNE,
			reg0Val = dest,
		};

		public static MIR TestRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.TestRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR CmpRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.CmpRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR XorRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.XorRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR AddRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.AddRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR AddInteger(x86_64_Names dest, int bitCount, ulong val) => new() {
			kind = Kind.AddInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			int1Val = val,
		};

		public static MIR SubRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.SubRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR SubInteger(x86_64_Names dest, int bitCount, ulong val) => new() {
			kind = Kind.SubInteger,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			int1Val = val,
		};

		public static MIR IMulRegister(x86_64_Names dest, int bitCount, x86_64_Names src) => new() {
			kind = Kind.IMulRegister,
			reg0Val = dest,
			int0Val = (ulong)bitCount,
			reg1Val = src,
		};

		public static MIR IDivRegister(int bitCount, x86_64_Names src) => new() {
			kind = Kind.IDivRegister,
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
			Comment,

			Preamble,
			MovInteger,
			CmoveInteger,
			MovRegister,
			MovIndirectRegister,
			MovRegisterIndirect,
			MovLabel,

			SetNE,

			Jump,
			JumpE,
			JumpNE,

			TestRegister,
			CmpRegister,

			XorRegister,

			AddRegister,
			AddInteger,

			SubRegister,
			SubInteger,

			IMulRegister,

			IDivRegister,

			PushRegister,
			PopRegister,

			CallRegister,
			Return,
		}

		public x86_64_Names reg0Val;
		public x86_64_Names reg1Val;
		public string strVal;
		public ulong int0Val;
		public ulong int1Val;
	}

	class x86_64_Registers : IRegisterSet<x86_64_Classes, x86_64_Names> {
		public x86_64_Classes[] Classes() => new[] {x86_64_Classes.Integer};

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
				default:
					throw new Exception($"Cannot get ordered register for class {c} yet");
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
		R8,
		R9,
		R10,
		R11,
		R12,
		R13,
		R14,
		R15,
		// Special Purpose
		RBP,
		RSP,
	}
}
