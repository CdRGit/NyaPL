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
		functions = RegisterLowering.LowerRegisters<VSDRLA_Context>(functions, VerySimpleDumbRegisterLoweringAlgorithm);

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
				var machineIR = GetMIR(func.Value);
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
				case MIR.Kind.Return:
					asmWriter.WriteLine("    mov rsp, rbp");
					asmWriter.WriteLine("    pop rbp");
					asmWriter.WriteLine($"    ret");
					break;
				case MIR.Kind.Push:
					asmWriter.WriteLine($"    push DWORD {instr.intVal}");
					break;
				case MIR.Kind.Peek:
					asmWriter.WriteLine($"    mov {instr.strVal}, [rsp+{instr.intVal}]");
					break;
				default:
					throw new Exception($"WriteMIR(MIR.Kind.{instr.kind}) not implemented yet");
			}
		}
	}

	private string MangleFunction(string functionName) {
		return functionName.Replace("/", "~");
	}

	private ReadOnlyCollection<MIR> GetMIR(IrBlock block) {
		var ctx = new Context();
		return GetMIR(block, ctx);
	}

	private ReadOnlyCollection<MIR> GetMIR(IrBlock block, Context ctx) {
		if (ctx.Visited(block)) return new List<MIR>().AsReadOnly();
		ctx.Visit(block);
		List<MIR> data = new();
		data.Add(MIR.Label($".block_{block.ID}"));
		foreach (var instr in block.Instructions) {
			switch (instr.Kind) {
				case IrKind.Copy:
					switch (instr[1]) {
						case IrParam.Int i:
							data.Add(MIR.Push((int)i.Value)); // this is very incorrect :)
							break;
						default:
							throw new Exception("cannot generate MIR for non-integer copy loads");
					}
					ctx.SetRegister((instr[0] as IrParam.Register)!, ctx.StackOffset);
					ctx.Push(4);
					break;
				case IrKind.LoadArguments:
					if (instr.Params.Length != 0) throw new Exception("cannot generate MIR for non-zero argument count functions");
					data.Add(MIR.Preamble());
					break;
				case IrKind.Return:
					data.Add(MIR.Peek(ctx.GetRegister((instr[0] as IrParam.Register)!), "rax"));
					data.Add(MIR.Return());
					break;
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

		public int StackOffset { get; private set; }
		public void Push(int offset) => StackOffset += offset;

		Dictionary<uint, int> registers = new();
		public void SetRegister(IrParam.Register reg, int offset) => registers[reg.Index] = offset;
		public int GetRegister(IrParam.Register reg) => registers[reg.Index];
	}

	struct MIR {
		public static MIR Label(string label_text) => new() {
			kind = Kind.Label,
			strVal = label_text,
		};

		public static MIR Preamble() => new() {
			kind = Kind.Preamble,
		};

		public static MIR Return() => new() {
			kind = Kind.Return,
		};

		public static MIR Push(int val) => new() {
			kind = Kind.Push,
			intVal = val,
		};

		public static MIR Peek(int offset, string reg) => new() {
			kind = Kind.Peek,
			intVal = offset,
			strVal = reg,
		};

		public Kind kind;
		public enum Kind {
			Label,
			Preamble,
			Return,
			Push,
			Peek,
		}

		public string strVal;
		public int intVal;
	}
}
