using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Nyapl.IrGeneration;

namespace Nyapl.CodeGen;

public class CodeGenLinux_x86_64 : ICodeGen {
	public void Generate(string filePath, ReadOnlyDictionary<string, IrBlock> functions) {
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
		Process.Start("nasm", new[] {"-felf64", $"{filePath}.asm"});
		Process.Start("ld", new[] {$"{filePath}.o"});
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
