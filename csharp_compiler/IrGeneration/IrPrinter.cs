using System;
using System.IO;
using System.Text;
using System.Linq;
using System.Diagnostics;
using System.Collections.Generic;

using Nyapl.Typing.Types;

namespace Nyapl.IrGeneration;

public static class IrPrinter {
	public static void DrawGraph(string funcName, IrBlock body, string? metaData = null) {
		string path = $"debug/flowgraph/{funcName.Replace("/", "-")}{(metaData == null ? "" : $"!{metaData}")}";
		using (StreamWriter writer = new($"{path}.dot")) {
			var explored = new HashSet<IrBlock>();
			writer.WriteLine("digraph {");

			writer.WriteLine(@"node [shape=""box""]");

			writer.WriteLine(@$"entry [label=""{funcName}"", shape=""Mdiamond""]");
			writer.WriteLine(@$"return [label=""return"", shape=""octagon""]");

			writer.WriteLine($"entry -> n{body.ID}");

			DrawGraph(writer, body, explored);

			writer.WriteLine("}");
		}
		// use `$ dot` to draw it
		Process.Start("dot", new[] {"-Tsvg", $"{path}.dot", $"-o{path}.svg"}).WaitForExit();
		Process.Start("dot", new[] {"-Tpng", $"{path}.dot", $"-o{path}.png"}).WaitForExit();
	}

	private static void DrawGraph(StreamWriter writer, IrBlock node, HashSet<IrBlock> explored) {
		if (explored.Contains(node)) return; // early return

		explored.Add(node);
		StringBuilder labelText = new();

		foreach (var instr in node.Instructions) {
			PrettyPrint(labelText, instr);
		}

		labelText.Replace("\n", "\\n");

		writer.WriteLine(@$"n{node.ID} [label=""{labelText.ToString()}"",xlabel=""{node.ID}""{(node.HasReturn ? @", shape=""box""" : "")}]");
		if (node.HasReturn) {
			writer.WriteLine($"n{node.ID} -> return");
		}

		foreach (var target in node.Outgoing) {
			DrawGraph(writer, target.node, explored);
			writer.WriteLine(@$"n{node.ID} -> n{target.node.ID} [label=""{target.label}""]");
		}

		StringBuilder localText = new();
		if (node.Locals.Any()) {
			foreach (var local in node.Locals) {
				localText.Append($"{local.Key}: ");
				PrettyPrint(localText, local.Value);
				localText.Append("\n");
			}
			writer.WriteLine(@$"n_locals{node.ID} [label=""{localText.ToString()}"",shape=""underline""]");
			writer.WriteLine(@$"n{node.ID} -> n_locals{node.ID} [style=""dashed"",arrowhead=""onormal""]");
		}
	}

	private static void PrettyPrint(StringBuilder builder, IrInstr instr) {
		switch (instr.Kind) {
			case IrKind.MachineSpecific:
				builder.Append("MACHINE SPECIFIC [");
				for (int i = 0; i < instr.Params.Length; i++)
				{
					if (i != 0) builder.Append(", ");
					PrettyPrint(builder, instr[i]);
				}
				builder.Append("]\n");
				break;
			case IrKind.Phi:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- Φ [");
				for (int i = 1; i < instr.Params.Length; i += 2) {
					builder.Append(i == 1 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
					builder.Append(": ");
					PrettyPrint(builder, instr[i+1]!);
				}
				builder.Append("]\n");
				break;
			case IrKind.BranchAlways:
				builder.Append("br\n");
				break;
			case IrKind.BranchBool:
				builder.Append("br ");
				PrettyPrint(builder, instr[0]!);
				builder.Append("? ");
				PrettyPrint(builder, instr[2]!);
				builder.Append(" : ");
				PrettyPrint(builder, instr[1]!);
				builder.Append("\n");
				break;
			case IrKind.LoadFunction:
			case IrKind.Copy:
			case IrKind.StoreLocal:
			case IrKind.LoadLocal:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!);
				builder.Append("\n");
				break;
			case IrKind.IntrinsicImpure:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!);
				builder.Append("[impure](");
				for (int i = 2; i < instr.Params.Length; i ++) {
					builder.Append(i == 2 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
				}
				builder.Append(")\n");
				break;
			case IrKind.CallImpure:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!);
				builder.Append("[impure](");
				for (int i = 2; i < instr.Params.Length; i ++) {
					builder.Append(i == 2 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
				}
				builder.Append(")\n");
				break;
			case IrKind.Call:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!);
				builder.Append("(");
				for (int i = 2; i < instr.Params.Length; i ++) {
					builder.Append(i == 2 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
				}
				builder.Append(")\n");
				break;
			case IrKind.Return:
				builder.Append("return ");
				PrettyPrint(builder, instr[0]!);
				builder.Append("\n");
				break;
			case IrKind.LoadTupleSection:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!);
				builder.Append($".offset(");
				PrettyPrint(builder, instr[2]!);
				builder.Append(")\n");
				break;
			case IrKind.CreateTuple:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- (");
				for (int i = 1; i < instr.Params.Length; i += 2) {
					builder.Append(i == 1 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
					builder.Append(": ");
					PrettyPrint(builder, instr[i+1]!);
				}
				builder.Append(")\n");
				break;
			case IrKind.Intrinsic:
				PrettyPrint(builder, instr[0]!);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!);
				builder.Append("(");
				for (int i = 2; i < instr.Params.Length; i++) {
					builder.Append(i == 2 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
				}
				builder.Append(")\n");
				break;
			case IrKind.LoadArguments:
				builder.Append("load arguments: (");
				for (int i = 0; i < instr.Params.Length; i++) {
					builder.Append(i == 0 ? "" : ", ");
					PrettyPrint(builder, instr[i]!);
				}
				builder.Append(")\n");
				break;
			default:
				throw new Exception($"PrettyPrint not implemented yet for IrInstr of kind {instr.Kind}");
		}
	}

	readonly static Dictionary<IrOpKind, string> intrinsicOps = new() {
		{IrOpKind.Multiply, "multiply"},
		{IrOpKind.Divide,   "divide"},
		{IrOpKind.Modulo,   "modulo"},

		{IrOpKind.Add,      "add"},
		{IrOpKind.Subtract, "subtract"},

		{IrOpKind.Equal,   "equal"},
		{IrOpKind.NotEq,   "not_equal"},
	};
	private static void PrettyPrint(StringBuilder builder, IrParam param) {
		if (param.GetType().IsGenericType) {
			builder.Append($"{param}");
			return;
		}
		switch (param) {
			case IrParam.Block b:
				builder.Append($"block.{b.Blk.ID}");
				break;
			case IrParam.Local l:
				builder.Append($"`local[{l.Name}]: ");
				PrettyPrint(builder, l.Type);
				builder.Append($"`");
				break;
			case IrParam.Int i:
				builder.Append($"{i.Value}");
				break;
			case IrParam.Bool b:
				builder.Append($"{b.Value}");
				break;
			case IrParam.IrType t:
				builder.Append($":");
				PrettyPrint(builder, t.Type);
				break;
			case IrParam.CompositeRegister c:
				builder.Append("composite[");
				PrettyPrint(builder, c.Registers[0]);
				foreach (var r in c.Registers.Skip(1)) {
					builder.Append(", ");
					PrettyPrint(builder, r);
				}
				builder.Append("]");
				break;
			case IrParam.Register r:
				builder.Append($"`r{r.Index:D2}: ");
				PrettyPrint(builder, r.Type);
				builder.Append($"`");
				break;
			case IrParam.Function f:
				builder.Append($"{f.Name}");
				break;
			case IrParam.Intrinsic i:
				builder.Append($"intrinsic:{i.Name}");
				break;
			case IrParam.Count c:
				builder.Append($"{c.Value}");
				break;
			case IrParam.Offset o:
				builder.Append($"{o.Value}");
				break;
			case IrParam.IntrinsicOp i:
				builder.Append($"intrinsic:{intrinsicOps[i.Kind]}");
				break;
			default:
				throw new Exception($"PrettyPrint not implemented yet for IrParam of type {param.GetType().Name}");
		}
	}

	private static void PrettyPrint(StringBuilder builder, Typ type) {
		switch (type) {
			case Intrinsic i: {
					switch (i.Type) {
						case IntrinsicType.I32:
							builder.Append("i32");
							break;
						case IntrinsicType.Bool:
							builder.Append("bool");
							break;
						default:
							throw new Exception($"PrettyPrint(IntrinsicType.{i.Type}) not implemented yet");
					}
				} break;
			case Apply a: {
					switch (a.BaseType) {
						case Intrinsic i:
							switch (i.Type) {
								case IntrinsicType.Tuple: {
									builder.Append("(");
									bool firstIteration = true;
									foreach (var param in a.ParameterTypes) {
										if (!firstIteration)
											builder.Append(", ");
										PrettyPrint(builder, param);
										firstIteration = false;
									}
									builder.Append(")");
									} break;
								default:
									throw new Exception($"PrettyPrint(Apply.BaseType: IntrinsicType.{i.Type}) not implemented yet");
							}
							break;
						case Function f: {
							builder.Append("fn ");
							builder.Append($"[{string.Join(", ", f.Effects)}]");
							builder.Append("(");
							bool firstIteration = true;
							foreach (var param in a.ParameterTypes) {
								if (!firstIteration)
									builder.Append(", ");
								PrettyPrint(builder, param);
								firstIteration = false;
							}
							builder.Append(")");
							} break;
						default:
							throw new Exception($"PrettyPrint(Apply.BaseType: {a.BaseType}) not implemented yet");
					}
				} break;
			default:
				throw new Exception($"PrettyPrint({type}) not implemented yet");
		}
	}
}
