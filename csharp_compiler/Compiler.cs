using System;
using System.Text;
using System.Linq;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;

using Nyapl.Lexing;

using Nyapl.Parsing;
using Nyapl.Parsing.Tree;

using Nyapl.Localizing;

using Nyapl.Typing;

using Nyapl.FlowAnalysis;

using Nyapl.IrGeneration;
using Nyapl.IrTransformation;

namespace Nyapl;

public class Compiler {
	public Arguments Args { get; }

	private Lexer        lexer        = new();
	private Parser       parser       = new();
	private Localizer    localizer;
	private TypeChecker  typeChecker  = new();
	private FlowAnalyzer flowAnalyzer = new();
	private IrGenerator  irGenerator  = new();
	private Mem2Reg      mem2reg      = new();

	private List<string> sourceFiles = new();
	private Dictionary<string, string>             readFiles      = new();
	private Dictionary<string, TokenList>          lexedFiles     = new();
	private Dictionary<string, FileNode>           parsedFiles    = new();
	private Dictionary<string, LocalizedFileNode>  localizedFiles = new();
	private Dictionary<string, TypedFileNode>      typedFiles     = new();
	private Dictionary<string, TypedFileNode>      analyzedFiles  = new();
	private Dictionary<string, IrResult>           generatedFiles = new();
	private Dictionary<string, IrResult>           mem2regFiles   = new();

	private static T Memoize<T>(string file, Dictionary<string, T> memory, Func<string, T> generator) {
		if (!memory.ContainsKey(file)) memory[file] = generator(file);
		return memory[file];
	}

	public Compiler(Arguments args) {
		Args = args;
		localizer = new(args);
		sourceFiles.Add(args.File);
	}

	private void PrettyPrint(AstNode node, int depth = 0) {
		Console.WriteLine($"{new string(' ', depth * 2)}{node}");
		foreach (var child in node.GetChildren()) PrettyPrint(child, depth + 1);
	}

	private void PrettyPrint(StringBuilder builder, IrParam param, string[] functions, string[] intrinsics) {
		switch (param) {
			case IrParam.Block b:
				builder.Append($"block.{b.Blk.ID}");
				break;
			case IrParam.Local l:
				builder.Append($"local[{l.Name}:{l.Size:D2}]");
				break;
			case IrParam.Int i:
				builder.Append($"{i.Value}");
				break;
			case IrParam.Bool b:
				builder.Append($"{b.Value}");
				break;
			case IrParam.Register r:
				builder.Append($"r{r.Index:D2}:{r.Size:D2}");
				break;
			case IrParam.Function f:
				builder.Append($"{functions[f.Index]}");
				break;
			case IrParam.Intrinsic i:
				builder.Append($"intrinsic:{intrinsics[i.Index]}");
				break;
			case IrParam.Parameter p:
				builder.Append($"p{p.Index:D2}:{p.Size:D2}");
				break;
			case IrParam.Argument a:
				builder.Append($"a{a.Index:D2}:{a.Size:D2}");
				break;
			case IrParam.Count c:
				builder.Append($"{c.Value}");
				break;
			case IrParam.Offset o:
				builder.Append($"{o.Value}");
				break;
			default:
				throw new Exception($"PrettyPrint not implemented yet for IrParam of type {param.GetType().Name}");
		}
	}

	private void PrettyPrint(StringBuilder builder, IrInstr instr, string[] functions, string[] intrinsics) {
		Dictionary<IrInstr.IrKind, string> operators = new() {
			{IrInstr.IrKind.Subtract, "-"},
			{IrInstr.IrKind.Add,      "+"},
			{IrInstr.IrKind.Multiply, "*"},
			{IrInstr.IrKind.Modulo,   "%"},
			{IrInstr.IrKind.Equal,    "=="},
			{IrInstr.IrKind.NotEq,    "!="},
		};
		switch (instr.Kind) {
			case IrInstr.IrKind.Phi:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- Φ [");
				for (int i = 1; i < instr.Params.Length; i += 2) {
					builder.Append(i == 1 ? "" : ", ");
					PrettyPrint(builder, instr[i]!, functions, intrinsics);
					builder.Append(": ");
					PrettyPrint(builder, instr[i+1]!, functions, intrinsics);
				}
				builder.Append("]\n");
				break;
			case IrInstr.IrKind.BranchAlways:
				builder.Append("br\n");
				break;
			case IrInstr.IrKind.BranchBool:
				builder.Append("br ");
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append("?\n");
				break;
			case IrInstr.IrKind.EmptyTuple:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ()\n");
				break;
			case IrInstr.IrKind.IntLiteral:
			case IrInstr.IrKind.BoolLiteral:
			case IrInstr.IrKind.LoadFunction:
			case IrInstr.IrKind.LoadIntrinsic:
			case IrInstr.IrKind.Copy:
			case IrInstr.IrKind.StoreParam:
			case IrInstr.IrKind.LoadArgument:
			case IrInstr.IrKind.StoreLocal:
			case IrInstr.IrKind.LoadLocal:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!, functions, intrinsics);
				builder.Append("\n");
				break;
			case IrInstr.IrKind.CallImpure:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!, functions, intrinsics);
				builder.Append("[impure](");
				PrettyPrint(builder, instr[2]!, functions, intrinsics);
				builder.Append(")\n");
				break;
			case IrInstr.IrKind.Call:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!, functions, intrinsics);
				builder.Append("(");
				PrettyPrint(builder, instr[2]!, functions, intrinsics);
				builder.Append(")\n");
				break;
			case IrInstr.IrKind.Return:
				builder.Append("return ");
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append("\n");
				break;
			case IrInstr.IrKind.LoadTupleSection:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!, functions, intrinsics);
				builder.Append($".offset(");
				PrettyPrint(builder, instr[2]!, functions, intrinsics);
				builder.Append(")\n");
				break;
			case IrInstr.IrKind.AppendTupleSection:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!, functions, intrinsics);
				builder.Append($" ... ");
				PrettyPrint(builder, instr[2]!, functions, intrinsics);
				builder.Append("\n");
				break;
			case IrInstr.IrKind.Add:
			case IrInstr.IrKind.Subtract:
			case IrInstr.IrKind.Multiply:
			case IrInstr.IrKind.Modulo:
			case IrInstr.IrKind.Equal:
			case IrInstr.IrKind.NotEq:
				PrettyPrint(builder, instr[0]!, functions, intrinsics);
				builder.Append(" <- ");
				PrettyPrint(builder, instr[1]!, functions, intrinsics);
				builder.Append($" {operators[instr.Kind]} ");
				PrettyPrint(builder, instr[2]!, functions, intrinsics);
				builder.Append("\n");
				break;
			default:
				throw new Exception($"PrettyPrint not implemented yet for IrInstr of kind {instr.Kind}");
		}
	}

	private void DrawGraph(StreamWriter writer, IrBlock node, string[] functions, string[] intrinsics, HashSet<IrBlock> explored) {
		if (explored.Contains(node)) return; // early return

		explored.Add(node);
		StringBuilder labelText = new();

		foreach (var instr in node.Instructions) {
			PrettyPrint(labelText, instr, functions, intrinsics);
		}

		labelText.Replace("\n", "\\n");

		writer.WriteLine(@$"n{node.ID} [label=""{labelText.ToString()}"",xlabel=""{node.ID}""{(node.HasReturn ? @", shape=""Msquare""" : "")}]");
		if (node.HasReturn) {
			writer.WriteLine($"n{node.ID} -> return");
		}

		foreach (var target in node.Outgoing) {
			DrawGraph(writer, target.node, functions, intrinsics, explored);
			writer.WriteLine(@$"n{node.ID} -> n{target.node.ID} [label=""{target.label}""]");
		}

		StringBuilder localText = new();
		if (node.Locals.Any()) {
			foreach (var local in node.Locals) {
				localText.Append($"{local.Key}: ");
				PrettyPrint(localText, local.Value, functions, intrinsics);
				localText.Append("\n");
			}
			writer.WriteLine(@$"n_locals{node.ID} [label=""{localText.ToString()}"",shape=""underline""]");
			writer.WriteLine(@$"n{node.ID} -> n_locals{node.ID} [style=""dashed"",arrowhead=""onormal""]");
		}
		if (node.Frontier.Any()) {
			var frontierText = string.Join(", ", node.Frontier.Select(b => b.ID));
			writer.WriteLine(@$"n_frontier{node.ID} [label=""{frontierText.ToString()}"",shape=""insulator""]");
			writer.WriteLine(@$"n{node.ID} -> n_frontier{node.ID} [style=""dashed"",arrowhead=""onormal""]");
		}
	}

	private void DrawGraph(string funcName, IrBlock body, string[] functions, string[] intrinsics) {
		string path = $"debug/flowgraph/{funcName}";
		using (StreamWriter writer = new($"{path}.dot")) {
			var explored = new HashSet<IrBlock>();
			writer.WriteLine("digraph {");

			writer.WriteLine(@"node [shape=""box""]");

			writer.WriteLine(@$"entry [label=""{funcName}"", shape=""Mdiamond""]");
			writer.WriteLine(@$"return [label=""return"", shape=""octagon""]");

			writer.WriteLine($"entry -> n{body.ID}");

			DrawGraph(writer, body, functions, intrinsics, explored);

			writer.WriteLine("}");
		}
		// use `$ dot` to draw it
		if (Args.DrawGraphs) {
			Process.Start("dot", new[] {"-Tsvg", $"{path}.dot", $"-o{path}.svg"});
			Process.Start("dot", new[] {"-Tpng", $"{path}.dot", $"-o{path}.png"});
			//throw new Exception("Actually draw graphs out");
		}
	}

	private void PrettyPrint(string file) {
		TokenList tokens = GetTokens(file);
		foreach (var token in tokens) Console.WriteLine(token);

		TypedFileNode AST = GetAnalyzedAST(file);
		PrettyPrint(AST);

		IrResult instructions = GetMem2RegIR(file);
		var functions = AST.Functions.Select(f => f.Name).ToArray();
		var intrinsics = AST.Platform.Intrinsics.Select(i => i.Key).ToArray();
		foreach (var func in instructions.Functions.Keys) {
			DrawGraph(func, instructions.Functions[func], functions, intrinsics);
		}
	}

	private void ReportError(CompileError error, int contextSize) {
		if (error is BundledError bundled) {
			foreach(var err in bundled.Errors) {
				ReportError(err, bundled.Errors.Count <= 2 ? contextSize : 3);
			}
			return;
		}

		var loc = error.Location;
		Console.WriteLine();

		Console.ForegroundColor = ConsoleColor.Red;
		Console.WriteLine(error.Message);

		string text = GetText(loc.Path);
		string[] lines = text.Split("\n");
		string line = lines.Skip(loc.Line).First().Replace("\t", "    ");

		Console.ResetColor();
		Console.WriteLine(line);

		string indicator = new string(' ', loc.Col) + "^here";

		Console.ForegroundColor = ConsoleColor.Yellow;
		Console.WriteLine(indicator);

		Console.ForegroundColor = ConsoleColor.DarkGray;
		Console.WriteLine("...");
		Console.ResetColor();
		string[] preContext = lines.Skip(loc.Line - contextSize/2).Take(Math.Min(loc.Line, contextSize/2)).ToArray();
		foreach (var l in preContext)
			Console.WriteLine(l.Replace("\t", "    "));
		Console.ForegroundColor = ConsoleColor.Yellow;
		Console.WriteLine(line);
		Console.ResetColor();
		string[] postContext = lines.Skip(loc.Line + 1).Take(contextSize/2).ToArray();
		foreach (var l in postContext)
			Console.WriteLine(l.Replace("\t", "    "));
		Console.ForegroundColor = ConsoleColor.DarkGray;
		Console.WriteLine("...");

		Console.ResetColor();
	}

	public void Compile() {
		try
		{
			// pretty-print all found sourceFiles
			foreach (var file in sourceFiles) {
				PrettyPrint(file);
			}
		}
		catch (CompileError error) {
			ReportError(error, 9);
		}
	}

	public string GetText(string file) =>
		Memoize(file, readFiles, f => File.ReadAllText(f));

	public TokenList GetTokens(string file) =>
		Memoize(file, lexedFiles, f => lexer.Lex(Path.GetFileName(f), GetText(f)));

	public FileNode GetAST(string file) =>
		Memoize(file, parsedFiles, f => parser.Parse(GetTokens(f)));

	public LocalizedFileNode GetLocalizedAST(string file) =>
		Memoize(file, localizedFiles, f => localizer.Localize(GetAST(f)));

	public TypedFileNode GetTypedAST(string file) =>
		Memoize(file, typedFiles, f => typeChecker.Check(GetLocalizedAST(f)));

	public TypedFileNode GetAnalyzedAST(string file) =>
		Memoize(file, analyzedFiles, f => flowAnalyzer.Analyze(GetTypedAST(f)));

	public IrResult GetIR(string file) =>
		Memoize(file, generatedFiles, f => irGenerator.Generate(GetAnalyzedAST(f)));

	public IrResult GetMem2RegIR(string file) =>
		Memoize(file, mem2regFiles, f => mem2reg.Transform(GetIR(f)));
}
