using System;
using System.Linq;
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

namespace Nyapl;

public class Compiler {
	public Arguments Args { get; }

	private Lexer        lexer        = new();
	private Parser       parser       = new();
	private Localizer    localizer;
	private TypeChecker  typeChecker  = new();
	private FlowAnalyzer flowAnalyzer = new();
	private IrGenerator  irGenerator  = new();

	private List<string> sourceFiles = new();
	private Dictionary<string, string>             readFiles      = new();
	private Dictionary<string, TokenList>          lexedFiles     = new();
	private Dictionary<string, FileNode>           parsedFiles    = new();
	private Dictionary<string, LocalizedFileNode>  localizedFiles = new();
	private Dictionary<string, TypedFileNode>      typedFiles     = new();
	private Dictionary<string, TypedFileNode>      analyzedFiles  = new();
	private Dictionary<string, IrList>             generatedFiles = new();

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

	private void PrettyPrint(IrInstr instr, string[] functions, string[] intrinsics) {
		switch (instr.Kind) {
			case IrInstr.IrKind.JumpAlways: {
					var label = (instr[0] as IrParam.Label)!.Name;
					Console.WriteLine($"    jump_always {label}");
				}
				break;
			case IrInstr.IrKind.JumpIfFalse: {
					var r = (instr[1] as IrParam.Register)!;
					var idx = r.Index;
					var size = r.Size;
					var label = (instr[0] as IrParam.Label)!.Name;
					Console.WriteLine($"    jump_if_false(r{idx}:{size}) {label}");
				}
				break;
			case IrInstr.IrKind.LoadFunction: {
					var r = (instr[0] as IrParam.Register)!;
					var idx = r.Index;
					var size = r.Size;
					var func = (instr[1] as IrParam.Function)!.Index;
					Console.WriteLine($"    r{idx}:{size} = &{functions[func]}");
				}
				break;
			case IrInstr.IrKind.LoadIntrinsic: {
					var r = (instr[0] as IrParam.Register)!;
					var idx = r.Index;
					var size = r.Size;
					var i = (instr[1] as IrParam.Intrinsic)!.Index;
					Console.WriteLine($"    r{idx}:{size} = &intrinsic:{intrinsics[i]}");
				}
				break;
			case IrInstr.IrKind.Copy: {
					var rD = (instr[0] as IrParam.Register)!;
					var idxD = rD.Index;
					var sizeD = rD.Size;
					var rS = (instr[1] as IrParam.Register)!;
					var idxS = rS.Index;
					var sizeS = rS.Size;
					Console.WriteLine($"    r{idxD}:{sizeD} = r{idxS}:{sizeS}");
				}
				break;
			case IrInstr.IrKind.StoreParam: {
					var pD = (instr[0] as IrParam.Parameter)!;
					var idxD = pD.Index;
					var sizeD = pD.Size;
					var rS = (instr[1] as IrParam.Register)!;
					var idxS = rS.Index;
					var sizeS = rS.Size;
					Console.WriteLine($"    p{idxD}:{sizeD} = r{idxS}:{sizeS}");
				}
				break;
			case IrInstr.IrKind.LoadArgument: {
					var rD = (instr[0] as IrParam.Register)!;
					var idxD = rD.Index;
					var sizeD = rD.Size;
					var rS = (instr[1] as IrParam.Argument)!;
					var idxS = rS.Index;
					var sizeS = rS.Size;
					Console.WriteLine($"    r{idxD}:{sizeD} = a{idxS}:{sizeS}");
				}
				break;
			case IrInstr.IrKind.Call: {
					var rD = (instr[0] as IrParam.Register)!;
					var idxD = rD.Index;
					var sizeD = rD.Size;
					var rF = (instr[1] as IrParam.Register)!;
					var idxF = rF.Index;
					var sizeF = rF.Size;
					var count = (instr[2] as IrParam.Count)!.Value;
					Console.WriteLine($"    r{idxD}:{sizeD} = call r{idxF}:{sizeF}({count})");
				}
				break;
			case IrInstr.IrKind.Return: {
					var r = (instr[0] as IrParam.Register)!;
					var idx = r.Index;
					var size = r.Size;
					Console.WriteLine($"    return r{idx}:{size}");
				}
				break;
			case IrInstr.IrKind.IntLiteral: {
					var r = (instr[0] as IrParam.Register)!;
					var idx = r.Index;
					var size = r.Size;
					var value = (instr[1] as IrParam.Int)!.Value;
					Console.WriteLine($"    r{idx}:{size} = {value}");
				}
				break;
			case IrInstr.IrKind.BoolLiteral: {
					var r = (instr[0] as IrParam.Register)!;
					var idx = r.Index;
					var size = r.Size;
					var value = (instr[1] as IrParam.Bool)!.Value;
					Console.WriteLine($"    r{idx}:{size} = {value.ToString().ToLower()}");
				}
				break;
			case IrInstr.IrKind.Label:
				Console.WriteLine($"{(instr[0] as IrParam.Label)!.Name}:");
				break;
			default:
				throw new Exception($"PrettyPrint({instr.Kind}) not implemented yet '{instr}'");
		}
	}

	private void PrettyPrint(string file) {
		TokenList tokens = GetTokens(file);
		foreach (var token in tokens) Console.WriteLine(token);

		TypedFileNode AST = GetAnalyzedAST(file);
		PrettyPrint(AST);

		IrList instructions = GetIR(file);
		var functions = AST.Functions.Select(f => f.Name).ToArray();
		var intrinsics = AST.Platform.Intrinsics.Select(i => i.Key).ToArray();
		foreach (var instr in instructions.Instructions) {
			// loop over and pretty print
			PrettyPrint(instr, functions, intrinsics);
		}
		foreach (var pair in instructions.Functions) Console.WriteLine($"{pair.Key}: {pair.Value}");
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

	public IrList GetIR(string file) =>
		Memoize(file, generatedFiles, f => irGenerator.Generate(GetAnalyzedAST(f)));
}
