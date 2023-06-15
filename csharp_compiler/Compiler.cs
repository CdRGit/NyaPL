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
	private Dictionary<string, LocalizedFileNode>  typedFiles     = new();
	private Dictionary<string, LocalizedFileNode>  analyzedFiles  = new();
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

	private void PrettyPrint(string file) {
		TokenList tokens = GetTokens(file);
		foreach (var token in tokens) Console.WriteLine(token);

		LocalizedFileNode AST = GetAnalyzedAST(file);
		PrettyPrint(AST);

		IrList instructions = GetIR(file);
		foreach (var instr in instructions.Instructions) Console.WriteLine(instr);
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

	public LocalizedFileNode GetTypedAST(string file) =>
		Memoize(file, typedFiles, f => typeChecker.Check(GetLocalizedAST(f)));

	public LocalizedFileNode GetAnalyzedAST(string file) =>
		Memoize(file, analyzedFiles, f => flowAnalyzer.Analyze(GetTypedAST(f)));

	public IrList GetIR(string file) =>
		Memoize(file, generatedFiles, f => irGenerator.Generate(GetAnalyzedAST(f)));
}
