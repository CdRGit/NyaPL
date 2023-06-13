using System;
using System.Collections.Generic;

using Nyapl.Parsing.Tree;

namespace Nyapl.FlowAnalysis;

public class FlowAnalyzer {
	private IEnumerable<FlowError> Analyze(Context ctx, AstListNode<StatementNode> body) {
		bool reportedDeadCode = false;

		foreach (var statement in body) {
			if (!reportedDeadCode && statement is not FunctionNode && ctx.Returns) {
				// report dead code (this may move to a different phase of the compiler later on)
				yield return new FlowError(statement.Location, $"Dead code");
				reportedDeadCode = true;
			}
			// for now I can just analyze all the statements in here, most do not require a lot of logic
			switch (statement) {
				// does not impact control flow but does need to be recursively checked
				case FunctionNode function:
					bool returnsCopy = ctx.Returns;
					foreach (var error in Analyze(ctx, function)) yield return error;
					ctx.Returns = returnsCopy;
					break;

				case ReturnStatementNode @return:
					ctx.Returns = true;
					break;

				// trivial, no impact on control flow
				case DeclareVarNode:
					break;
				default:
					throw new Exception($"Analyze(ctx, {statement.GetType().Name}) not yet implemented");
			}
		}
		// done!
	}

	private IEnumerable<FlowError> Analyze(Context ctx, FunctionNode function) {
		ctx.Returns = false; // assume no paths return

		foreach (var error in Analyze(ctx, function.Body)) yield return error;

		// add a code path flow error to the list if not all paths return, so code path return errors can be reported in bulk
		if (!ctx.Returns) yield return new FlowError(function.Location, "Not all code paths return");
	}

	public FileNode Analyze(FileNode file) {
		var ctx = new Context();
		// keep track of multiple errors so I can bulk report them
		List<CompileError> errors = new();
		
		// the only things in a file with flow currently are Functions, which (currently) only have one flow path, will still write the code to theoretically support multiple flow paths, might need to rewrite parts of this
		foreach (var function in file.Functions) {
			errors.AddRange(Analyze(ctx, function));
		}

		if (errors.Count > 0) {
			throw new BundledError(errors.AsReadOnly());
		}

		return file;
	}

	private class Context {
		// in some cases not all paths having a return is fine, for instance for `if` (in the future) and other branching control flow having only some branches return is fine
		public bool Returns { get; set; }
	}
}
