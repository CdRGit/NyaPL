using System;
using System.Collections.Generic;

using Nyapl.Parsing.Tree;

using Nyapl.Typing;

namespace Nyapl.FlowAnalysis;

public class FlowAnalyzer {
	private void Analyze(Context ctx, AstListNode<StatementNode> body) {
		bool reportedDeadCode = false;

		foreach (var statement in body) {
			if (!reportedDeadCode && statement is not FunctionNode && ctx.Returns) {
				// report dead code (this may move to a different phase of the compiler later on)
				ctx.Errors.Add(new FlowError(statement.Location, $"Dead code"));
				reportedDeadCode = true;
			}
			// for now I can just analyze all the statements in here, most do not require a lot of logic
			switch (statement) {
				// does not impact control flow but does need to be recursively checked
				case FunctionNode function: {
					bool returnsCopy = ctx.Returns;
					Analyze(ctx, function);
					ctx.Returns = returnsCopy;
				} break;
				// Oh Boy
				case IfStatementNode @if: {
					bool returnsCopy = ctx.Returns;
					bool allIfsReturn = @if.Else != null;
					ctx.Returns = false;
					Analyze(ctx, @if.IfBody);
					allIfsReturn &= ctx.Returns;
					foreach (var elif in @if.Elifs) {
						ctx.Returns = false;
						Analyze(ctx, elif.Body);
						allIfsReturn &= ctx.Returns;
					}
					if (@if.Else != null) {
						ctx.Returns = false;
						Analyze(ctx, @if.Else.Body);
						allIfsReturn &= ctx.Returns;
					}
					ctx.Returns = returnsCopy || allIfsReturn;
				} break;
				// easier than if but still requires some thinking
				case WhileStatementNode @while: {
					bool returnsCopy = ctx.Returns;
					ctx.Returns = false;
					Analyze(ctx, @while.Body);
					ctx.Returns = returnsCopy || ctx.Returns;
				} break;

				case ReturnStatementNode @return:
					ctx.Returns = true;
					break;

				case UnsafeStatementNode @unsafe:
					Analyze(ctx, @unsafe.Body);
					break;

				// trivial, no impact on control flow
				case DeclareVarNode:
				case ReassignNode:
				case DestructureNode:
				case NoopStatementNode: // this one literally does nothing
					break;
				default:
					throw new Exception($"Analyze(ctx, {statement.GetType().Name}) not yet implemented");
			}
		}
		// done!
	}

	private void Analyze(Context ctx, FunctionNode function) {
		ctx.Returns = false; // assume no paths return

		Analyze(ctx, function.Body);

		// add a code path flow error to the list if not all paths return, so code path return errors can be reported in bulk
		if (!ctx.Returns) ctx.Errors.Add(new FlowError(function.Location, "Not all code paths return"));
	}

	public TypedFileNode Analyze(TypedFileNode file) {
		var ctx = new Context();
		// the only things in a file with flow currently are Functions, which (currently) only have one flow path, will still write the code to theoretically support multiple flow paths, might need to rewrite parts of this
		foreach (var function in file.Functions) {
			Analyze(ctx, function);
		}

		// bulk report errors
		if (ctx.Errors.Count > 0) {
			throw new BundledError(ctx.Errors.AsReadOnly());
		}

		return file;
	}

	private class Context {
		// in some cases not all paths having a return is fine, for instance for `if` (in the future) and other branching control flow having only some branches return is fine, so we use a boolean rather than directly reporting errors
		public bool Returns { get; set; }
		// store a list of errors in context to allow seperate analysis to add errors without needing to pass those down
		public List<CompileError> Errors { get; set; } = new();
	}
}
