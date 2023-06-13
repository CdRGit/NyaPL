namespace Nyapl.FlowAnalysis;

public class FlowError : CompileError {
	public FlowError(SourceLoc location, string message) : base(location, message, "Flow Analysis") {

	}
}
