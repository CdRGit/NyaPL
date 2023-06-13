namespace Nyapl.Parsing;

public class ParseError : CompileError {
	public ParseError(SourceLoc location, string message) : base(location, message, "Parsing") {

	}
}
