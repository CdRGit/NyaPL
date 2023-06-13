namespace Nyapl.Typing;

public class TypeError : CompileError {
	public TypeError(SourceLoc location, string message) : base(location, message, "Type") {

	}
}
