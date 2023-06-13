.PHONY: csharp_compiler

csharp_compiler:
	dotnet run --project csharp_compiler test.nya -s
