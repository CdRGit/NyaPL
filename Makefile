.PHONY: csharp_compiler
.PHONY: rust_bootstrap

rust_bootstrap:
	cd rust_bootstrap && cargo run

csharp_compiler:
	dotnet run --project csharp_compiler test.nya -d
