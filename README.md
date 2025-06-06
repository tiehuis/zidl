# zidl

An IDL Compiler written in Zig.

This is intended as an alternative to more mature projects such as WIDL, for use directly in
the zig compiler.

# Current status

~36.7% passing or near passing.

```
./check-dir mingw-w64/mingw-w64-headers/include/
Total processed:   316
Total Time:        25s 591ms
Skipped (no .h):   7
Minor diffs (<20): 116
Major diffs (≥20): 200
Crashes:           0
Total Diff Lines:  631681
```

# Compatibility

The goal of this compiler is to have output parity with WIDL for the mingw-64 header files. Anything
more is not intended to be supported at this stage.

# Usage

```
zig build
./zig-out/bin/zidl --help
```

```
Usage ./zig-out/bin/zidl: [options] file..

General options:
  -h, --help      Print this message.
  -v, --version   Print zidl version.

Compile options:
  --winrt                         Use midl3.0 (default midl2.0)
  --skip-imports                  Don't parse imported files
  --stacktrace                    Render a stacktrace on parse error
  --log-level debug|info|warn|err Print all log messages less than requested level
  -D<macro>=<value>               Define a macro for the C preprocessor

Debug options:
  --tokens                        Print a list of tokens produced by the tokenizer
  --codegen                       Perform codegen step (default: true)
  --show-failed-optional-parses   Render all parsing failures for optional rules
```

# License

MIT Licensed.

This is a clean-room implementation. The PEG grammar has created with help from
WIDL BNF grammar as a reference.
