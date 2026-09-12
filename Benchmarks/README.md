# Benchmarks

Run the connection update benchmarks in Release mode:

```sh
dotnet run -c Release --project Benchmarks -- --filter '*ConnectionUpdates*'
```

`ConnectionUpdates` measures edits to folders of 100 or 1,000 documents with
eight cross-document references per document. Parsing and fixture construction
happen before measurement. Each invocation starts from the same folder; timings
include lookup maintenance, symbol differences, and connection updates.

Run the diagnostics calculation benchmark with:

```sh
dotnet run -c Release --project Benchmarks -- --filter '*DiagnosticUpdates*'
```

`DiagnosticUpdates` uses the same folder sizes and eight-reference document
shape, plus one broken link per document. `Calculate` measures initial
calculation and updates after a prose edit, link edit, or target-heading edit;
`FindAffectedDocuments` measures the time spent finding documents that need
diagnostics recalculated after an edit.
Parsing, folder updates, and the prior diagnostics results are prepared in
setup. The initial `FindAffectedDocuments` case has no previous workspace state
to compare.

An earlier single-run measurement of the pre-cleanup dependency graph at 1,000
documents took about 13 µs for an unlinked title rename, 77 µs for a linked
title rename, and 266 µs for document removal; a full rebuild took about 297 ms.
These are indicative figures, not measurements of the final working tree. Run
`dotnet test` for correctness and rerun the benchmark for current performance.
