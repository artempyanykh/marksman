# Incremental diagnostics plan

Diagnostics are currently recalculated for every document in both the previous
and current state before publication. Reuse the last published diagnostics and
recalculate only documents affected by an edit. There is one calculation path:
when previous results are `None`, every current document needs calculation.

Track progress in order. Keep tests passing after each code change.

- [x] **1. Establish behavior with tests.** Cover initial calculation, unchanged
  results, broken links becoming valid or ambiguous after another document
  changes, ambiguous-link related locations after a target heading moves,
  ambiguity clearing when a target disappears, removal clearing diagnostics,
  re-opening a document, and comparison of the latest state after multiple
  changes. Assert diagnostic content and publication decisions, not just
  message text.
- [x] **2. Add a diagnostics benchmark and record a baseline.** Reuse the
  existing 100- and 1,000-document fixtures. Measure initial calculation and
  updates after an unrelated edit, a link edit, and a target-heading edit.
  Construct and parse fixtures outside the measured call, force diagnostic
  results to be evaluated, and measure time and allocations. Keep generated
  benchmark results out of the repository.
- [x] **3. Refactor without changing behavior.** Extract per-document
  diagnostic calculation, represent previous results as a keyed snapshot, and
  make the calculation and publication decision testable apart from the mailbox.
  Check that the behavior tests still pass.
- [x] **4. Derive affected documents from snapshots.** At publication time,
  compare the last published workspace with the latest one. Select directly
  changed and reopened documents, sources whose cached Conn resolutions differ,
  and references into edited targets in either graph. Select every document for
  added, removed, or reconfigured folders. Test comparison across coalesced
  edits. No per-edit change summary is needed in state mutations or hooks.
- [ ] **5. Implement incremental calculation and publication.** With previous
  results `None`, calculate every current document. Otherwise, reuse cached
  results, recalculate affected documents, remove results for deleted documents,
  and publish only changes or required re-open notifications. Do not retain a
  separate full-calculation routine.
- [ ] **6. Verify correctness and cost.** Run `dotnet test`. Assert that an
  unrelated document is not recalculated. Rerun the benchmark and compare time
  and allocations with the baseline, especially for unrelated edits and larger
  folders.

Follow-up review work for Conn, after incremental diagnostics is wired in:

- [ ] Measure the snapshot resolution scan on graph-changing edits. If it
  dominates diagnostic calculation, use a cheaper snapshot comparison. The
  resolved/unresolved graph scans in `Conn.difference` are currently for
  paranoid validation, not the normal update path.
- [ ] Give alias changes one explicit external-input invalidation API so a
  `Conn.update` caller cannot omit the affected aliases.
- [ ] Make definition-selection dependencies explicit or verify the mapping
  from changed definitions to selectors against what selection reads.
- [ ] Simplify Conn's three computation queues without losing their dependency
  order or incremental correctness.
- [ ] Share graph construction between Conn's compact formatting and
  difference routines.
- [ ] Centralize selector scope extraction and the repeated orphan-collection
  folds when working on those paths.

Baseline on 2026-09-12: BenchmarkDotNet 0.15.6, .NET 9.0.19, Linux x64,
Intel Core i7-12700K. The Release benchmark was run in-process with two warmup
and five measurement iterations at 200 ms each. Figures are mean time and
allocated memory per calculation; they are indicative short-run measurements.

| Scenario | 100 documents | 1,000 documents |
| --- | ---: | ---: |
| Initial | 3.6 ms / 5.9 MB | 46.8 ms / 63.9 MB |
| Prose edit | 7.0 ms / 11.7 MB | 91.9 ms / 126.2 MB |
| Link edit | 7.0 ms / 11.7 MB | 91.1 ms / 126.2 MB |
| Target-heading edit | 7.1 ms / 11.7 MB | 91.4 ms / 126.2 MB |

Repeat after building the Release benchmark project:

```sh
dotnet run -c Release --no-build --project Benchmarks -- \
  --filter '*DiagnosticUpdates*' -j Short \
  --warmupCount 2 --iterationCount 5 --iterationTime 200 --inProcess
```
