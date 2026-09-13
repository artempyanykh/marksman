# Incremental diagnostics plan

Diagnostics reuse the results calculated for the last published workspace state
and recalculate only documents affected by an edit. There is one calculation
path: when previous results are `None`, every current document needs calculation.

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
  diagnostic calculation, represent previous results as a keyed map, and
  make the calculation and publication decision testable apart from the mailbox.
  Check that the behavior tests still pass.
- [x] **4. Find affected documents from workspace states.** At publication time,
  compare the last published workspace with the latest one. Select directly
  changed and reopened documents, sources whose cached Conn resolutions differ,
  and references into edited targets in either graph. Select every document for
  added, removed, or reconfigured folders. Test comparison across coalesced
  edits. No per-edit change summary is needed in state mutations or hooks.
- [x] **5. Implement incremental calculation and publication.** With previous
  results `None`, calculate every current document. Otherwise, reuse cached
  results, recalculate affected documents, remove results for deleted documents,
  and publish only changes or required re-open notifications. Do not retain a
  separate full-calculation routine.
- [x] **6. Verify correctness and cost.** Run `dotnet test`. Assert that an
  unrelated document is not recalculated. Rerun the benchmark and compare time
  and allocations with the baseline, especially for unrelated edits and larger
  folders.

Follow-up review work after incremental diagnostics is wired in:

- [x] Reduce the cost of comparing documents in two folder states. Compare the
  ordered document maps directly, without building intermediate sets or adding
  a change summary to each state mutation. At 1,000 documents, finding affected
  documents after a prose edit fell from about 3.4 ms / 7.1 MB to
  1.06 ms / 2.43 MB. The comparison still visits every document.
- [x] Reduce the cost of comparing cached reference resolutions between the old
  and new Conn states. Compare their ordered reference computations in one pass
  instead of looking up each reference in the other map. At 1,000 documents,
  finding affected documents after a link edit fell from about 22.5 ms / 26 MB
  to 3.8 ms / 8.0 MB; a target-heading edit fell from about 23.3 ms / 26 MB to
  3.8 ms / 8.0 MB. The comparison still scans the reference computations.
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

After step 5, with the same short-run benchmark settings:

| Scenario | 100 documents | 1,000 documents |
| --- | ---: | ---: |
| Initial | 3.28 ms / 5.4 MB | 42.3 ms / 60.3 MB |
| Prose edit | 0.52 ms / 1.0 MB | 3.89 ms / 7.6 MB |
| Link edit | 1.98 ms / 2.8 MB | 25.1 ms / 30.2 MB |
| Target-heading edit | 1.97 ms / 2.8 MB | 24.8 ms / 30.2 MB |

After the document and reference-resolution comparison follow-ups, a fresh
end-to-end run for 1,000 documents measured:

| Scenario | Before incremental diagnostics | Current implementation |
| --- | ---: | ---: |
| Initial | 46.8 ms / 63.9 MB | 41.8 ms / 63.3 MB |
| Prose edit | 91.9 ms / 126.2 MB | 1.39 ms / 3.0 MB |
| Link edit | 91.1 ms / 126.2 MB | 4.53 ms / 8.6 MB |
| Target-heading edit | 91.4 ms / 126.2 MB | 4.52 ms / 8.6 MB |

Finding the documents that need diagnostics recalculated takes about
3.4 ms / 7.1 MB for the 1,000-document prose edit and 25 ms / 29.7 MB for link
and heading edits. The document diff takes about 2.8 ms / 6.1 MB; comparing
cached reference resolutions takes about 20.7 ms / 22.6 MB. These component
figures came from temporary microbenchmarks. `FindAffectedDocuments` remains in
the benchmark project for repeatable checks.

Repeat after building the Release benchmark project:

```sh
dotnet run -c Release --no-build --project Benchmarks -- \
  --filter '*DiagnosticUpdates*' -j Short \
  --warmupCount 2 --iterationCount 5 --iterationTime 200 --inProcess
```
