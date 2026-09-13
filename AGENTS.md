# Coding guidelines

- Model the problem at the right level. Separate reusable mechanisms from domain-specific policy. Keep APIs, state, and benchmarks aligned with the scope and lifecycle of what they represent.
- Make change propagation coherent. Give each dependency or invalidation rule one clear owner. Avoid parallel, hand-maintained paths, and make it hard for callers to omit required information.
- Name things for what they are. Prefer domain terms over analogies or shorthand. Reconsider names across the whole change; use comments to explain decisions rather than unclear names.
- Make the smallest complete change. Reconcile new code with existing structures, remove what it replaces, and avoid tiny helpers or unrelated edits that obscure the design.
- Understand F# and .NET behavior before optimizing. Check equality, allocation, collection conversions, recursion safety, and standard-library options. Choose a more elaborate structure only when its benefit is demonstrated.
- Test observable behavior. Cover meaningful transitions and edge cases with assertions a reader can understand. Keep one calculation path when special cases can be expressed as inputs to it.
- Measure performance at the right level. Benchmark both the suspected operation and its effect on the user-facing workflow. Compare before and after, and keep fixtures, results, and claims proportionate to what was measured.
- Keep work reviewable. Put lasting design rationale near the code; keep internal plans and benchmark artifacts tidy. Follow repository conventions for commits, and pause at agreed checkpoints for feedback.
- Run plain `dotnet test` without asking for permission.
