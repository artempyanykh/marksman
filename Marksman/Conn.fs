module Marksman.Conn

open System.Collections.Generic
open Ionide.LanguageServerProtocol.Logging

open Marksman.Misc
open Marksman.MMap
open Marksman.PartitionedMap
open Marksman.Names
open Marksman.Paths
open Marksman.Graph
open Marksman.Syms

[<RequireQualifiedAccess>]
type DefinitionSelector =
    | DocumentTarget of Scope
    | SectionTarget of Scope * string
    | LinkDefinitionTarget of Scope * LinkLabel

module DefinitionSelector =
    let scope =
        function
        | DefinitionSelector.DocumentTarget scope
        | DefinitionSelector.SectionTarget(scope, _)
        | DefinitionSelector.LinkDefinitionTarget(scope, _) -> scope

    /// Def.Doc is an input to document selection because it supplies the
    /// fallback when a document has no title.
    let forDefinition scope =
        function
        | Def.Doc -> [ DefinitionSelector.DocumentTarget scope ]
        | Def.Title id -> [
            DefinitionSelector.DocumentTarget scope
            DefinitionSelector.SectionTarget(scope, id)
          ]
        | Def.Header(_, id) -> [ DefinitionSelector.SectionTarget(scope, id) ]
        | Def.LinkDef label -> [ DefinitionSelector.LinkDefinitionTarget(scope, label) ]

    let readsDefinition selector definition =
        forDefinition (scope selector) definition |> List.contains selector

type CandidateDocumentResolution = { documents: Set<DocId>; aliasesRead: Set<DocumentAlias> }

/// The oracle evaluates computations against the current folder snapshot.
type Oracle = {
    resolveCandidateDocuments: InternName -> CandidateDocumentResolution
    selectDefinitions: DefinitionSelector -> Def[]
}

/// The document facts Conn needs to update its symbols and lookup dependencies.
type DocumentInput = { id: DocId; slug: Slug; path: RelPath; symbols: Set<Sym> }

type DocumentChange =
    | Added of DocumentInput
    | Removed of DocumentInput
    | Replaced of before: DocumentInput * after: DocumentInput

type ConnectionChange = private {
    symbolDifference: Difference<ScopedSym>
    invalidatedDocumentAliases: Set<DocumentAlias>
}

module ConnectionChange =
    let ofDocuments (markdownExtensions: seq<string>) (changes: seq<DocumentChange>) =
        let aliases input = DocumentAlias.ofDocument markdownExtensions input.slug input.path

        let mutable symbolDifference: Difference<ScopedSym> = Difference.empty
        let mutable invalidatedDocumentAliases = Set.empty

        for change in changes do
            let before, after =
                match change with
                | DocumentChange.Added current -> None, Some current
                | DocumentChange.Removed previous -> Some previous, None
                | DocumentChange.Replaced(previous, current) -> Some previous, Some current

            let symbolChange =
                match before, after with
                | Some previous, Some current when previous.id = current.id -> {
                    added =
                        (current.symbols - previous.symbols)
                        |> Set.map (Sym.scopedToDoc current.id)
                    removed =
                        (previous.symbols - current.symbols)
                        |> Set.map (Sym.scopedToDoc previous.id)
                  }
                | _ -> {
                    added =
                        after
                        |> Option.map (fun input ->
                            input.symbols |> Set.map (Sym.scopedToDoc input.id))
                        |> Option.defaultValue Set.empty
                    removed =
                        before
                        |> Option.map (fun input ->
                            input.symbols |> Set.map (Sym.scopedToDoc input.id))
                        |> Option.defaultValue Set.empty
                  }

            symbolDifference <- {
                added = symbolDifference.added + symbolChange.added
                removed = symbolDifference.removed + symbolChange.removed
            }

            let aliasesToInvalidate =
                match before, after with
                | Some previous, Some current when
                    previous.id = current.id
                    && previous.slug = current.slug
                    && previous.path = current.path
                    ->
                    Set.empty
                | Some previous, Some current when previous.id <> current.id ->
                    aliases previous + aliases current
                | _ ->
                    let oldAliases =
                        before |> Option.map aliases |> Option.defaultValue Set.empty

                    let newAliases =
                        after |> Option.map aliases |> Option.defaultValue Set.empty

                    (oldAliases - newAliases) + (newAliases - oldAliases)

            invalidatedDocumentAliases <- invalidatedDocumentAliases + aliasesToInvalidate

        {
            symbolDifference = symbolDifference
            invalidatedDocumentAliases = invalidatedDocumentAliases
        }

    let isEmpty change =
        Difference.isEmpty change.symbolDifference
        && Set.isEmpty change.invalidatedDocumentAliases

type UnresolvedScope =
    | FullyUnknown
    | InScope of Scope

[<RequireQualifiedAccess>]
type Unresolved =
    | Ref of Scope * Ref
    | Scope of UnresolvedScope

    member this.CompactFormat() =
        match this with
        | Ref(scope, ref) -> $"{ref} @ {scope}"
        | Scope FullyUnknown -> "FullyUnknown"
        | Scope(InScope scope) -> $"{scope}"

// TODO: Separate incremental graph maintenance from the document-reference
// computations below. Conn currently mixes value caching, dependency tracking,
// invalidation, and scheduling with the rules for selecting documents and
// definitions, which obscures both parts. For example, setComputationValue
// returns a change flag that callers must propagate, while
// DefinitionSelector.forDefinition derives invalidation links on demand and other
// dependencies are recorded during evaluation. A possible next step is a small
// graph component that owns value comparison, dependency replacement, and
// propagation. The reference resolver would define computations and their
// inputs, including definition changes, through one consistent dependency API.
[<RequireQualifiedAccess>]
type private ConnectionComputation =
    | ResolveCandidateDocuments of InternName
    | SelectDefinitions of DefinitionSelector
    | ResolveReference of Scope * Ref

[<RequireQualifiedAccess>]
type private ConnectionDependency =
    | ExternalInput of DocumentAlias
    | ComputedValue of ConnectionComputation

type private ReferenceResolution = { resolved: Set<ScopedSym>; unresolved: Set<UnresolvedScope> }

[<RequireQualifiedAccess>]
type private ConnectionValue =
    | CandidateDocuments of Set<DocId>
    | SelectedDefinitions of Set<Def>
    | ReferenceResolution of ReferenceResolution

let private resolutionGraph symbols computedValues =
    let referenceEdges =
        seq {
            for node, value in PartitionedMap.toSeq computedValues do
                match node, value with
                | ConnectionComputation.ResolveReference(scope, ref),
                  ConnectionValue.ReferenceResolution result ->
                    for target in result.resolved do
                        yield (scope, Sym.Ref ref), target
                | _ -> ()
        }

    let tagEdges =
        seq {
            for scope, sym in MMap.toSeq symbols do
                match sym with
                | Sym.Tag _ -> yield (scope, sym), (Scope.Global, sym)
                | _ -> ()
        }

    Seq.append referenceEdges tagEdges
    |> Seq.fold (fun graph (source, target) -> Graph.addEdge source target graph) Graph.empty

let private unresolvedGraph computedValues =
    seq {
        for node, value in PartitionedMap.toSeq computedValues do
            match node, value with
            | ConnectionComputation.ResolveReference(scope, ref),
              ConnectionValue.ReferenceResolution result ->
                for target in result.unresolved do
                    yield Unresolved.Ref(scope, ref), Unresolved.Scope target
            | _ -> ()
    }
    |> Seq.fold (fun graph (source, target) -> Graph.addEdge source target graph) Graph.empty

/// Source symbols and derived computations form one connection state. The
/// reverse-reference map indexes resolved references and tag occurrences.
type Conn = private {
    symbols: MMap<Scope, Sym>
    dependencies: MMap<ConnectionComputation, ConnectionDependency>
    dependents: MMap<ConnectionDependency, ConnectionComputation>
    computedValues: PartitionedMap<ConnectionComputation, ConnectionValue>
    referencesByTarget: MMap<ScopedSym, ScopedSym>
} with

    member private this.ResolvedCompactFormat() =
        let graph = resolutionGraph this.symbols this.computedValues

        let edges =
            graph.edges
            |> MMap.toSetSeq
            |> Seq.groupBy (fun ((scope, _), _) -> scope)

        let lines =
            seq {
                for scope, scopedEdges in edges do
                    yield $"{scope}:"

                    for (_, sym), targets in scopedEdges do
                        for targetScope, targetSym in targets do
                            yield Indented(2, $"{sym} -> {targetSym} @ {targetScope}").ToString()
            }

        concatLines lines

    member private this.UnresolvedCompactFormat() =
        let graph = unresolvedGraph this.computedValues

        let lines =
            seq {
                for source, target in MMap.toSeq graph.edges do
                    yield $"{source.CompactFormat()} -> {target.CompactFormat()}"
            }

        concatLines lines

    member this.CompactFormat() =
        let lines =
            seq {
                let refs =
                    this.symbols
                    |> MMap.toSetSeq
                    |> Seq.choose (fun (scope, syms) ->
                        let refs = syms |> Seq.choose Sym.asRef |> Set.ofSeq
                        if Set.isEmpty refs then None else Some(scope, refs))

                if not (Seq.isEmpty refs) then
                    yield "Refs:"

                    for scope, refs in refs do
                        yield $"  {scope}:"

                        for ref in refs do
                            yield Indented(4, ref).ToString()

                let defs =
                    this.symbols
                    |> MMap.toSetSeq
                    |> Seq.choose (fun (scope, syms) ->
                        let defs = syms |> Seq.choose Sym.asDef |> Set.ofSeq
                        if Set.isEmpty defs then None else Some(scope, defs))

                if not (Seq.isEmpty defs) then
                    yield "Defs:"

                    for scope, defs in defs do
                        yield $"  {scope}:"

                        for def in defs do
                            yield Indented(4, def).ToString()

                let tags =
                    this.symbols
                    |> MMap.toSetSeq
                    |> Seq.choose (fun (scope, syms) ->
                        let tags = syms |> Seq.choose Sym.asTag |> Set.ofSeq
                        if Set.isEmpty tags then None else Some(scope, tags))

                if not (Seq.isEmpty tags) then
                    yield "Tags:"

                    for scope, tags in tags do
                        yield $"  {scope}:"

                        for tag in tags do
                            yield Indented(4, tag).ToString()

                yield "Resolved:"
                yield Indented(2, this.ResolvedCompactFormat()).ToString()
                yield "Unresolved:"
                yield Indented(2, this.UnresolvedCompactFormat()).ToString()
            }

        concatLines lines

type ConnDifference = {
    refsDifference: MMapDifference<Scope, Ref>
    defsDifference: MMapDifference<Scope, Def>
    tagsDifference: MMapDifference<Scope, Tag>
    resolvedDifference: GraphDifference<ScopedSym>
    unresolvedDifference: GraphDifference<Unresolved>
    stateDifferences: string list
} with

    member this.IsEmpty() =
        this.refsDifference.IsEmpty()
        && this.defsDifference.IsEmpty()
        && this.tagsDifference.IsEmpty()
        && this.resolvedDifference.IsEmpty()
        && this.unresolvedDifference.IsEmpty()
        && List.isEmpty this.stateDifferences

    member this.CompactFormat() =
        let lines =
            seq {
                if not (this.refsDifference.IsEmpty()) then
                    yield "Refs difference:"
                    yield Indented(2, this.refsDifference.CompactFormat()).ToString()

                if not (this.defsDifference.IsEmpty()) then
                    yield "Defs difference:"
                    yield Indented(2, this.defsDifference.CompactFormat()).ToString()

                if not (this.tagsDifference.IsEmpty()) then
                    yield "Tags difference:"
                    yield Indented(2, this.tagsDifference.CompactFormat()).ToString()

                if not (this.resolvedDifference.IsEmpty()) then
                    yield "Resolved difference:"
                    yield Indented(2, this.resolvedDifference.CompactFormat()).ToString()

                if not (this.unresolvedDifference.IsEmpty()) then
                    yield "Unresolved difference:"
                    yield Indented(2, this.unresolvedDifference.CompactFormat()).ToString()

                for state in this.stateDifferences do
                    yield $"{state} differs"
            }

        concatLines lines

module Conn =
    let private logger = LogProvider.getLoggerByName "Conn"

    let empty = {
        symbols = MMap.empty
        dependencies = MMap.empty
        dependents = MMap.empty
        computedValues = PartitionedMap.empty
        referencesByTarget = MMap.empty
    }

    let private computationDependencies computation conn =
        MMap.tryFind computation conn.dependencies
        |> Option.defaultValue Set.empty

    let private dependentComputations dependency conn =
        MMap.tryFind dependency conn.dependents
        |> Option.defaultValue Set.empty

    let private symbolsOf choose conn =
        conn.symbols
        |> MMap.toSeq
        |> Seq.choose (fun (scope, sym) -> choose sym |> Option.map (fun value -> scope, value))
        |> MMap.ofSeq

    let private tryReferenceName (scope, ref) =
        match scope, ref with
        | Scope.Doc src, CrossRef ref -> Some(InternName.mkUnchecked src ref.Doc)
        | _ -> None

    let private selectorForReference ref scope =
        match ref with
        | CrossRef(CrossDoc _) -> DefinitionSelector.DocumentTarget scope
        | CrossRef(CrossSection(_, section))
        | IntraRef(IntraSection section) ->
            DefinitionSelector.SectionTarget(scope, Slug.toString section)
        | IntraRef(IntraLinkDef label) -> DefinitionSelector.LinkDefinitionTarget(scope, label)

    let private removeComputation computation conn =
        let dependenciesOfComputation = computationDependencies computation conn
        let computedValue = ConnectionDependency.ComputedValue computation
        let dependentsOfComputation = dependentComputations computedValue conn

        let dependents =
            dependenciesOfComputation
            |> Set.fold
                (fun acc dependency -> MMap.removeValue dependency computation acc)
                conn.dependents

        let dependencies =
            dependentsOfComputation
            |> Set.fold
                (fun acc dependent -> MMap.removeValue dependent computedValue acc)
                conn.dependencies

        {
            conn with
                dependencies = MMap.removeKey computation dependencies
                dependents = MMap.removeKey computedValue dependents
                computedValues = PartitionedMap.remove computation conn.computedValues
        },
        dependenciesOfComputation

    // ResolveReference computations are rooted at actual source symbols, not derived
    // caches, so they're removed by removeSymbol, never garbage-collected here.
    let private isCollectableComputation =
        function
        | ConnectionComputation.ResolveCandidateDocuments _
        | ConnectionComputation.SelectDefinitions _ -> true
        | ConnectionComputation.ResolveReference _ -> false

    let private collectOrphanedComputations dependencies conn =
        if Set.isEmpty dependencies then
            conn
        else
            // The input set deduplicates initial work. Collectable computations
            // currently have no computed-value inputs, so removing one cannot
            // enqueue another. The existence check below also makes repeated
            // entries safe if that changes.
            let pending = Queue<ConnectionComputation>()

            let enqueue dependencies =
                for dependency in dependencies do
                    match dependency with
                    | ConnectionDependency.ComputedValue computation -> pending.Enqueue computation
                    | ConnectionDependency.ExternalInput _ -> ()

            enqueue dependencies
            let mutable current = conn

            while pending.Count > 0 do
                let computation = pending.Dequeue()

                if
                    isCollectableComputation computation
                    && Set.isEmpty (
                        dependentComputations
                            (ConnectionDependency.ComputedValue computation)
                            current
                    )
                    && PartitionedMap.containsKey computation current.computedValues
                then
                    let next, inputs = removeComputation computation current
                    current <- next
                    enqueue inputs

            current

    let private setComputationDependencies computation dependencies conn =
        let old = computationDependencies computation conn

        if old = dependencies then
            conn
        else
            let removed = old - dependencies
            let added = dependencies - old

            let dependents =
                removed
                |> Set.fold
                    (fun acc dependency -> MMap.removeValue dependency computation acc)
                    conn.dependents
                |> fun index ->
                    added
                    |> Set.fold (fun acc dependency -> MMap.add dependency computation acc) index

            let conn = {
                conn with
                    dependencies = MMap.setValues computation dependencies conn.dependencies
                    dependents = dependents
            }

            collectOrphanedComputations removed conn

    let private setComputationValue computation value conn =
        let values = PartitionedMap.add computation value conn.computedValues

        { conn with computedValues = values },
        not (obj.ReferenceEquals(values, conn.computedValues))

    let private evaluateCandidateDocuments oracle name conn =
        let node = ConnectionComputation.ResolveCandidateDocuments name
        let resolution = oracle.resolveCandidateDocuments name

        let dependencies =
            resolution.aliasesRead |> Set.map ConnectionDependency.ExternalInput

        let conn = setComputationDependencies node dependencies conn

        let conn, changed =
            setComputationValue node (ConnectionValue.CandidateDocuments resolution.documents) conn

        conn, resolution.documents, changed

    let private evaluateDefinitionSelection oracle selector conn =
        let node = ConnectionComputation.SelectDefinitions selector

        let scope = DefinitionSelector.scope selector

        let definitions =
            if MMap.containsKey scope conn.symbols then
                oracle.selectDefinitions selector |> Set.ofArray
            else
                Set.empty

        let conn, changed =
            setComputationValue node (ConnectionValue.SelectedDefinitions definitions) conn

        conn, definitions, changed

    let private ensureCandidateDocuments oracle name conn =
        match
            PartitionedMap.tryFind
                (ConnectionComputation.ResolveCandidateDocuments name)
                conn.computedValues
        with
        | Some(ConnectionValue.CandidateDocuments documents) -> conn, documents
        | _ ->
            let conn, documents, _ = evaluateCandidateDocuments oracle name conn
            conn, documents

    let private ensureSelectedDefinitions oracle selector conn =
        match
            PartitionedMap.tryFind
                (ConnectionComputation.SelectDefinitions selector)
                conn.computedValues
        with
        | Some(ConnectionValue.SelectedDefinitions definitions) -> conn, definitions
        | _ ->
            let conn, definitions, _ =
                evaluateDefinitionSelection oracle selector conn

            conn, definitions

    let private replaceReferenceResolution ((scope, ref) as source) resolution conn =
        let node = ConnectionComputation.ResolveReference source
        let sourceSymbol = scope, Sym.Ref ref

        let oldTargets =
            match PartitionedMap.tryFind node conn.computedValues with
            | Some(ConnectionValue.ReferenceResolution old) -> old.resolved
            | _ -> Set.empty

        let referencesByTarget =
            oldTargets
            |> Set.fold
                (fun index target -> MMap.removeValue target sourceSymbol index)
                conn.referencesByTarget
            |> fun index ->
                resolution.resolved
                |> Set.fold (fun index target -> MMap.add target sourceSymbol index) index

        {
            conn with
                computedValues =
                    PartitionedMap.add
                        node
                        (ConnectionValue.ReferenceResolution resolution)
                        conn.computedValues
                referencesByTarget = referencesByTarget
        }

    let private evaluateReference oracle ((scope, ref) as source) conn =
        let mutable conn = conn
        let mutable dependencies = Set.empty

        let scopes =
            match tryReferenceName source with
            | None -> Set.singleton scope
            | Some name ->
                let candidatesNode = ConnectionComputation.ResolveCandidateDocuments name

                dependencies <-
                    Set.add (ConnectionDependency.ComputedValue candidatesNode) dependencies

                let next, docs = ensureCandidateDocuments oracle name conn
                conn <- next
                Set.map Scope.Doc docs

        let mutable result = {
            resolved = Set.empty
            unresolved = if Set.isEmpty scopes then Set.singleton FullyUnknown else Set.empty
        }

        for targetScope in scopes do
            let selector = selectorForReference ref targetScope
            let selection = ConnectionComputation.SelectDefinitions selector
            dependencies <- Set.add (ConnectionDependency.ComputedValue selection) dependencies
            let next, definitions = ensureSelectedDefinitions oracle selector conn
            conn <- next

            if Set.isEmpty definitions then
                result <- {
                    result with
                        unresolved = Set.add (InScope targetScope) result.unresolved
                }

            let targets = definitions |> Set.map (fun def -> targetScope, Sym.Def def)
            result <- { result with resolved = result.resolved + targets }

        conn
        |> setComputationDependencies (ConnectionComputation.ResolveReference source) dependencies
        |> replaceReferenceResolution source result

    let private removeReference source conn =
        let node = ConnectionComputation.ResolveReference source
        let scope, ref = source
        let sourceSymbol = scope, Sym.Ref ref

        let referencesByTarget =
            match PartitionedMap.tryFind node conn.computedValues with
            | Some(ConnectionValue.ReferenceResolution resolution) ->
                resolution.resolved
                |> Set.fold
                    (fun index target -> MMap.removeValue target sourceSymbol index)
                    conn.referencesByTarget
            | _ -> conn.referencesByTarget

        let conn = { conn with referencesByTarget = referencesByTarget }
        let conn, dependencies = removeComputation node conn

        collectOrphanedComputations dependencies conn

    let private removeSymbol (scope, sym) conn =
        let conn =
            match sym with
            | Sym.Ref ref -> removeReference (scope, ref) conn
            | Sym.Tag _ -> {
                conn with
                    referencesByTarget =
                        MMap.removeValue (Scope.Global, sym) (scope, sym) conn.referencesByTarget
              }
            | Sym.Def _ -> conn

        { conn with symbols = MMap.removeValue scope sym conn.symbols }

    let private addSymbol (scope, sym) conn =
        let referencesByTarget =
            match sym with
            | Sym.Tag _ -> MMap.add (Scope.Global, sym) (scope, sym) conn.referencesByTarget
            | _ -> conn.referencesByTarget

        {
            conn with
                symbols = MMap.add scope sym conn.symbols
                referencesByTarget = referencesByTarget
        }

    let private rebuild oracle initialWork conn =
        let mutable conn = conn
        let mutable work = Set.empty

        let priority =
            function
            | ConnectionComputation.ResolveCandidateDocuments _ -> 0
            | ConnectionComputation.SelectDefinitions _ -> 1
            | ConnectionComputation.ResolveReference _ -> 2

        let enqueue node = work <- Set.add (priority node, node) work

        Set.iter enqueue initialWork
        let mutable evaluatedCandidateDocumentComputations = 0
        let mutable evaluatedReferences = 0

        // Resolve prerequisites before references. A changed value requeues its
        // dependents, and the ordered set suppresses duplicate work.
        while not (Set.isEmpty work) do
            let item = Set.minElement work
            work <- Set.remove item work
            let _, node = item

            match node with
            | ConnectionComputation.ResolveCandidateDocuments name when
                PartitionedMap.containsKey node conn.computedValues
                ->
                let next, _, changed = evaluateCandidateDocuments oracle name conn
                conn <- next
                evaluatedCandidateDocumentComputations <- evaluatedCandidateDocumentComputations + 1

                if changed then
                    dependentComputations (ConnectionDependency.ComputedValue node) conn
                    |> Set.iter enqueue
            | ConnectionComputation.SelectDefinitions selector when
                PartitionedMap.containsKey node conn.computedValues
                ->
                let next, _, changed = evaluateDefinitionSelection oracle selector conn
                conn <- next

                if changed then
                    dependentComputations (ConnectionDependency.ComputedValue node) conn
                    |> Set.iter enqueue
            | ConnectionComputation.ResolveReference(scope, ref) when
                MMap.tryFind scope conn.symbols
                |> Option.exists (Set.contains (Sym.Ref ref))
                ->
                conn <- evaluateReference oracle (scope, ref) conn
                evaluatedReferences <- evaluatedReferences + 1
            | _ -> ()

        logger.trace (
            Log.setMessage "Updated connection graph"
            >> Log.addContext
                "#candidate_document_computations"
                evaluatedCandidateDocumentComputations
            >> Log.addContext "#references" evaluatedReferences
        )

        conn

    let update (oracle: Oracle) (change: ConnectionChange) (previous: Conn) : Conn =
        if ConnectionChange.isEmpty change then
            previous
        else
            let mutable conn = previous

            for symbol in change.symbolDifference.removed do
                conn <- removeSymbol symbol conn

            for symbol in change.symbolDifference.added do
                conn <- addSymbol symbol conn

            let dirtyDefinitionComputations =
                change.symbolDifference.added + change.symbolDifference.removed
                |> Seq.choose (fun (scope, sym) ->
                    Sym.asDef sym |> Option.map (fun def -> scope, def))
                |> Seq.collect (fun (scope, definition) ->
                    DefinitionSelector.forDefinition scope definition)
                |> Seq.map ConnectionComputation.SelectDefinitions
                |> Set.ofSeq

            let dirtyCandidateDocumentComputations =
                change.invalidatedDocumentAliases
                |> Seq.collect (fun key ->
                    dependentComputations (ConnectionDependency.ExternalInput key) conn)
                |> Set.ofSeq

            let newReferenceComputations =
                change.symbolDifference.added
                |> Seq.choose Marksman.Syms.ScopedSym.asScopedRef
                |> Seq.map ConnectionComputation.ResolveReference
                |> Set.ofSeq

            rebuild
                oracle
                (dirtyDefinitionComputations
                 + dirtyCandidateDocumentComputations
                 + newReferenceComputations)
                conn

    let mk (oracle: Oracle) (symMap: MMap<DocId, Sym>) : Conn =
        let mutable conn = empty

        for doc, sym in MMap.toSeq symMap do
            conn <- addSymbol (Scope.Doc doc, sym) conn

        for scope, sym in MMap.toSeq conn.symbols do
            match sym with
            | Sym.Ref ref -> conn <- evaluateReference oracle (scope, ref) conn
            | _ -> ()

        conn

    let difference c1 c2 : ConnDifference = {
        refsDifference = MMap.difference (symbolsOf Sym.asRef c1) (symbolsOf Sym.asRef c2)
        defsDifference = MMap.difference (symbolsOf Sym.asDef c1) (symbolsOf Sym.asDef c2)
        tagsDifference = MMap.difference (symbolsOf Sym.asTag c1) (symbolsOf Sym.asTag c2)
        resolvedDifference =
            Graph.difference
                (resolutionGraph c1.symbols c1.computedValues)
                (resolutionGraph c2.symbols c2.computedValues)
        unresolvedDifference =
            Graph.difference (unresolvedGraph c1.computedValues) (unresolvedGraph c2.computedValues)
        stateDifferences = [
            if c1.dependencies <> c2.dependencies then
                "Dependencies"
            if c1.dependents <> c2.dependents then
                "Dependents"
            if c1.computedValues <> c2.computedValues then
                "Computed values"
            if c1.referencesByTarget <> c2.referencesByTarget then
                "Reverse reference index"
        ]
    }

module Query =
    let private sourceContains scope sym conn =
        MMap.tryFind scope conn.symbols |> Option.exists (Set.contains sym)

    /// Compare materialized reference results without recalculating references.
    /// Unchanged partitions of the computed-value map need no traversal.
    let documentsWithChangedReferenceResolutions (before: Conn) (after: Conn) : Set<DocId> =
        let mutable changed = Set.empty

        PartitionedMap.iterDifferences
            (fun node _ _ ->
                match node with
                | ConnectionComputation.ResolveReference(Scope.Doc doc, _) ->
                    changed <- Set.add doc changed
                | _ -> ())
            before.computedValues
            after.computedValues

        changed

    let resolve ((scope, sym) as scopedSym) (conn: Conn) : Set<ScopedSym> =
        match sym with
        | Sym.Ref ref when sourceContains scope sym conn ->
            match
                PartitionedMap.tryFind
                    (ConnectionComputation.ResolveReference(scope, ref))
                    conn.computedValues
            with
            | Some(ConnectionValue.ReferenceResolution result) -> result.resolved
            | _ -> Set.empty
        | Sym.Tag _ when sourceContains scope sym conn -> Set.singleton (Scope.Global, sym)
        | _ ->
            MMap.tryFind scopedSym conn.referencesByTarget
            |> Option.defaultValue Set.empty
