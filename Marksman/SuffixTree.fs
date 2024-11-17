module Marksman.SuffixTree

module Impl =
    type SuffixTree<'K, 'V> when 'K: comparison = {
        nodes: Map<'K, SuffixTree<'K, 'V>>
        value: option<'V>
    }

    let empty = { nodes = Map.empty; value = None }

    let hasChildren st = Map.isEmpty st.nodes |> not

    let hasValue st = Option.isSome st.value

    let hasData st = hasValue st || hasChildren st

    let add (key: list<'K>) (value: 'V) (st: SuffixTree<'K, 'V>) : SuffixTree<'K, 'V> =
        let rec go key st =
            match key with
            | [] -> { st with value = Some value }
            | keyHead :: keyTail ->
                let tailTree = Map.tryFind keyHead st.nodes |> Option.defaultValue empty
                let tailTree = go keyTail tailTree
                { nodes = Map.add keyHead tailTree st.nodes; value = st.value }

        go (List.rev key) st

    let remove (key: list<'K>) (st: SuffixTree<'K, 'V>) : SuffixTree<'K, 'V> =
        let rec go key st =
            match key with
            | [] -> if hasData st then Some { st with value = None } else None
            | keyHd :: keyTl ->
                let withUpdatedTail tlTree =
                    match go keyTl tlTree with
                    | Some tlTree -> Some { st with nodes = Map.add keyHd tlTree st.nodes }
                    | None ->
                        let st = { st with nodes = Map.remove keyHd st.nodes }
                        if hasData st then Some st else None

                Map.tryFind keyHd st.nodes
                |> Option.map withUpdatedTail
                |> Option.defaultValue (Some st)

        go (List.rev key) st |> Option.defaultValue empty

    let ofSeq (sx: seq<list<'K> * 'V>) : SuffixTree<'K, 'V> =
        Seq.fold (fun st (key, value) -> add key value st) empty sx

    let rec private findSubtree keyRev st =
        match keyRev with
        | [] -> st
        | keyHead :: keyTail ->
            match Map.tryFind keyHead st.nodes with
            | None -> empty
            | Some sub -> findSubtree keyTail sub

    let rec collectValues (st: SuffixTree<'K, 'V>) =
        seq {
            match st.value with
            | Some v -> yield v
            | None -> ()

            for sub in st.nodes |> Map.values do
                yield! collectValues sub
        }


    let filterMatchingValues (key: list<'K>) (st: SuffixTree<'K, 'V>) : seq<'V> =
        let key = List.rev key
        let st = findSubtree key st
        st |> collectValues

[<CustomEquality; NoComparison>]
type SuffixTree<'K, 'V> when 'V: equality = private {
    splitFn: 'K -> list<string>
    tree: Impl.SuffixTree<string, 'V>
} with

    override this.Equals(other) =
        match other with
        | :? SuffixTree<'K, 'V> as otherST -> this.tree = otherST.tree
        | _ -> false

    override this.GetHashCode() = this.tree.GetHashCode()

module SuffixTree =
    let empty splitFn = { splitFn = splitFn; tree = Impl.empty }

    let ofSeq splitFn (data: seq<'K * 'V>) =
        let tree = Seq.map (fun (k, v) -> splitFn k, v) data |> Impl.ofSeq
        { splitFn = splitFn; tree = tree }

    let add k v t = { t with tree = Impl.add (t.splitFn k) v t.tree }

    let remove k t = { t with tree = Impl.remove (t.splitFn k) t.tree }

    let filterMatchingValues k t = Impl.filterMatchingValues (t.splitFn k) t.tree
