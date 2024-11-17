module Marksman.Mapping

open Marksman.MMap

type Mapping<'Dom, 'Cod> when 'Dom: comparison and 'Cod: comparison = {
    mapping: Map<'Dom, 'Cod>
    inverse: MMap<'Cod, 'Dom>
} with

    member this.Add(x: 'Dom, y: 'Cod) : Mapping<'Dom, 'Cod> =
        let cod = this.mapping.TryFind(x)

        let inverse =
            match cod with
            | None -> this.inverse
            | Some cod -> this.inverse.RemoveValue(cod, x)

        { mapping = this.mapping.Add(x, y); inverse = inverse.Add(y, x) }

    member this.InDom(x: 'Dom) : bool = this.mapping.ContainsKey(x)

    member this.InCod(y: 'Cod) : bool = this.inverse.ContainsKey(y)

    member this.Image(x: 'Dom) : 'Cod = Map.find x this.mapping

    member this.TryImage(x: 'Dom) : option<'Cod> = Map.tryFind x this.mapping

    member this.PreImage(y: 'Cod) : Set<'Dom> = this.inverse.Find(y)

    member this.TryPreImage(y: 'Cod) : option<Set<'Dom>> = this.inverse.TryFind(y)


module Mapping =
    let empty = { mapping = Map.empty; inverse = MMap.empty }

    let add (x: 'Dom) (y: 'Cod) (m: Mapping<'Dom, 'Cod>) = m.Add(x, y)

    let inDom x (m: Mapping<'Dom, 'Cod>) = m.InDom(x)
    let inCod y (m: Mapping<'Dom, 'Cod>) = m.InCod(y)

    let image x (m: Mapping<'Dom, 'Cod>) = m.Image(x)
    let tryImage x (m: Mapping<'Dom, 'Cod>) = m.TryImage(x)

    let preImage y (m: Mapping<'Dom, 'Cod>) = m.PreImage(y)
    let tryPreImage y (m: Mapping<'Dom, 'Cod>) = m.TryPreImage(y)
