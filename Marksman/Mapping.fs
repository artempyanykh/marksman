module Marksman.Mapping

open Marksman.MMap

type Mapping<'Dom, 'Cod> when 'Dom: comparison and 'Cod: comparison = {
    mapping: Map<'Dom, 'Cod>
    inverse: MMap<'Cod, 'Dom>
}

module Mapping =
    let empty = { mapping = Map.empty; inverse = MMap.empty }

    let add (x: 'Dom) (y: 'Cod) (s: Mapping<'Dom, 'Cod>) =
        let cod = Map.tryFind x s.mapping

        let inverse =
            match cod with
            | None -> s.inverse
            | Some cod -> MMap.removeValue cod x s.inverse

        { mapping = Map.add x y s.mapping; inverse = MMap.add y x inverse }

    let inDom x s = Map.containsKey x s.mapping
    let inCod y s = MMap.containsKey y s.inverse

    let image x s = Map.find x s.mapping
    let tryImage x s = Map.tryFind x s.mapping

    let preImage y s = MMap.find y s.inverse
    let tryPreImage y s = MMap.tryFind y s.inverse
