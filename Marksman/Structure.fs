module Marksman.Structure

open Marksman.Sym
open Marksman.Mapping

type Structure =
    private
        { cst: Cst.Cst
          ast: Ast.Ast
          sym: Set<Sym>
          c2a: Mapping<Cst.Element, Ast.Element>
          a2s: Mapping<Ast.Element, Sym> }

    member this.Cst = this.cst
    member this.Ast = this.ast

    member this.Symbols = this.sym

module Structure =
    let abstractElements { ast = ast } = ast.elements
    let concreteElements { cst = cst } = cst.elements
    let symbols { sym = sym } = sym

    let findMatchingAbstract (cel: Cst.Element) structure : Ast.Element =
        match Mapping.tryImage cel structure.c2a with
        | Some ael -> ael
        | None -> failwith $"No matching abstract element for: {Cst.Element.fmt cel}"

    let tryFindMatchingConcrete (ael: Ast.Element) structure : option<Cst.Element[]> =
        Mapping.tryPreImage ael structure.c2a |> Option.map Set.toArray

    let findMatchingConcrete (ael: Ast.Element) structure : Cst.Element[] =
        let cels =
            tryFindMatchingConcrete ael structure |> Option.defaultValue [||]

        if Array.isEmpty cels then
            failwith $"No matching concrete element for: {ael.CompactFormat()}"
        else
            cels

    let ofCst (exts: seq<string>) (cst: Cst.Cst) : Structure =
        let rec go cst =
            seq {
                for cel in cst do
                    let ael = Cst.Element.toAbstract cel
                    let sym = ael |> Option.bind (Ast.Element.toSym exts)

                    match ael with
                    | Some ael -> yield cel, ael, sym
                    | None -> ()
            }

        let abs = ResizeArray<Ast.Element>()

        let syms = ResizeArray<Sym.Sym>()
        syms.Add(Sym.Sym.Def Def.Doc)

        let mutable c2a = Mapping.empty
        let mutable a2s = Mapping.empty
        // Accumulate AST elements and mapping
        for cel, ael, sym in go cst.elements do
            abs.Add(ael)
            c2a <- Mapping.add cel ael c2a

            sym
            |> Option.iter (fun sym ->
                syms.Add(sym)
                a2s <- Mapping.add ael sym a2s)

        let ast: Ast.Ast = { elements = abs.ToArray() }

        { cst = cst
          ast = ast
          sym = syms.ToArray() |> Set.ofArray
          c2a = c2a
          a2s = a2s }
