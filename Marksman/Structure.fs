module Marksman.Structure

open FSharpPlus.GenericBuilders

open Marksman.Syms
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

    let tryFindMatchingAbstract (cel: Cst.Element) structure : option<Ast.Element> =
        Mapping.tryImage cel structure.c2a

    let findMatchingAbstract (cel: Cst.Element) structure : Ast.Element =
        tryFindMatchingAbstract cel structure
        |> Option.defaultWith (fun () ->
            failwith $"No matching abstract element for: {Cst.Element.fmt cel}")

    // DON'T USE. ONLY FOR TESTING
    let tryFindConcreteForAbstract (ael: Ast.Element) structure : option<Set<Cst.Element>> =
        Mapping.tryPreImage ael structure.c2a

    let findConcreteForAbstract (ael: Ast.Element) structure : Set<Cst.Element> =
        let cels =
            tryFindConcreteForAbstract ael structure
            |> Option.defaultValue Set.empty

        if Set.isEmpty cels then
            failwith $"No matching concrete element for: {ael.CompactFormat()}"
        else
            cels

    let tryFindSymbolForAbstract (ael: Ast.Element) structure : option<Sym> =
        Mapping.tryImage ael structure.a2s

    let tryFindSymbolForConcrete (cel: Cst.Element) structure : option<Sym> =
        monad' {
            let! ael = tryFindMatchingAbstract cel structure
            return! tryFindSymbolForAbstract ael structure
        }

    let findAbstractForSymbol (sym: Sym) structure : Set<Ast.Element> =
        Mapping.tryPreImage sym structure.a2s |> Option.defaultValue Set.empty

    let findConcreteForSymbol (sym: Sym) structure : Set<Cst.Element> =
        findAbstractForSymbol sym structure
        |> Set.fold (fun acc ael -> acc + findConcreteForAbstract ael structure) Set.empty

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

        let syms = ResizeArray<Syms.Sym>()
        syms.Add(Syms.Sym.Def Def.Doc)

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
