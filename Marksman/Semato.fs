/// SEMAntic TOkens
module Marksman.Semato

open Ionide.LanguageServerProtocol.Types

open Marksman.Cst
open Marksman.Index

[<Struct>]
type TokenType =
    | WikiLink
    | RefLink
    | Tag

module TokenType =
    let toLspName =
        function
        | WikiLink -> "class"
        | RefLink -> "class"
        | Tag -> "enumMember"

    let toNum =
        function
        | WikiLink -> 0u
        | RefLink -> 1u
        | Tag -> 2u

    let mapping = [| WikiLink; RefLink; Tag |] |> Array.map toLspName

type Token = {
    range: Range
    typ: TokenType
} with

    override this.ToString() = $"{this.typ}@{this.range}"

module Token =
    // We don't use modifiers in the encoding currently
    [<Literal>]
    let private Modifiers = 0u

    let private isSingleLine tok = tok.range.Start.Line = tok.range.End.Line
    let private lenSingleLine tok = tok.range.End.Character - tok.range.Start.Character

    let private deltaEncode prevTok curTok =
        assert (isSingleLine curTok)

        let deltaLine, deltaChar =
            match prevTok with
            | None -> curTok.range.Start.Line, curTok.range.Start.Character
            | Some prevTok ->
                let deltaLine = curTok.range.Start.Line - prevTok.range.Start.Line
                assert (deltaLine >= 0)

                let deltaChar =
                    if deltaLine = 0 then
                        curTok.range.Start.Character - prevTok.range.Start.Character
                    else
                        curTok.range.Start.Character

                assert (deltaChar >= 0)

                deltaLine, deltaChar

        [|
            uint32 deltaLine
            uint32 deltaChar
            uint32 (lenSingleLine curTok)
            TokenType.toNum curTok.typ
            Modifiers
        |]

    let encodeAll (tokens: seq<Token>) : array<uint32> =
        let tokens =
            tokens
            // Multiline tokens are messy to support. Let's filter them out
            |> Seq.filter isSingleLine
            // For further processing we need to sort tokens by their start pos
            |> Seq.sortBy (fun t -> t.range.Start)

        let encoded = ResizeArray()
        let mutable prevTok = None

        for curTok in tokens do
            encoded.AddRange(deltaEncode prevTok curTok)
            prevTok <- Some curTok

        encoded.ToArray()

    let ofIndex (index: Index) : seq<Token> =
        seq {
            for link in Index.wikiLinks index do
                match WikiLink.contentRange link.data with
                | Some range -> yield { range = range; typ = WikiLink }
                | None -> ()

            for link in Index.mdLinks index do
                match MdLink.referenceLabel link.data with
                | Some label -> yield { range = label.range; typ = RefLink }
                | None -> ()

            for tag in Index.tags index -> { range = tag.range; typ = Tag }
        }

    let isInRange (range: Range) token =
        token.range.Start >= range.Start && token.range.End <= range.End

    let inRange (range: Range) tokens = tokens |> Seq.filter (isInRange range)

    let ofIndexEncoded = ofIndex >> encodeAll

    let ofIndexEncodedInRange index range = ofIndex index |> inRange range |> encodeAll
