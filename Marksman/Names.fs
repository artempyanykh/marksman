module Marksman.Names

open System
open Marksman.Misc
open Marksman.Paths

type UrlEncoded = UrlEncoded of string

module UrlEncoded =
    let mkUnchecked = UrlEncoded
    let encode (str: string) = UrlEncoded(str.UrlEncode())

    let decode (UrlEncoded str) = str.UrlDecode()
    let raw (UrlEncoded str) = str

type WikiEncoded = WikiEncoded of string

module WikiEncoded =
    let mkUnchecked = WikiEncoded
    let encode (str: string) = WikiEncoded(str.EncodeForWiki())
    let decode (WikiEncoded str) = str.UrlDecode()
    let raw (WikiEncoded raw) = raw
    let encodeAsString (str: string) : string = encode str |> raw

type FolderId = UriWith<RootPath>

module FolderId =
    let ofUri uri = UriWith.mkRoot uri

[<Struct>]
[<StructuredFormatDisplay("{ShortFormat}")>]
[<CustomEquality; CustomComparison>]
type DocId =
    | DocId of UriWith<RootedRelPath>

    member this.Raw =
        let (DocId uri) = this
        uri

    member this.Path =
        let (DocId uri) = this
        uri.data

    member this.Uri =
        let (DocId uri) = this
        uri.uri

    member this.ShortFormat = this.Path |> RootedRelPath.toSystem

    interface IEquatable<DocId> with
        member this.Equals(that: DocId) =
            let (DocId thisUri) = this
            let (DocId thatUri) = that
            thisUri.uri = thatUri.uri

    override this.Equals(that) =
        match that with
        | :? DocId as that -> (this :> IEquatable<_>).Equals(that)
        | _ -> false

    override this.GetHashCode() =
        let (DocId uri) = this
        hash uri.uri

    interface IComparable<DocId> with
        member this.CompareTo(that: DocId) =
            let (DocId thisUri) = this
            let (DocId thatUri) = that
            compare thisUri.uri thatUri.uri

    interface IComparable with
        member this.CompareTo(that: obj) =
            match that with
            | :? DocId as that -> (this :> IComparable<_>).CompareTo(that)
            | _ -> failwith $"Can't compare DocId with other types: {that}"


type InternName = { src: DocId; name: string }

type InternPath =
    | ExactAbs of RootedRelPath
    | ExactRel of src: DocId * path: RootedRelPath
    | Approx of RelPath

module InternPath =
    let toRel =
        function
        | ExactAbs rooted
        | ExactRel (_, rooted) -> RootedRelPath.relPathForced rooted
        | Approx path -> path

module InternName =
    let mkUnchecked src name = { src = src; name = name }

    let mkChecked exts src name =
        if isPotentiallyInternalRef exts name then
            Some({ src = src; name = name })
        else
            None

    let name { name = name } = name

    let slug { name = name } = Slug.ofString name

    let src { src = src } = src

    let tryAsPath ({ src = src; name = name }: InternName) =
        if Uri.IsWellFormedUriString(name, UriKind.Absolute) then
            None
        else if name.StartsWith('/') then
            let relPath =
                LocalPath.ofSystem (
                    UrlEncoded.mkUnchecked (name.TrimStart('/')) |> UrlEncoded.decode
                )

            let rootPath = RootedRelPath.rootPath src.Path
            let namePath = RootedRelPath.mk rootPath relPath
            Some(ExactAbs namePath)
        else
            try
                let rawNamePath =
                    LocalPath.ofSystem (UrlEncoded.mkUnchecked name |> UrlEncoded.decode)

                if LocalPath.hasDotComponents rawNamePath then
                    RootedRelPath.directory src.Path
                    |> Option.bind (fun dir -> RootedRelPath.combine dir rawNamePath)
                    |> Option.map (fun x -> ExactRel(src, x))
                else
                    match rawNamePath with
                    | Abs _ -> None
                    | Rel path -> Some(Approx path)

            with
            | :? UriFormatException
            | :? InvalidOperationException -> None

    let asPath name =
        tryAsPath name
        |> Option.defaultWith (fun () -> failwith $"Can't convert InternName {name} to a path")
