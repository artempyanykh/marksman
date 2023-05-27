module Marksman.Names

open System
open Marksman.Misc
open Marksman.Paths

type UrlEncoded = UrlEncoded of string

module UrlEncoded =
    let mkUnchecked = UrlEncoded
    let encode (str: string) = UrlEncoded(str.UrlEncode())

    let decode (UrlEncoded str) = str.UrlDecode()

type WikiEncoded = WikiEncoded of string

module WikiEncoded =
    let mkUnchecked = WikiEncoded
    let encode (str: string) = WikiEncoded(str.EncodeForWiki())
    let raw (WikiEncoded raw) = raw

type FolderId = UriWith<RootPath>

module FolderId =
    let ofUri uri = UriWith.mkRoot uri

type DocId = UriWith<RootedRelPath>

type InternName = { src: DocId; name: string }

module InternName =
    let mkUnchecked src name = { src = src; name = name }

    let mkChecked exts src name =
        if Uri.IsWellFormedUriString(name, UriKind.Absolute) then
            None
        else if isPotentiallyMarkdownFile exts name then
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

            let rootPath = src.data.root
            let namePath = RootedRelPath.mk rootPath relPath
            Some(namePath)
        else
            try
                let rawNamePath =
                    LocalPath.ofSystem (UrlEncoded.mkUnchecked name |> UrlEncoded.decode)

                RootedRelPath.combine (RootedRelPath.directory src.data) rawNamePath
            with
            | :? UriFormatException
            | :? InvalidOperationException -> None

    let asPath name =
        tryAsPath name
        |> Option.defaultWith (fun () -> failwith $"Can't convert InternName {name} to a path")
