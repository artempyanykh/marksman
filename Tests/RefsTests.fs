module Marksman.RefsTests

open Ionide.LanguageServerProtocol.Types
open System.IO
open Xunit

open Marksman.Cst
open Marksman.Helpers
open Marksman.Misc
open Marksman.Names
open Marksman.Paths
open Marksman.Refs
open Marksman.Doc
open Marksman.Folder

module InternNameTests =
    let internAsPath docId name =
        InternName.tryAsPath (InternName.mkUnchecked docId name)
        |> Option.map (InternPath.toRel >> RelPath.toSystem)

    [<Fact>]
    let relPath_1 () =
        let folder = dummyRootPath [ "rootFolder" ] |> mkFolderId

        let docPath =
            dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]
            |> mkDocId folder

        let actual = internAsPath docPath "../doc.md" |> Option.get

        Assert.Equal("doc.md", actual)

    [<Fact>]
    let relPath_2 () =
        let folder = dummyRootPath [ "rootFolder" ] |> mkFolderId
        let docPath = dummyRootPath [ "rootFolder"; "doc1.md" ] |> mkDocId folder
        let actual = internAsPath docPath "./doc2.md" |> Option.get

        Assert.Equal("doc2.md", actual)

    [<Fact>]
    let relPath_non_exist () =
        let folder = dummyRootPath [ "rootFolder" ] |> mkFolderId
        let docPath = dummyRootPath [ "rootFolder"; "doc1.md" ] |> mkDocId folder
        let actual = internAsPath docPath "../doc2.md"
        Assert.Equal(None, actual)

    [<Fact>]
    let rootPath () =
        let folder = dummyRootPath [ "rootFolder" ] |> mkFolderId

        let docPath =
            dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]
            |> mkDocId folder

        let actual = internAsPath docPath "/doc.md" |> Option.get

        Assert.Equal("doc.md", actual)

    [<Fact>]
    let url_no_schema_FP () =
        let folder = dummyRootPath [ "rootFolder" ] |> mkFolderId

        let docPath =
            dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]
            |> mkDocId folder

        let actual = internAsPath docPath "www.google.com" |> Option.get

        Assert.Equal("www.google.com", actual)

    [<Fact>]
    let url_schema () =
        let folder = dummyRootPath [ "rootFolder" ] |> mkFolderId

        let docPath =
            dummyRootPath [ "rootFolder"; "subfolder"; "sub.md" ]
            |> mkDocId folder

        let actual = internAsPath docPath "http://www.google.com"

        Assert.Equal(None, actual)

let requireElementAtPos doc line col =
    Cst.elementAtPos (Position.Mk(line, col)) (Doc.cst doc)
    |> Option.defaultWith (fun _ -> failwith $"No element found at ({line}, {col})")

let formatRefs (refs: seq<Doc * Element>) =
    refs
    |> Seq.map (fun (doc, el) -> Path.GetFileName(Doc.uri doc), el.Range.DebuggerDisplay)
    |> Array.ofSeq

module FileLinkTests =
    let doc1 = FakeDoc.Mk(path = "doc1.md", contentLines = [| "# Horses" |])

    let subDoc1 =
        FakeDoc.Mk(path = "sub/doc1.md", contentLines = [| "# Sub Horses" |])

    let subSubDoc1 =
        FakeDoc.Mk(path = "sub/sub/doc1.md", contentLines = [| "# Sub Sub Horses" |])

    let doc2 =
        FakeDoc.Mk(path = "doc2.md", contentLines = [| "# Riding Horses" |])

    let subDoc2 =
        FakeDoc.Mk(path = "sub/doc2.md", contentLines = [| "# Sub Riding Horses" |])

    let doc3 = FakeDoc.Mk(path = "doc3.md", contentLines = [| "# Fruit" |])

    let doc4 =
        FakeDoc.Mk(path = "file with spaces.md", contentLines = [| "# File with spaces" |])

    [<Fact>]
    let fileName_Partial () =
        let folder =
            FakeFolder.Mk(
                [ doc1; doc2; doc4 ],
                { Config.Config.Default with complWikiStyle = Some Config.FileStem }
            )

        let partial =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked doc1.Id "doc")

        Assert.Empty(partial)

        let full =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked doc1.Id "doc2")
            |> Array.ofSeq

        Assert.Equal(1, full.Length)
        Assert.Equal(doc2, full[0].doc)

        let fullWithSpacesEncoded =
            FileLink.filterMatchingDocs
                folder
                (InternName.mkUnchecked doc2.Id "file%20with%20spaces.md")
            |> Array.ofSeq

        Assert.Equal(1, fullWithSpacesEncoded.Length)
        Assert.Equal(doc4, fullWithSpacesEncoded[0].doc)

        let fullWithSpacesNotEncoded =
            FileLink.filterMatchingDocs
                folder
                (InternName.mkUnchecked doc2.Id "file with spaces.md")
            |> Array.ofSeq

        Assert.Equal(1, fullWithSpacesNotEncoded.Length)
        Assert.Equal(doc4, fullWithSpacesNotEncoded[0].doc)

    [<Fact>]
    let fileName_RelativeAsAbs () =
        let folder =
            FakeFolder.Mk(
                [ doc1; subDoc1; subSubDoc1; doc2; subDoc2 ],
                { Config.Config.Default with complWikiStyle = Some Config.FilePathStem }
            )

        let actual =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked doc1.Id "doc1")
            |> Seq.map FileLink.doc
            |> Array.ofSeq

        Assert.Equal<Doc>(actual, [| doc1; subDoc1; subSubDoc1 |])

        let actual =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked subDoc1.Id "doc2")
            |> Seq.map FileLink.doc
            |> Array.ofSeq

        Assert.Equal<Doc>(actual, [| doc2; subDoc2 |])

        let actual =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked doc1.Id "sub/doc1")
            |> Seq.map FileLink.doc
            |> Array.ofSeq

        Assert.Equal<Doc>(actual, [| subDoc1; subSubDoc1 |])

    [<Fact>]
    let heading_Partial () =
        let folder =
            FakeFolder.Mk(
                [ doc1; doc2; doc3 ],
                { Config.Config.Default with complWikiStyle = Some Config.TitleSlug }
            )

        let actual =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked doc3.Id "horses")
            |> Seq.map FileLink.doc
            |> Array.ofSeq

        Assert.Equal<Doc>(actual, [| doc1 |])

        let actual =
            FileLink.filterMatchingDocs folder (InternName.mkUnchecked doc3.Id "riding-horses")
            |> Seq.map FileLink.doc
            |> Array.ofSeq

        Assert.Equal<Doc>(actual, [| doc2 |])



module BasicRefsTests =
    let doc1 =
        FakeDoc.Mk(
            path = "doc1.md",
            contentLines =
                [| "# Doc 1" // 0
                   "" // 1
                   "## D1 H2.1" // 2
                   "" // 3
                   "[[doc-2#d2-h22]]" // 4
                   "" // 5
                   "## D1 H2.2" // 6
                   "" // 7
                   "[[#dup]]" // 8
                   "" // 9
                   "## Dup" // 10
                   "Entry 1" // 11
                   "" // 12
                   "## Dup" // 13
                   "Entry 2" // 14
                   "" // 15
                   "#tag1 #tag2" |] // 16
        )

    let doc2 =
        FakeDoc.Mk(
            path = "doc2.md",
            contentLines =
                [| "# Doc 2" // 0
                   "" // 1
                   "# D2 H2.1" // 2
                   "" // 3
                   "[D2-Link-1]" // 4
                   "" // 5
                   "[[#d2-h22]]" // 6
                   "" // 7
                   "[d2-LINK-1]" // 8
                   "" // 9
                   "# D2 H2.2" // 10
                   "" // 11
                   "[[doc-1]]" // 12
                   "[[doc-1#dup]]" // 13
                   "[lbl1](/doc1.md)" // 14
                   "[^fn1]" // 15
                   "" // 16
                   "[d2-link-1]: some-url" // 17
                   "" // 18
                   "[^fn1]: This is footnote" // 19
                   "" //20
                   "#tag1" |] //21
        )

    let folder = FakeFolder.Mk [ doc1; doc2 ]

    [<Fact>]
    let refToTag_atTag () =
        let def =
            Cst.elementAtPos (Position.Mk(21, 1)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs false folder doc2 def |> formatRefs

        checkInlineSnapshot (fun x -> x.ToString()) refs [ "(doc1.md, (16,0)-(16,5))" ]

    [<Fact>]
    let refToTag_atTag_withDecl () =
        let def =
            Cst.elementAtPos (Position.Mk(21, 1)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs true folder doc2 def |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc1.md, (16,0)-(16,5))"; "(doc2.md, (21,0)-(21,5))" ]

    [<Fact>]
    let refToLinkDef_atDef () =
        let def =
            Cst.elementAtPos (Position.Mk(17, 3)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs false folder doc2 def |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (4,0)-(4,11))"; "(doc2.md, (8,0)-(8,11))" ]

    [<Fact>]
    let refToLinkDef_atDef_withDecl () =
        let def =
            Cst.elementAtPos (Position.Mk(17, 3)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs true folder doc2 def |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (4,0)-(4,11))"
              "(doc2.md, (8,0)-(8,11))"
              "(doc2.md, (17,0)-(17,21))" ]

    [<Fact>]
    let refToLinkDef_atLink () =
        let def =
            Cst.elementAtPos (Position.Mk(8, 4)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs false folder doc2 def |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (4,0)-(4,11))"; "(doc2.md, (8,0)-(8,11))" ]

    [<Fact>]
    let refToLinkDef_atLink_withDecl () =
        let def =
            Cst.elementAtPos (Position.Mk(8, 4)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs true folder doc2 def |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (4,0)-(4,11))"
              "(doc2.md, (8,0)-(8,11))"
              "(doc2.md, (17,0)-(17,21))" ]

    [<Fact(Skip = "Footnote parsing not implemented")>]
    let refToFootnote_atLink () =
        let fnLink =
            Cst.elementAtPos (Position.Mk(15, 2)) (Doc.cst doc2)
            |> Option.defaultWith (fun _ -> failwith "No def")

        let refs = Dest.findElementRefs true folder doc2 fnLink |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (19,0)-(19,16))"; "(doc2.md, (15,0)-(15,6))" ]

    [<Fact>]
    let refToDoc_atTitle () =
        let title =
            Cst.elementAtPos (Position.Mk(0, 2)) (Doc.cst doc1)
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs false folder doc1 title |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc2.md, (12,0)-(12,9))"
              "(doc2.md, (13,0)-(13,13))"
              "(doc2.md, (14,0)-(14,16))" ]

    [<Fact>]
    let refToDoc_atTitle_withDecl () =
        let title =
            Cst.elementAtPos (Position.Mk(0, 2)) (Doc.cst doc1)
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs true folder doc1 title |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc1.md, (0,0)-(0,7))"
              "(doc2.md, (12,0)-(12,9))"
              "(doc2.md, (13,0)-(13,13))"
              "(doc2.md, (14,0)-(14,16))" ]

    [<Fact>]
    let refToDoc_atLink () =
        let wl =
            Cst.elementAtPos (Position.Mk(4, 4)) (Doc.cst doc1)
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs false folder doc1 wl |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc1.md, (4,0)-(4,16))"; "(doc2.md, (6,0)-(6,11))" ]

    [<Fact>]
    let refToDoc_atLink_withDecl () =
        let wl =
            Cst.elementAtPos (Position.Mk(4, 4)) (Doc.cst doc1)
            |> Option.defaultWith (fun _ -> failwith "No title")

        let refs = Dest.findElementRefs true folder doc1 wl |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(doc1.md, (4,0)-(4,16))"
              "(doc2.md, (6,0)-(6,11))"
              "(doc2.md, (10,0)-(10,9))" ]


module LinkKindRefsTests =
    let doc1 =
        FakeDoc.Mk(
            path = "file1.md",
            contentLines =
                [| "# Doc 1 Title"
                   "[[file2]]"
                   "[[file2.md]]"
                   "[[file2#doc-2-subtitle]]"
                   "[[doc-2-title]]"
                   "[link](file2)"
                   "[[file3]]" |]
        )

    let doc2 =
        FakeDoc.Mk(
            path = "file2.md",
            contentLines = [| "# Doc 2 Title"; "## Doc 2 SubTitle"; "[[doc-3-title]]" |]
        )

    let doc3 =
        FakeDoc.Mk(path = "sub/file3.md", contentLines = [| "# Doc 3 Title" |])

    let folder = FakeFolder.Mk([ doc1; doc2; doc3 ])


    [<Fact>]
    let atWiki_VariousFilenames () =
        let link = requireElementAtPos doc1 1 2
        let refs = Dest.findElementRefs false folder doc1 link |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(file1.md, (1,0)-(1,9))"
              "(file1.md, (2,0)-(2,12))"
              // Referencing a doc should resolve to all sections in the doc
              "(file1.md, (3,0)-(3,24))"
              "(file1.md, (4,0)-(4,15))"
              "(file1.md, (5,0)-(5,13))" ]

    [<Fact>]
    let atWiki_Filenames_Subfolder () =
        let link = requireElementAtPos doc2 2 3
        let refs = Dest.findElementRefs false folder doc2 link |> formatRefs

        checkInlineSnapshot
            (fun x -> x.ToString())
            refs
            [ "(file1.md, (6,0)-(6,9))"; "(file2.md, (2,0)-(2,15))" ]

module EncodingTests =
    let doc1 =
        FakeDoc.Mk(
            content =
                """# Doc 1

## Heading 1

## Heading 2

[[#Heading 1]]
[[#Heading%201]]
[[Doc 2]]
[[Doc%202]]
[[Doc 2#Heading %231]]
[](Doc%202)
[](Doc%202#heading-1)
[[doc.3.with.dots.md]]
[[doc.3.with.dots]]
""",
            path = "doc1.md"
        )

    let doc2 =
        FakeDoc.Mk(
            content =
                """# Doc 2

## Heading #1
""",
            path = "doc2.md"
        )

    let doc3 = FakeDoc.Mk(content = """Doc 3""", path = "doc.3.with.dots.md")

    let folder =
        FakeFolder.Mk(
            [ doc1; doc2; doc3 ],
            { Config.Config.Default with complWikiStyle = Some Config.FilePathStem }
        )

    let exts = (Folder.configOrDefault folder).CoreMarkdownFileExtensions()

    let simplifyDest (dest: Dest) : string =
        match dest with
        | Dest.Doc fileLink -> fileLink.doc |> Doc.name
        | Dest.Heading (docLink, node) ->
            let doc = DocLink.doc docLink |> Doc.name
            let head = node.text
            $"{doc} / {head}"
        | Dest.LinkDef (doc, node) ->
            let defName = MdLinkDef.label node.data
            $"{Doc.name doc} / {defName}"
        | Dest.Tag (doc, node) ->
            let tag = node.text
            $"{Doc.name doc} / {tag}"

    let resolveAtPos doc line col =
        let el = requireElementAtPos doc line col
        Dest.tryResolveElement folder doc el |> Seq.map simplifyDest

    [<Fact>]
    let headingNotEncoding () =
        let refs = resolveAtPos doc1 6 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 1 / ## Heading 1" ]

    [<Fact>]
    let headingUrlEncoded () =
        let refs = resolveAtPos doc1 7 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 1 / ## Heading 1" ]

    [<Fact>]
    let docNotEncoded () =
        let refs = resolveAtPos doc1 8 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 2 / # Doc 2" ]

    [<Fact>]
    let docUrlEncoded () =
        let refs = resolveAtPos doc1 9 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 2 / # Doc 2" ]

    [<Fact>]
    let docHeadingMixedEncoding () =
        let refs = resolveAtPos doc1 10 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 2 / ## Heading #1" ]

    [<Fact>]
    let inlineDocUrlEncoding () =
        let refs = resolveAtPos doc1 11 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 2 / # Doc 2" ]

    [<Fact>]
    let inlineDocHeadingMixedEncoding () =
        let refs = resolveAtPos doc1 12 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "Doc 2 / ## Heading #1" ]

    [<Fact>]
    let docFileNameWithDots () =
        let refs = resolveAtPos doc1 13 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "doc.3.with.dots" ]

        let refs = resolveAtPos doc1 14 5
        checkInlineSnapshot (fun x -> x.ToString()) refs [ "doc.3.with.dots" ]
