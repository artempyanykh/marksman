module Marksman.Refactor

open Ionide.LanguageServerProtocol.Types
open Marksman.Workspace
open Cst

type RenameResult =
    | Edit of WorkspaceEdit
    | Error of string
    | NoChange

// let tryFindElementToRename

//let rename (doc: Doc) (pos: Position) (newName: string) : RenameResult =
//    match Cst.elementAtPos pos doc.cst with
//    | None -> NoChange
//    | Some (ML link) ->
//        match MdLink.referenceLabel link with
//        | None -> NoChange
//        | Some label -> NoChange
            
        
