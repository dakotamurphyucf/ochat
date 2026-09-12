open! Core
module Inventory = Chatml.Chatml_surface_inventory

type document =
  { path : string
  ; sha256 : string
  ; text : string
  }

type grammar_production =
  { id : string
  ; contract_sha256 : string
  }

type t =
  { identity : string
  ; documents : document String.Map.t
  ; surfaces : Inventory.t String.Map.t
  ; surface_hashes : string String.Map.t
  ; grammar : grammar_production list
  }

let format_version = 2
let digest = Chatmd_shell_spec.Source_ref.digest
let identity t = t.identity
let documents t = Map.data t.documents
let surface_ids t = Map.keys t.surfaces
let grammar t = t.grammar

let document t ~path =
  match Map.find t.documents path with
  | Some document -> Ok document
  | None -> Error ("authoring source document is not installed: " ^ path)
;;

let signatures t ~surface_id =
  match Map.find t.surfaces surface_id with
  | Some inventory -> Ok inventory
  | None -> Error ("authoring compiler surface is not installed: " ^ surface_id)
;;

let valid_path path =
  String.is_suffix path ~suffix:".md"
  && List.for_all (String.split path ~on:'/') ~f:(function
    | "" | "." | ".." -> false
    | component ->
      String.for_all component ~f:(function
        | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> true
        | _ -> false))
;;

let installed () =
  let open Result.Let_syntax in
  let%bind inventories = Inventory.standard () in
  let%bind documents =
    match
      List.for_all Bundled_documents.documents ~f:(fun (path, text) ->
        valid_path path && not (String.is_empty text))
    with
    | false -> Error "invalid embedded authoring document"
    | true ->
      Bundled_documents.documents
      |> List.map ~f:(fun (path, text) -> path, { path; sha256 = digest text; text })
      |> String.Map.of_alist
      |> (function
       | `Duplicate_key path -> Error ("duplicate authoring document: " ^ path)
       | `Ok documents -> Ok documents)
  in
  let%bind surfaces =
    List.map inventories ~f:(fun inventory -> inventory.Inventory.surface_id, inventory)
    |> String.Map.of_alist
    |> function
    | `Duplicate_key id -> Error ("duplicate authoring surface: " ^ id)
    | `Ok surfaces -> Ok surfaces
  in
  let surface_hashes =
    Map.map surfaces ~f:(fun inventory ->
      Inventory.to_json inventory |> Jsonaf.to_string |> digest)
  in
  let grammar =
    List.map Bundled_grammar.productions ~f:(fun (id, contract_sha256) ->
      { id; contract_sha256 })
  in
  let identity =
    [%sexp
      (format_version : int)
    , (Map.to_alist (Map.map documents ~f:(fun document -> document.sha256))
       : (string * string) list)
    , (Map.to_alist surface_hashes : (string * string) list)
    , (List.map grammar ~f:(fun production -> production.id, production.contract_sha256)
       : (string * string) list)]
    |> Sexp.to_string_mach
    |> digest
  in
  Ok { identity; documents; surfaces; surface_hashes; grammar }
;;

let manifest t =
  `Object
    [ "format_version", `Number (Int.to_string format_version)
    ; "identity", `String t.identity
    ; ( "grammar"
      , `Array
          (List.map t.grammar ~f:(fun production ->
             `Object
               [ "id", `String production.id
               ; "sha256", `String production.contract_sha256
               ])) )
    ; ( "documents"
      , `Array
          (documents t
           |> List.map ~f:(fun document ->
             `Object
               [ "path", `String document.path
               ; "sha256", `String document.sha256
               ; "bytes", `Number (Int.to_string (String.length document.text))
               ])) )
    ; ( "surfaces"
      , `Array
          (Map.to_alist t.surface_hashes
           |> List.map ~f:(fun (id, sha256) ->
             `Object [ "id", `String id; "sha256", `String sha256 ])) )
    ]
;;
