open! Core
module Catalog = Recovery_notes.Catalog
module P = Agent_protocol
module H = P.History_text

let error code message = P.Error.create code ~message ~retryable:false ()
let number n = `Number (Int.to_string n)
let counter n = `String (Int64.to_string n)
let revision_json rev = counter (Catalog.Catalog_revision.to_int64 rev)

let nullable f = function
  | Some value -> f value
  | None -> `Null
;;

let selection_json { Catalog.id; revision } =
  `Object
    [ "id", counter (Catalog.Note_id.to_int64 id)
    ; "revision", counter (Catalog.Note_revision.to_int64 revision)
    ]
;;

module Range = struct
  type t =
    { start : int
    ; stop : int
    }

  let create ~start ~stop =
    if start >= 0 && stop >= start && stop <= 65_536
    then Ok { start; stop }
    else Error (P.Error.invalid_request "invalid recovery note byte range")
  ;;

  let read t content =
    let open Result.Let_syntax in
    let%bind part = H.create ~index:0 ~text:(Catalog.Content.to_string content) in
    let%map slice = H.read part ~start:t.start ~stop:t.stop in
    `Object
      [ "text", `String slice.text
      ; "start", number t.start
      ; "stop", number (t.start + String.length slice.text)
      ; "next_offset", nullable number slice.next_offset
      ]
  ;;
end

module Page = struct
  type cursor =
    { revision : Catalog.Catalog_revision.t
    ; after : Catalog.Note_path.t
    ; query : Chatml_source.Digest.t
    }

  type t =
    { limit : int
    ; cursor : cursor option
    }

  let create ~limit ~cursor =
    if limit >= 1 && limit <= 16
    then Ok { limit; cursor }
    else Error (P.Error.invalid_request "recovery note page size must be 1 to 16")
  ;;

  let query_digest catalog term =
    let input =
      `Array
        [ P.Session_ref.to_json (Catalog.owner catalog)
        ; nullable P.Search_term.to_json term
        ]
      |> Jsonaf.to_string
    in
    Chatml_source.Digest.of_contents input ~checkpoint:ignore
  ;;

  let cursor_json { revision; after; query } =
    `Object
      [ "revision", revision_json revision
      ; "after", `String (Catalog.Note_path.to_string after)
      ; "query", `String (Chatml_source.Digest.to_hex query)
      ]
  ;;

  let validate t catalog ~query =
    match t.cursor with
    | None -> Ok ()
    | Some cursor ->
      if
        Catalog.Catalog_revision.equal cursor.revision (Catalog.revision catalog)
        && Chatml_source.Digest.equal cursor.query query
        && List.exists (Catalog.list catalog) ~f:(fun (note : Catalog.metadata) ->
          Catalog.Note_path.equal cursor.after note.path)
      then Ok ()
      else Error (error Cursor_expired "recovery note cursor expired")
  ;;

  let run t catalog ~query project =
    let open Result.Let_syntax in
    let%bind () = validate t catalog ~query in
    let revision = Catalog.revision catalog in
    let notes =
      match t.cursor with
      | None -> Catalog.list catalog
      | Some { after; _ } ->
        List.drop_while (Catalog.list catalog) ~f:(fun (note : Catalog.metadata) ->
          Catalog.Note_path.compare note.path after <= 0)
    in
    let finish items cursor =
      `Object
        [ "catalog_revision", revision_json revision
        ; "items", `Array (List.rev items)
        ; "next", nullable cursor_json cursor
        ]
    in
    let rec take items remaining after = function
      | [] -> Ok (finish items None)
      | (note : Catalog.metadata) :: rest ->
        (match%bind project note with
         | None -> take items remaining after rest
         | Some _ when remaining = 0 ->
           Ok
             (finish
                items
                (Option.map after ~f:(fun after -> { revision; after; query })))
         | Some value -> take (value :: items) (remaining - 1) (Some note.path) rest)
    in
    take [] t.limit None notes
  ;;
end

type t =
  | Read of Catalog.selection * Range.t
  | List of Page.t
  | Search of P.Search_term.t * Page.t

type plan =
  | Exact_note of Catalog.selection * Range.t
  | Exact_list of Catalog.Catalog_revision.t * Page.t
  | Exact_search of Catalog.Catalog_revision.t * P.Search_term.t * Page.t

let missing () =
  P.Error.create
    Conflict
    ~message:"recovery note is unavailable"
    ~data:(`String "recovery.note_missing")
    ~retryable:false
    ()
;;

let stale () = error Conflict "recovery note revision changed"

let read catalog selection =
  Catalog.read catalog selection
  |> Result.map_error ~f:(function
    | Note_not_found -> missing ()
    | Stale_note_revision _ -> stale ())
;;

let prepare t catalog =
  let open Result.Let_syntax in
  match t with
  | Read (selection, range) ->
    let%bind content = read catalog selection in
    let%map _ = Range.read range content in
    Exact_note (selection, range)
  | List page ->
    let%map () = Page.validate page catalog ~query:(Page.query_digest catalog None) in
    Exact_list (Catalog.revision catalog, page)
  | Search (term, page) ->
    let%map () =
      Page.validate page catalog ~query:(Page.query_digest catalog (Some term))
    in
    Exact_search (Catalog.revision catalog, term, page)
;;

let current t catalog =
  match t with
  | Exact_note (selection, _) -> Result.is_ok (Catalog.read catalog selection)
  | Exact_list (revision, _) | Exact_search (revision, _, _) ->
    Catalog.Catalog_revision.equal revision (Catalog.revision catalog)
;;

let metadata_fields (note : Catalog.metadata) =
  [ "selection", selection_json note.selection
  ; "path", `String (Catalog.Note_path.to_string note.path)
  ; "byte_length", number note.byte_length
  ]
;;

let render t catalog =
  let open Result.Let_syntax in
  if not (current t catalog)
  then Error (stale ())
  else (
    match t with
    | Exact_note (selection, range) ->
      let%bind content = read catalog selection in
      Range.read range content
    | Exact_list (_, page) ->
      Page.run page catalog ~query:(Page.query_digest catalog None) (fun note ->
        Ok (Some (`Object (metadata_fields note))))
    | Exact_search (_, term, page) ->
      let literal = H.Literal.create term in
      Page.run page catalog ~query:(Page.query_digest catalog (Some term)) (fun note ->
        let%bind content = read catalog note.selection in
        let%bind part = H.create ~index:0 ~text:(Catalog.Content.to_string content) in
        let%map found = H.Literal.find literal part in
        Option.map found ~f:(fun hit ->
          `Object
            (metadata_fields note
             @ [ "start", number hit.start
               ; "stop", number hit.stop
               ; "snippet", P.Search_snippet.to_json hit.snippet
               ]))))
;;
