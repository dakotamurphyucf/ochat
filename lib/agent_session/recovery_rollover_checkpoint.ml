open! Core
module P = Agent_protocol
module Scope = P.Recovery_window.Scope
module J = P.Json_codec
module D = Document_schema

let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value
let revision_json revision = `Number (Int64.to_string revision)
let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

module Expected = struct
  type t =
    { scope : Scope.t
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  let create scope ~revision =
    if Int64.(revision < 0L)
    then invalid "invalid recovery rollover revision"
    else Ok { scope; revision }
  ;;

  let to_json t =
    `Object [ "scope", Scope.to_json t.scope; "revision", revision_json t.revision ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind scope = J.required_as fields "scope" Scope.of_json in
    let%bind revision = J.required_as fields "revision" nonnegative in
    create scope ~revision
  ;;
end

module Catalog = Recovery_notes.Catalog

type recipe = Owner_selection of P.Id.Principal.t [@@deriving equal, sexp_of]

type t =
  { expected : Expected.t
  ; through_history : Chatml_source.Digest.t
  ; notes : Catalog.selection list
  ; recipe : recipe
  }
[@@deriving equal, sexp_of]

let validate_notes notes =
  let compare (a : Catalog.selection) (b : Catalog.selection) =
    Catalog.Note_id.compare a.id b.id
  in
  if
    List.is_empty notes
    || Stdlib.List.compare_length_with notes Catalog.max_notes > 0
    || List.contains_dup notes ~compare
  then invalid "invalid rollover checkpoint note selection"
  else Ok ()
;;

let history_digest history =
  if Stdlib.List.compare_length_with history Recovery_window_record.max_members > 0
  then invalid "rollover checkpoint history exceeds the recovery window bound"
  else (
    let json = `Array (List.map history ~f:P.History.entry_to_json) in
    let open Result.Let_syntax in
    let%map () =
      D.Json.validate ~limits:D.Limits.default json
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "rollover checkpoint history exceeds document bounds")
    in
    Chatml_source.Digest.of_contents (Jsonaf.to_string json) ~checkpoint:ignore)
;;

let check_notes expected notes ~catalog =
  match catalog with
  | Some catalog
    when P.Session_ref.equal
           (Catalog.owner catalog)
           (Scope.session expected.Expected.scope)
         && List.for_all notes ~f:(fun note -> Result.is_ok (Catalog.read catalog note))
    -> Ok ()
  | Some _ | None -> conflict "rollover checkpoint notes are unavailable"
;;

let capture expected ~history ~catalog ~notes ~recipe =
  let open Result.Let_syntax in
  let%bind () = validate_notes notes in
  let%bind () = check_notes expected notes ~catalog in
  let%map through_history = history_digest history in
  { expected; through_history; notes; recipe }
;;

let expected t = t.expected

let check_contents t ~history ~catalog =
  let open Result.Let_syntax in
  let%bind () = check_notes t.expected t.notes ~catalog in
  let%bind current = history_digest history in
  if Chatml_source.Digest.equal current t.through_history
  then Ok ()
  else conflict "rollover checkpoint covers different canonical history"
;;

let recipe_to_json (Owner_selection principal) =
  `Object
    [ "kind", `String "owner_selection"; "principal", P.Id.Principal.to_json principal ]
;;

let recipe_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  if String.equal kind "owner_selection"
  then (
    let%map principal = J.required_as fields "principal" P.Id.Principal.of_json in
    Owner_selection principal)
  else invalid "unknown rollover checkpoint recipe"
;;

let note_to_json (note : Catalog.selection) =
  `Object
    [ "id", revision_json (Catalog.Note_id.to_int64 note.id)
    ; "revision", revision_json (Catalog.Note_revision.to_int64 note.revision)
    ]
;;

let note_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind id = J.required_as fields "id" nonnegative >>= Catalog.Note_id.of_int64 in
  let%map revision =
    J.required_as fields "revision" nonnegative >>= Catalog.Note_revision.of_int64
  in
  { Catalog.id; revision }
;;

let notes_of_json = function
  | `Array notes when Stdlib.List.compare_length_with notes Catalog.max_notes <= 0 ->
    List.map notes ~f:note_of_json |> Result.all
  | _ -> invalid "invalid rollover checkpoint note selection"
;;

let to_json t =
  `Object
    [ "expected", Expected.to_json t.expected
    ; "through_history", `String (Chatml_source.Digest.to_hex t.through_history)
    ; "notes", `Array (List.map t.notes ~f:note_to_json)
    ; "recipe", recipe_to_json t.recipe
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind expected = J.required_as fields "expected" Expected.of_json in
  let%bind digest = J.required_as fields "through_history" J.string in
  let%bind through_history =
    Chatml_source.Digest.of_hex digest
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid rollover checkpoint history digest")
  in
  let%bind notes = J.required_as fields "notes" notes_of_json in
  let%bind recipe = J.required_as fields "recipe" recipe_of_json in
  let%map () = validate_notes notes in
  { expected; through_history; notes; recipe }
;;
