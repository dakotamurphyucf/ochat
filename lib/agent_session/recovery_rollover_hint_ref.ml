open! Core
module P = Agent_protocol
module C = Recovery_notes.Catalog
module R = P.Recovery_text_range
module Reference = Recovery_note_share.Reference
module J = P.Json_codec

type t =
  | Local_note of C.selection
  | Shared_note of Reference.t
  | Canonical_text of R.t

let invalid message = Error (P.Error.invalid_request message)
let local_note selection = Local_note selection
let shared_note reference = Shared_note reference

let canonical_text (request : R.t) =
  if
    Option.is_none request.preparation_revision
    && request.stop - request.start <= P.History_text.default_slice_bytes
    && request.max_bytes <= P.History_text.default_slice_bytes
  then Ok (Canonical_text request)
  else invalid "successor hint requires a bounded canonical text range"
;;

let equal a b =
  match a, b with
  | Local_note a, Local_note b -> C.equal_selection a b
  | Shared_note a, Shared_note b -> Reference.equal a b
  | Canonical_text a, Canonical_text b ->
    P.Recovery_window.Citation.equal a.citation b.citation
    && Int.equal a.part b.part
    && Int.equal a.start b.start
    && Int.equal a.stop b.stop
    && Int.equal a.max_bytes b.max_bytes
  | (Local_note _ | Shared_note _ | Canonical_text _), _ -> false
;;

let decimal n = `String (Int64.to_string n)

let positive = function
  | `String value when String.length value <= 19 ->
    (match Int64.of_string value with
     | n when Int64.(n > 0L) && String.equal value (Int64.to_string n) -> Ok n
     | _ | (exception _) -> invalid "invalid successor hint note coordinate")
  | _ -> invalid "invalid successor hint note coordinate"
;;

let to_json t =
  let kind, value =
    match t with
    | Local_note { id; revision } ->
      ( "local_note"
      , `Object
          [ "id", decimal (C.Note_id.to_int64 id)
          ; "revision", decimal (C.Note_revision.to_int64 revision)
          ] )
    | Shared_note reference -> "shared_note", Reference.to_json reference
    | Canonical_text request -> "canonical_text", R.to_json request
  in
  `Object [ "kind", `String kind; "reference", value ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = J.validate_limits ~max_depth:8 ~max_bytes:2_048 json in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  let%bind value = J.required fields "reference" in
  if not (Int.equal (List.length (J.to_alist fields)) 2)
  then invalid "invalid successor hint reference fields"
  else (
    match kind with
    | "local_note" ->
      let%bind fields = J.fields value in
      if not (Int.equal (List.length (J.to_alist fields)) 2)
      then invalid "invalid successor hint note fields"
      else (
        let%bind id = J.required_as fields "id" positive >>= C.Note_id.of_int64 in
        let%map revision =
          J.required_as fields "revision" positive >>= C.Note_revision.of_int64
        in
        Local_note { id; revision })
    | "shared_note" -> Result.map (Reference.of_json value) ~f:shared_note
    | "canonical_text" -> R.of_json value >>= canonical_text
    | _ -> invalid "invalid successor hint reference kind")
;;

module Selection = struct
  type reference = t
  type t = reference list

  let empty = []
  let max_references = 8

  let create references =
    if Stdlib.List.compare_length_with references max_references > 0
    then invalid "successor hint reference count exceeds the 8-item limit"
    else (
      let%map.Result _ =
        List.fold_result references ~init:[] ~f:(fun seen reference ->
          if List.mem seen reference ~equal
          then invalid "duplicate successor hint reference"
          else Ok (reference :: seen))
      in
      references)
  ;;

  let references t = t
  let equal = List.equal equal
  let to_json t = `Array (List.map t ~f:to_json)

  let of_json = function
    | `Array values when Stdlib.List.compare_length_with values max_references <= 0 ->
      let%bind.Result references = List.map values ~f:of_json |> Result.all in
      create references
    | _ -> invalid "invalid successor hint reference selection"
  ;;
end

module Witness = struct
  type t =
    { references : Selection.t
    ; check_current : unit -> bool
    }

  let create ~references ~check_current = { references; check_current }
  let references t = t.references
  let check_current t = t.check_current ()
end
