open! Core
module J = Json_codec
module W = Recovery_wire
module Citation = Recovery_window.Citation

let method_name = "session.recovery.read_text"
let feature = "recovery.text_range"
let max_offset = History_text.max_part_bytes
let max_reply = History_text.max_slice_bytes
let invalid message = Error (Protocol_error.invalid_request message)
let number n = `Number (Int.to_string n)

type t =
  { citation : Citation.t
  ; preparation_revision : int64 option
  ; part : int
  ; start : int
  ; stop : int
  ; max_bytes : int
  }

let create
      citation
      ?preparation_revision
      ~part
      ~start
      ~stop
      ?(max_bytes = History_text.default_slice_bytes)
      ()
  =
  let has_source =
    match Citation.version citation, preparation_revision with
    | Canonical _, None -> true
    | Effective _, Some revision -> Int64.(revision >= 0L)
    | Canonical _, Some _ | Effective _, None -> false
  in
  if
    (not has_source)
    || part < 0
    || part >= History_text.max_parts
    || start < 0
    || stop < start
    || stop > max_offset
    || max_bytes < 1
    || max_bytes > max_reply
  then invalid "invalid recovery text range or preparation revision"
  else Ok { citation; preparation_revision; part; start; stop; max_bytes }
;;

let session t = Recovery_window.Scope.session (Citation.scope t.citation)

let to_json t =
  `Object
    ([ "citation", Citation.to_json t.citation
     ; "part", number t.part
     ; "start", number t.start
     ; "stop", number t.stop
     ; "max_bytes", number t.max_bytes
     ]
     @ Option.to_list
         (Option.map t.preparation_revision ~f:(fun revision ->
            "preparation_revision", W.revision_to_json revision)))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = W.admit_coordinate json in
  let%bind f =
    W.fields
      json
      [ "citation"; "preparation_revision"; "part"; "start"; "stop"; "max_bytes" ]
  in
  let%bind citation = J.required_as f "citation" Citation.of_json in
  let%bind preparation_revision =
    J.optional_as f "preparation_revision" W.revision_of_json
  in
  let%bind part =
    J.required_as f "part" (J.bounded_int ~min:0 ~max:(History_text.max_parts - 1))
  in
  let offset = J.bounded_int ~min:0 ~max:max_offset in
  let%bind start = J.required_as f "start" offset in
  let%bind stop = J.required_as f "stop" offset in
  let%bind max_bytes =
    J.optional_as f "max_bytes" (J.bounded_int ~min:1 ~max:max_reply)
  in
  create citation ?preparation_revision ~part ~start ~stop ?max_bytes ()
;;

module Reply = struct
  type t =
    { text : string
    ; next_offset : int option
    }

  let of_slice (slice : History_text.Slice.t) =
    { text = slice.text; next_offset = slice.next_offset }
  ;;

  let to_json t =
    `Object
      (("text", `String t.text)
       :: Option.to_list (Option.map t.next_offset ~f:(fun n -> "next_offset", number n))
      )
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "text"; "next_offset" ] in
    let%bind text = J.required_as fields "text" J.string in
    let%bind next_offset =
      J.optional_as fields "next_offset" (J.bounded_int ~min:1 ~max:max_offset)
    in
    if String.length text > max_reply || not (Stdlib.String.is_valid_utf_8 text)
    then invalid "invalid recovery range text"
    else Ok { text; next_offset }
  ;;

  let validate t ~request =
    let bytes = String.length t.text in
    let finish = request.start + bytes in
    let valid_end =
      match t.next_offset with
      | None -> finish = request.stop
      | Some next -> bytes > 0 && next = finish && finish < request.stop
    in
    if bytes <= request.max_bytes && valid_end
    then Ok ()
    else invalid "recovery text response does not match its requested range"
  ;;
end
