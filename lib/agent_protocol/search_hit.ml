open! Core
module J = Json_codec

type t =
  { session : Session_ref.t
  ; generation : int
  ; session_revision : int64
  ; history_id : History.Id.t
  ; content_revision : History.Content_revision.t
  ; part_index : int
  ; snippet : Search_snippet.t
  }
[@@deriving sexp_of]

let create
      ~session
      ~generation
      ~session_revision
      ~history_id
      ~content_revision
      ~part_index
      ~snippet
  =
  if
    generation < 0
    || Int64.(session_revision < zero)
    || part_index < 0
    || part_index > 4095
  then
    Error
      (Protocol_error.invalid_request
         "invalid search hit identity, revision or part index")
  else
    Ok
      { session
      ; generation
      ; session_revision
      ; history_id
      ; content_revision
      ; part_index
      ; snippet
      }
;;

let session t = t.session
let generation t = t.generation
let session_revision t = t.session_revision
let history_id t = t.history_id
let content_revision t = t.content_revision
let part_index t = t.part_index
let snippet t = t.snippet

let session_revision_of_json = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value
       when Int64.(value >= zero) && String.equal (Int64.to_string value) encoded ->
       Ok value
     | Some _ | None ->
       Error (Protocol_error.invalid_request "invalid search session revision"))
  | _ ->
    Error
      (Protocol_error.invalid_request "search session revision must be a decimal string")
;;

let to_json t =
  `Object
    [ "session", Session_ref.to_json t.session
    ; "generation", `Number (Int.to_string t.generation)
    ; "session_revision", `String (Int64.to_string t.session_revision)
    ; "history_id", History.Id.to_json t.history_id
    ; "content_revision", History.Content_revision.to_json t.content_revision
    ; "part_index", `Number (Int.to_string t.part_index)
    ; "snippet", Search_snippet.to_json t.snippet
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind session = J.required_as fields "session" Session_ref.of_json in
  let%bind generation =
    J.required_as fields "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind session_revision =
    J.required_as fields "session_revision" session_revision_of_json
  in
  let%bind history_id = J.required_as fields "history_id" History.Id.of_json in
  let%bind content_revision =
    J.required_as fields "content_revision" History.Content_revision.of_json
  in
  let%bind part_index =
    J.required_as fields "part_index" (J.bounded_int ~min:0 ~max:4095)
  in
  let%bind snippet = J.required_as fields "snippet" Search_snippet.of_json in
  create
    ~session
    ~generation
    ~session_revision
    ~history_id
    ~content_revision
    ~part_index
    ~snippet
;;
