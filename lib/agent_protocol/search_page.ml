open! Core
module J = Json_codec

type t =
  { hits : Search_hit.t list
  ; next_cursor : Page.Cursor.t option
  ; reached_end : bool
  ; scanned_entries : int
  ; scanned_sessions : int
  }

let compare_entry_identity a b =
  match Session_ref.compare (Search_hit.session a) (Search_hit.session b) with
  | 0 -> History.Id.compare (Search_hit.history_id a) (Search_hit.history_id b)
  | order -> order
;;

let to_json t =
  `Object
    ([ "hits", `Array (List.map t.hits ~f:Search_hit.to_json)
     ; ("reached_end", if t.reached_end then `True else `False)
     ; "scanned_entries", `Number (Int.to_string t.scanned_entries)
     ; "scanned_sessions", `Number (Int.to_string t.scanned_sessions)
     ]
     @ Option.to_list
         (Option.map t.next_cursor ~f:(fun cursor ->
            "next_cursor", Page.Cursor.to_json cursor)))
;;

let create ~hits ~next_cursor ~reached_end ~scanned_entries ~scanned_sessions =
  let hit_count = List.length hits in
  if
    hit_count > 100
    || scanned_entries < hit_count
    || scanned_entries > 512
    || scanned_entries < 0
    || scanned_sessions < 0
    || scanned_sessions > 64
  then Error (Protocol_error.invalid_request "invalid search page scan bounds")
  else if Bool.equal reached_end (Option.is_some next_cursor)
  then
    Error (Protocol_error.invalid_request "search completion disagrees with continuation")
  else if (not reached_end) && scanned_entries = 0 && scanned_sessions = 0
  then Error (Protocol_error.invalid_request "partial search page makes no progress")
  else if List.contains_dup hits ~compare:compare_entry_identity
  then Error (Protocol_error.invalid_request "search page repeats a canonical entry")
  else (
    let t = { hits; next_cursor; reached_end; scanned_entries; scanned_sessions } in
    Result.map
      (J.validate_limits ~max_depth:24 ~max_bytes:262_144 (to_json t))
      ~f:(fun () -> t))
;;

let hits t = t.hits
let next_cursor t = t.next_cursor
let reached_end t = t.reached_end
let scanned_entries t = t.scanned_entries
let scanned_sessions t = t.scanned_sessions

let of_json json =
  let open Result.Let_syntax in
  let%bind () = J.validate_limits ~max_depth:24 ~max_bytes:262_144 json in
  let%bind fields = J.fields json in
  let%bind hits =
    J.required_as fields "hits" (function
      | `Array values when List.length values <= 100 ->
        J.list Search_hit.of_json (`Array values)
      | _ ->
        Error
          (Protocol_error.invalid_request
             "search hits must be an array of at most 100 items"))
  in
  let%bind next_cursor = J.optional_as fields "next_cursor" Page.Cursor.of_json in
  let%bind reached_end = J.required_as fields "reached_end" J.bool in
  let%bind scanned_entries =
    J.required_as fields "scanned_entries" (J.bounded_int ~min:0 ~max:512)
  in
  let%bind scanned_sessions =
    J.required_as fields "scanned_sessions" (J.bounded_int ~min:0 ~max:64)
  in
  create ~hits ~next_cursor ~reached_end ~scanned_entries ~scanned_sessions
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;
