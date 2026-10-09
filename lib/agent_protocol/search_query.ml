open! Core
module J = Json_codec

type t =
  { server_id : Id.Server.t
  ; term : Search_term.t
  ; catalog : Session.List_request.t
  ; scan_limit : int
  }

let to_json t =
  match Session.List_request.to_json t.catalog with
  | `Object fields ->
    `Object
      (("server_id", Id.Server.to_json t.server_id)
       :: ("term", Search_term.to_json t.term)
       :: ("scan_limit", `Number (Int.to_string t.scan_limit))
       :: fields)
  | _ -> assert false
;;

let create ~server_id ~term ~(catalog : Session.List_request.t) ~scan_limit =
  let open Result.Let_syntax in
  let%bind () =
    if List.length catalog.labels > 128
    then Error (Protocol_error.invalid_request "search has more than 128 label filters")
    else
      J.validate_limits
        ~max_depth:24
        ~max_bytes:65_536
        (Session.List_request.to_json catalog)
  in
  let%bind catalog = Session.List_request.normalize catalog in
  if catalog.page.limit > 100
  then Error (Protocol_error.invalid_request "search hit limit exceeds 100")
  else if scan_limit < 1 || scan_limit > 512
  then
    Error (Protocol_error.invalid_request "search scan_limit must be between 1 and 512")
  else if
    not
      (Session_catalog_query.Sort.equal
         catalog.sort
         { field = Created_at; direction = Ascending })
  then
    Error
      (Protocol_error.invalid_request
         "search requires chronological created_at ascending order")
  else (
    let t = { server_id; term; catalog; scan_limit } in
    Result.map
      (J.validate_limits ~max_depth:24 ~max_bytes:65_536 (to_json t))
      ~f:(fun () -> t))
;;

let server_id t = t.server_id
let term t = t.term
let catalog t = t.catalog
let scan_limit t = t.scan_limit

let of_json json =
  let open Result.Let_syntax in
  let%bind () = J.validate_limits ~max_depth:24 ~max_bytes:65_536 json in
  let%bind fields = J.fields json in
  let%bind server_id = J.required_as fields "server_id" Id.Server.of_json in
  let%bind term = J.required_as fields "term" Search_term.of_json in
  let%bind catalog = Session.List_request.of_json json in
  let%bind scan_limit =
    J.required_as fields "scan_limit" (J.bounded_int ~min:1 ~max:512)
  in
  create ~server_id ~term ~catalog ~scan_limit
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;
