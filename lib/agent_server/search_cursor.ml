open! Core
module P = Agent_protocol

module Position = struct
  type t =
    { session : int
    ; entry : int
    }
  [@@deriving equal, sexp_of]

  let create ~session ~entry =
    if session < 0 || session > 10000 || entry < 0 || entry > 65536
    then Error (P.Error.invalid_request "search continuation position is out of bounds")
    else Ok { session; entry }
  ;;

  let start = { session = 0; entry = 0 }
end

type t = { secret : string }

type binding =
  { authority : string
  ; query : string
  ; source : string
  }
[@@deriving equal]

let create () = { secret = P.Id.Transaction.(to_string (create ())) }
let sign t text = Digestif.SHA256.(hmac_string ~key:t.secret text |> to_hex)
let digest t json = sign t (Jsonaf.to_string json)
let same_basis = equal_binding

let invalid () =
  P.Error.create
    Cursor_expired
    ~message:"search cursor is invalid or expired"
    ~retryable:false
    ()
;;

let refresh_required () =
  P.Error.create
    Conflict
    ~message:"search source changed; refresh required"
    ~data:(`Object [ "refresh_required", `True ])
    ~retryable:false
    ()
;;

let bind t ~principal ~query ~organization_revision ~catalog =
  let open Result.Let_syntax in
  if List.length catalog > 10000
  then
    Error
      (P.Error.create
         Resource_limit
         ~message:"search catalog exceeds 10000 sessions"
         ~retryable:false
         ())
  else (
    let request = P.Search_query.catalog query in
    let%map query =
      P.Search_query.create
        ~server_id:(P.Search_query.server_id query)
        ~term:(P.Search_query.term query)
        ~catalog:{ request with page = { request.page with cursor = None } }
        ~scan_limit:(P.Search_query.scan_limit query)
    in
    { authority = digest t (P.Principal.to_json principal)
    ; query = digest t (P.Search_query.to_json query)
    ; source =
        digest
          t
          (`Array
              [ `String (Int64.to_string organization_revision)
              ; `Array (List.map catalog ~f:P.Session_catalog.to_json)
              ])
    })
;;

let unsigned binding (position : Position.t) =
  String.concat
    ~sep:":"
    [ "search-v1"
    ; binding.authority
    ; binding.query
    ; binding.source
    ; Int.to_string position.session
    ; Int.to_string position.entry
    ]
;;

let issue t binding position =
  let text = unsigned binding position in
  P.Page.Cursor.of_string (Base64.encode_exn (text ^ ":" ^ sign t text))
;;

let resolve t binding = function
  | None -> Ok Position.start
  | Some cursor ->
    (match Base64.decode (P.Page.Cursor.to_string cursor) with
     | Error _ -> Error (invalid ())
     | Ok text ->
       (match String.split text ~on:':' with
        | [ "search-v1"; authority; query; source; session; entry; signature ] ->
          let unsigned =
            String.concat
              ~sep:":"
              [ "search-v1"; authority; query; source; session; entry ]
          in
          if
            (not (String.equal signature (sign t unsigned)))
            || (not (String.equal authority binding.authority))
            || not (String.equal query binding.query)
          then Error (invalid ())
          else if not (String.equal source binding.source)
          then Error (refresh_required ())
          else (
            match Int.of_string_opt session, Int.of_string_opt entry with
            | Some session, Some entry -> Position.create ~session ~entry
            | _ -> Error (invalid ()))
        | _ -> Error (invalid ())))
;;
