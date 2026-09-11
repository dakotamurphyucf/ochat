open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store

type t = { secret : string }

type query =
  | All_outputs
  | Submission of P.History.Id.t
[@@deriving equal, sexp]

type context =
  { relationship : D.Reference.t
  ; generation : int
  ; compaction_generation : int
  ; history_epoch : Agent_session.Durable_event_log.history_epoch
  ; access_revision : string
  ; query : query
  }

type position =
  { entry : int
  ; byte : int
  }
[@@deriving equal, sexp]

let create () = { secret = P.Id.Transaction.(to_string (create ())) }
let digest text = Digestif.SHA256.(digest_string text |> to_hex)
let sign t text = Digestif.SHA256.(hmac_string ~key:t.secret text |> to_hex)

let expired () =
  P.Error.create
    Cursor_expired
    ~message:"The output cursor is invalid or expired; request a fresh bounded snapshot."
    ~retryable:false
    ~data:(`Object [ "snapshot_required", `True ])
    ()
;;

let binding context =
  [%sexp
    ("ochat.managed-output.v1" : string)
  , (context.relationship : D.Reference.t)
  , (context.generation : int)
  , (context.compaction_generation : int)
  , (context.history_epoch : Agent_session.Durable_event_log.history_epoch)
  , (context.access_revision : string)
  , (context.query : query)]
  |> Sexp.to_string_mach
  |> digest
;;

let anchors entries position =
  let rec loop index prefix = function
    | remaining when Int.equal index position.entry ->
      let current =
        match position.byte, remaining with
        | 0, _ -> Some "-"
        | byte, text :: _ when byte > 0 && byte < String.length text -> Some (digest text)
        | _ -> None
      in
      Option.map current ~f:(fun current ->
        Digestif.SHA256.(get prefix |> to_hex), current)
    | text :: rest when index < position.entry ->
      (* Fixed-length digest frames avoid ambiguous string concatenations without
         retaining a second concatenation of the entire output transcript. *)
      loop (index + 1) (Digestif.SHA256.feed_string prefix (digest text)) rest
    | _ -> None
  in
  match position.entry >= 0 && position.byte >= 0 with
  | true -> loop 0 (Digestif.SHA256.init ()) entries
  | false -> None
;;

let issue t ~context ~entries position =
  match anchors entries position with
  | None ->
    Error (P.Error.invalid_request "output position is outside the retained entries")
  | Some (prefix, current) ->
    let payload =
      String.concat
        ~sep:":"
        [ "1"
        ; binding context
        ; Int.to_string position.entry
        ; Int.to_string position.byte
        ; prefix
        ; current
        ]
    in
    P.Page.Cursor.of_string (Base64.encode_exn (payload ^ ":" ^ sign t payload))
;;

let resolve t ~context ~entries cursor =
  match cursor with
  | None -> Ok { entry = 0; byte = 0 }
  | Some cursor ->
    let open Result.Let_syntax in
    let%bind decoded =
      Base64.decode (P.Page.Cursor.to_string cursor)
      |> Result.map_error ~f:(fun _ -> expired ())
    in
    (match String.split decoded ~on:':' with
     | [ "1"; actual_binding; entry; byte; prefix; current; signature ] ->
       let payload =
         String.concat ~sep:":" [ "1"; actual_binding; entry; byte; prefix; current ]
       in
       (match
          ( String.equal actual_binding (binding context)
            && String.equal signature (sign t payload)
          , Int.of_string_opt entry
          , Int.of_string_opt byte )
        with
        | true, Some entry, Some byte ->
          let position = { entry; byte } in
          (match anchors entries position with
           | Some (expected_prefix, expected_current)
             when String.equal prefix expected_prefix
                  && String.equal current expected_current -> Ok position
           | _ -> Error (expired ()))
        | _ -> Error (expired ()))
     | _ -> Error (expired ()))
;;
