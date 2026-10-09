open! Core
module P = Agent_protocol
module State = Agent_session.Session_state
module D = Document_schema

type t =
  { session : P.Session_ref.t
  ; generation : int
  ; revision : int64
  ; entries : P.History.entry list
  ; length : int
  ; initial_prefix : int
  }

let resource message = P.Error.create Resource_limit ~message ~retryable:false ()

let create ~server_id (state : State.t) =
  let rec bounded_length count = function
    | [] -> Ok count
    | _ :: _ when count = 65536 -> Error (resource "search source exceeds 65536 entries")
    | _ :: rest -> bounded_length (count + 1) rest
  in
  let open Result.Let_syntax in
  let entries = state.conversation.canonical_history in
  let%bind length = bounded_length 0 entries in
  let initial_prefix = state.conversation.initial_prompt_entry_count in
  if initial_prefix < 0 || initial_prefix > length
  then
    Error
      (P.Error.create
         Persistence_error
         ~message:"invalid initial search prefix"
         ~retryable:false
         ())
  else
    Ok
      { session = P.Session_ref.create ~server_id ~session_id:state.identity.session_id
      ; generation = state.identity.generation
      ; revision = state.counters.revision
      ; entries
      ; length
      ; initial_prefix
      }
;;

let session t = t.session
let generation t = t.generation
let revision t = t.revision
let length t = t.length

let index_of_id t id =
  List.find_mapi t.entries ~f:(fun index entry ->
    if index >= t.initial_prefix && P.History.Id.equal entry.P.History.id id
    then Some index
    else None)
;;

module Window = struct
  type t =
    { entries : Search_entry.t option list
    ; next_index : int
    ; scanned_entries : int
    ; scanned_bytes : int
    ; reached_end : bool
    }
end

let source_error = function
  | D.Error.Limit_exceeded _ -> resource "search source payload exceeds its work limit"
  | Invalid_configuration _
  | Malformed _
  | Duplicate_key _
  | Unsupported_beta_format
  | Unsupported_format _
  | Invalid_field _
  | Unsupported_kind _
  | Unsupported_version _
  | Missing_conversion _
  | Wrong_kind _
  | Wrong_version _
  | Required_semantics_unknown _
  | Extension_conflict _ ->
    P.Error.create
      Persistence_error
      ~message:"malformed canonical search payload"
      ~retryable:false
      ()
;;

let payload_limits =
  D.Limits.create ~max_bytes:2_097_152 ~max_depth:64 ~max_fields:32768 ~max_nodes:65536
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let project t ~principal ~cache ~offset ~limit ~max_bytes =
  let open Result.Let_syntax in
  if
    offset < 0
    || offset > t.length
    || limit < 1
    || limit > 512
    || max_bytes < 1
    || max_bytes > 8_388_608
  then Error (P.Error.invalid_request "invalid search source window")
  else if not (P.Principal.has_scope principal View_session_transcript)
  then
    Error
      (P.Error.create
         Permission_denied
         ~message:"conversation search requires transcript access"
         ~retryable:false
         ())
  else (
    let scope_identity = Principal_projection.scope_identity principal in
    let finish index bytes reversed =
      Ok
        Window.
          { entries = List.rev reversed
          ; next_index = index
          ; scanned_entries = index - offset
          ; scanned_bytes = bytes
          ; reached_end = index = t.length
          }
    in
    let rec scan remaining index bytes reversed =
      if index - offset = limit
      then finish index bytes reversed
      else (
        match remaining with
        | [] -> finish index bytes reversed
        | _ :: rest when index < t.initial_prefix ->
          scan rest (index + 1) bytes (None :: reversed)
        | entry :: rest ->
          let%bind size =
            D.Json.validate_and_measure ~limits:payload_limits entry.P.History.payload
            |> Result.map_error ~f:source_error
          in
          if size > max_bytes - bytes
          then finish index bytes reversed
          else (
            let%bind key =
              Search_cache.Key.create
                ~scope_identity
                ~session:t.session
                ~generation:t.generation
                ~session_revision:t.revision
                ~canonical_index:index
            in
            let%bind projected =
              match Search_cache.find cache key with
              | `Hit value -> Ok value
              | `Miss ->
                let%bind public =
                  Principal_projection.readable_history_entry principal entry
                in
                let%map projected = Search_entry.of_public public in
                Search_cache.add cache key projected;
                projected
            in
            scan rest (index + 1) (bytes + size) (projected :: reversed)))
    in
    scan (List.drop t.entries offset) offset 0 [])
;;
