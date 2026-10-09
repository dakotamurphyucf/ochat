open Core

module Snapshot = struct
  type t =
    { records : Agent_protocol.Audit.t list
    ; previous_hash : string option
    ; next_sequence : int64
    }
end

type availability =
  | Available of Snapshot.t
  | Unavailable of Store_error.t

type t =
  { env : Eio_unix.Stdenv.base
  ; segment : Journal_segment.t
  ; max_payload_length : int
  ; limits : Document_schema.Limits.t
  ; cursor_secret : string
  ; mutex : Eio.Mutex.t
  ; mutable availability : availability
  }

let segment_id = Journal_segment.Id.first
let secret_file directory = Filename.concat directory "cursor-secret"

let available t =
  match t.availability with
  | Available snapshot -> Ok snapshot
  | Unavailable failure -> Error failure
;;

let recover_entries entries ~limits =
  let rec loop previous_hash next_sequence records = function
    | [] -> Ok Snapshot.{ records = List.rev records; previous_hash; next_sequence }
    | entry :: rest ->
      let open Result.Let_syntax in
      let frame = entry.Journal_segment.frame in
      let%bind () =
        if Frame.flags frame = 0
        then Ok ()
        else Error (Store_error.Corrupt "audit frame flags must be zero")
      in
      let%bind named =
        Document_record.of_frame frame ~limits ~expected_digest:None
        |> Result.map_error ~f:Document_fields.record_error
      in
      let%bind evidence =
        Audit_evidence_document.of_document
          (Document_record.document named)
          ~previous_hash
          ~next_sequence
          ~limits
      in
      let record = Audit_event_document.value (Audit_evidence_document.event evidence) in
      let%bind () =
        if Int64.equal next_sequence Int64.max_value
        then Error (Store_error.Corrupt "audit sequence overflow")
        else Ok ()
      in
      loop
        (Some (Audit_evidence_document.record_hash evidence))
        Int64.(next_sequence + 1L)
        (record :: records)
        rest
  in
  loop None 1L [] entries
;;

let random_secret () =
  let create () =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  create () ^ create ()
;;

let load_or_create_secret env directory =
  let path = secret_file directory in
  match Durable_file.load ~env ~path with
  | Ok secret when not (String.is_empty secret) -> Ok secret
  | Ok _ -> Error (Store_error.Corrupt "audit cursor secret is empty")
  | Error (Missing _) ->
    let secret = random_secret () in
    Result.map
      (Durable_file.replace ~env ~durability:Flush_file_and_directory ~path secret)
      ~f:(fun () -> secret)
  | Error _ as failure -> failure
;;

let open_segment env directory =
  let path = Filename.concat directory (Journal_segment.Id.filename segment_id) in
  if Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / path)
  then Journal_segment.open_existing ~env ~directory ~id:segment_id
  else Journal_segment.create_exclusive ~env ~directory ~id:segment_id
;;

let repair_tail env segment scan =
  if scan.Journal_segment.crash_tail
  then Journal_segment.truncate_crash_tail ~env segment scan
  else Ok ()
;;

let ensure_directory env directory =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / directory);
    Ok ()
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error
      (Store_error.Io
         { operation = "create audit directory"
         ; path = directory
         ; message = Exn.to_string exn
         })
;;

let recover_snapshot ~env segment ~max_payload_length ~limits =
  let open Result.Let_syntax in
  let%bind scan = Journal_segment.scan ~env ~max_payload_length segment in
  (* Complete framed semantics must be admitted before modifying a torn tail. *)
  let%bind snapshot = recover_entries scan.entries ~limits in
  let%map () = repair_tail env segment scan in
  snapshot
;;

let open_or_create ~env ~directory ~max_payload_length =
  let open Result.Let_syntax in
  if not (Filename.is_absolute directory)
  then
    Error
      (Store_error.Io
         { operation = "open audit store"
         ; path = directory
         ; message = "path must be absolute"
         })
  else (
    let%bind limits =
      Document_fields.limits ~max_bytes:max_payload_length |> Document_fields.store
    in
    let%bind () = ensure_directory env directory in
    let%bind segment = open_segment env directory in
    let%bind snapshot = recover_snapshot ~env segment ~max_payload_length ~limits in
    let%map cursor_secret = load_or_create_secret env directory in
    { env
    ; segment
    ; max_payload_length
    ; limits
    ; cursor_secret
    ; mutex = Eio.Mutex.create ()
    ; availability = Available snapshot
    })
;;

let reconcile_after_failure t failure =
  t.availability <- Unavailable failure;
  (* This entire journal is the authority. Either fully verified old or new
     snapshot is honest; failed secondary recovery leaves reads unavailable. *)
  Eio.Cancel.protect (fun () ->
    try
      match
        recover_snapshot
          ~env:t.env
          t.segment
          ~max_payload_length:t.max_payload_length
          ~limits:t.limits
      with
      | Ok snapshot -> t.availability <- Available snapshot
      | Error _ -> ()
    with
    | _ -> ())
;;

let append_locked t ~timestamp ~level ~name ~session_id ~principal_id ~payload ~redacted =
  let open Result.Let_syntax in
  let%bind snapshot = available t in
  let%bind () =
    if Int64.equal snapshot.next_sequence Int64.max_value
    then Error (Store_error.Corrupt "audit sequence overflow")
    else Ok ()
  in
  let record =
    Agent_protocol.Audit.
      { sequence = snapshot.next_sequence
      ; timestamp
      ; level
      ; name
      ; session_id
      ; principal_id
      ; payload
      ; redacted
      }
  in
  let%bind event =
    Audit_event_document.create record ~limits:t.limits |> Document_fields.store
  in
  let%bind evidence =
    Audit_evidence_document.create
      event
      ~previous_hash:snapshot.previous_hash
      ~limits:t.limits
    |> Document_fields.store
  in
  let%bind document =
    Audit_evidence_document.to_document evidence ~limits:t.limits |> Document_fields.store
  in
  let%bind frame =
    Document_record.encode document ~limits:t.limits ~flags:0
    |> Result.map_error ~f:Document_fields.record_error
  in
  try
    match Journal_segment.append ~env:t.env ~durability:Flush t.segment ~frame with
    | Error failure ->
      reconcile_after_failure t failure;
      Error failure
    | Ok _ ->
      t.availability
      <- Available
           Snapshot.
             { records = snapshot.records @ [ record ]
             ; previous_hash = Some (Audit_evidence_document.record_hash evidence)
             ; next_sequence = Int64.(snapshot.next_sequence + 1L)
             };
      Ok record
  with
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    reconcile_after_failure
      t
      (Store_error.of_exn
         ~operation:"append audit evidence"
         ~path:(Journal_segment.path t.segment)
         exn);
    Exn.raise_with_original_backtrace exn backtrace
;;

let append t ~timestamp ~level ~name ~session_id ~principal_id ~payload ~redacted =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      try
        Ok
          (append_locked
             t
             ~timestamp
             ~level
             ~name
             ~session_id
             ~principal_id
             ~payload
             ~redacted)
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let cursor_signature secret sequence =
  Digestif.SHA256.digest_string (secret ^ "\000" ^ Int64.to_string sequence)
  |> Digestif.SHA256.to_hex
;;

let encode_cursor t sequence =
  sprintf "%Ld.%s" sequence (cursor_signature t.cursor_secret sequence)
  |> Agent_protocol.Page.Cursor.of_string
  |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
;;

let decode_cursor t = function
  | None -> Ok 0L
  | Some cursor ->
    let encoded = Agent_protocol.Page.Cursor.to_string cursor in
    (match String.lsplit2 encoded ~on:'.' with
     | None -> Error (Store_error.Corrupt "audit cursor is malformed")
     | Some (sequence_text, signature) ->
       (match Int64.of_string sequence_text with
        | sequence when Int64.(sequence >= 0L) ->
          if String.equal signature (cursor_signature t.cursor_secret sequence)
          then Ok sequence
          else Error (Store_error.Corrupt "audit cursor signature is invalid")
        | _ -> Error (Store_error.Corrupt "audit cursor sequence is invalid")
        | exception _ -> Error (Store_error.Corrupt "audit cursor sequence is invalid")))
;;

let level_rank = function
  | Agent_protocol.Audit.Info -> 0
  | Warning -> 1
  | Error -> 2
;;

let matches
      (request : Agent_protocol.Audit.Read_request.t)
      after_sequence
      (record : Agent_protocol.Audit.t)
  =
  Int64.(record.Agent_protocol.Audit.sequence > after_sequence)
  && Option.value_map request.session_id ~default:true ~f:(fun session_id ->
    Option.value_map record.session_id ~default:false ~f:(fun candidate ->
      Agent_protocol.Id.Session.compare session_id candidate = 0))
  && Option.value_map request.principal_id ~default:true ~f:(fun principal_id ->
    Option.value_map record.principal_id ~default:false ~f:(fun candidate ->
      Agent_protocol.Id.Principal.compare principal_id candidate = 0))
  && Option.value_map request.minimum_level ~default:true ~f:(fun minimum ->
    level_rank record.level >= level_rank minimum)
  && Option.value_map request.name_prefix ~default:true ~f:(fun prefix ->
    String.is_prefix record.name ~prefix)
;;

let read_locked t request =
  let open Result.Let_syntax in
  let%bind snapshot = available t in
  let%bind after_sequence =
    decode_cursor t request.Agent_protocol.Audit.Read_request.page.cursor
  in
  let matching = List.filter snapshot.records ~f:(matches request after_sequence) in
  let items = List.take matching request.page.limit in
  let%map next_cursor =
    match List.last items with
    | Some last when List.length matching > List.length items ->
      Result.map (encode_cursor t last.sequence) ~f:Option.some
    | Some _ | None -> Ok None
  in
  Agent_protocol.Page.{ items; next_cursor }
;;

let read t request = Eio.Mutex.use_ro t.mutex (fun () -> read_locked t request)
