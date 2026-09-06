open Core

module Persisted = struct
  type t =
    { previous_hash : string option
    ; record_json : string
    ; record_hash : string
    }
  [@@deriving bin_io]
end

type t =
  { env : Eio_unix.Stdenv.base
  ; segment : Journal_segment.t
  ; max_payload_length : int
  ; cursor_secret : string
  ; mutex : Eio.Mutex.t
  ; mutable records : Agent_protocol.Audit.t list
  ; mutable previous_hash : string option
  ; mutable next_sequence : int64
  }

let segment_id = Journal_segment.Id.first
let secret_file directory = Filename.concat directory "cursor-secret"

let encode_record record =
  Agent_protocol.Audit.to_json record
  |> Agent_protocol.Json_codec.canonical_string
  |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
;;

let record_hash previous_hash record_json =
  let previous_hash = Option.value previous_hash ~default:"" in
  Digestif.SHA256.digest_string (previous_hash ^ "\000" ^ record_json)
  |> Digestif.SHA256.to_hex
;;

let encode_persisted persisted =
  Bin_prot.Utils.bin_dump ~header:false Persisted.bin_writer_t persisted
  |> Bigstring.to_string
;;

let decode_persisted encoded =
  try Ok (Bin_prot.Reader.of_string Persisted.bin_reader_t encoded) with
  | exn ->
    Error (Store_error.Corrupt ("audit record decode failed: " ^ Exn.to_string exn))
;;

let decode_record encoded =
  try
    Jsonaf.of_string encoded
    |> Agent_protocol.Audit.of_json
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
  with
  | exn -> Error (Store_error.Corrupt ("audit JSON decode failed: " ^ Exn.to_string exn))
;;

let validate_persisted previous_hash next_sequence persisted =
  let open Result.Let_syntax in
  if not (Option.equal String.equal persisted.Persisted.previous_hash previous_hash)
  then Error (Store_error.Corrupt "audit hash chain is discontinuous")
  else if
    not
      (String.equal
         persisted.record_hash
         (record_hash previous_hash persisted.record_json))
  then Error (Store_error.Corrupt "audit record hash does not match")
  else (
    let%bind record = decode_record persisted.record_json in
    if not (Int64.equal record.sequence next_sequence)
    then Error (Store_error.Corrupt "audit sequence is discontinuous")
    else Ok record)
;;

let recover_entries entries =
  let rec loop previous_hash next_sequence records = function
    | [] -> Ok (List.rev records, previous_hash, next_sequence)
    | entry :: rest ->
      let open Result.Let_syntax in
      let%bind persisted =
        Frame.payload entry.Journal_segment.frame |> decode_persisted
      in
      let%bind record = validate_persisted previous_hash next_sequence persisted in
      loop
        (Some persisted.record_hash)
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
  | exn ->
    Error
      (Store_error.Io
         { operation = "create audit directory"
         ; path = directory
         ; message = Exn.to_string exn
         })
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
  else if max_payload_length <= 0
  then Error (Store_error.Corrupt "audit payload limit must be positive")
  else (
    let%bind () = ensure_directory env directory in
    let%bind segment = open_segment env directory in
    let%bind scan = Journal_segment.scan ~env ~max_payload_length segment in
    let%bind () = repair_tail env segment scan in
    let%bind records, previous_hash, next_sequence = recover_entries scan.entries in
    let%map cursor_secret = load_or_create_secret env directory in
    { env
    ; segment
    ; max_payload_length
    ; cursor_secret
    ; mutex = Eio.Mutex.create ()
    ; records
    ; previous_hash
    ; next_sequence
    })
;;

let append_locked t ~timestamp ~level ~name ~session_id ~principal_id ~payload ~redacted =
  let open Result.Let_syntax in
  let record =
    Agent_protocol.Audit.
      { sequence = t.next_sequence
      ; timestamp
      ; level
      ; name
      ; session_id
      ; principal_id
      ; payload
      ; redacted
      }
  in
  let%bind record_json = encode_record record in
  let record_hash = record_hash t.previous_hash record_json in
  let persisted =
    Persisted.{ previous_hash = t.previous_hash; record_json; record_hash }
  in
  let%bind frame =
    Frame.encode
      ~max_payload_length:t.max_payload_length
      ~flags:0
      (encode_persisted persisted)
    |> Result.map_error ~f:(fun error ->
      Store_error.Corrupt ([%sexp_of: Frame.error] error |> Sexp.to_string_hum))
  in
  let%map _ = Journal_segment.append ~env:t.env ~durability:Flush t.segment ~frame in
  t.records <- t.records @ [ record ];
  t.previous_hash <- Some record_hash;
  t.next_sequence <- Int64.(t.next_sequence + 1L);
  record
;;

let append t ~timestamp ~level ~name ~session_id ~principal_id ~payload ~redacted =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    append_locked t ~timestamp ~level ~name ~session_id ~principal_id ~payload ~redacted)
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
  let%bind after_sequence =
    decode_cursor t request.Agent_protocol.Audit.Read_request.page.cursor
  in
  let matching = List.filter t.records ~f:(matches request after_sequence) in
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
