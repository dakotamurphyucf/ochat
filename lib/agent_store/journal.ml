open Core

type t =
  { env : Eio_unix.Stdenv.base
  ; directory : string
  ; current_path : string
  ; max_payload_length : int
  ; max_segment_bytes : int64
  ; max_segment_frames : int
  ; mutable current : Journal_segment.t
  ; mutable current_bytes : int64
  ; mutable current_frames : int
  }

type entry =
  { segment_id : Journal_segment.Id.t
  ; offset : int64
  ; next_offset : int64
  ; frame : Frame.t
  }

type scan =
  { entries : entry list
  ; current_segment : Journal_segment.Id.t
  ; crash_tail : (Journal_segment.Id.t * int64) option
  }

type append_result =
  { segment_id : Journal_segment.Id.t
  ; offset : int64
  ; next_offset : int64
  ; checksum_hex : string
  }

let current_segment t = Journal_segment.id t.current
let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)
let current_path directory = Filename.concat directory "CURRENT"

let validate_limits ~max_payload_length ~max_segment_bytes ~max_segment_frames =
  if max_payload_length <= 0
  then Error (Store_error.Corrupt "maximum journal payload length must be positive")
  else if Int64.(max_segment_bytes <= zero)
  then Error (Store_error.Corrupt "maximum journal segment size must be positive")
  else if max_segment_frames <= 0
  then Error (Store_error.Corrupt "maximum journal segment frame count must be positive")
  else Ok ()
;;

let write_current ~env path id =
  Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path
    (Journal_segment.Id.filename id ^ "\n")
;;

let make
      ~env
      ~directory
      ~max_payload_length
      ~max_segment_bytes
      ~max_segment_frames
      ~current
      ~current_bytes
      ~current_frames
  =
  { env
  ; directory
  ; current_path = current_path directory
  ; max_payload_length
  ; max_segment_bytes
  ; max_segment_frames
  ; current
  ; current_bytes
  ; current_frames
  }
;;

let create ~env ~directory ~max_payload_length ~max_segment_bytes ~max_segment_frames =
  let open Result.Let_syntax in
  let%bind () =
    validate_limits ~max_payload_length ~max_segment_bytes ~max_segment_frames
  in
  let%bind () =
    if not (Filename.is_absolute directory)
    then
      Error
        (Store_error.Io
           { operation = "create journal"
           ; path = directory
           ; message = "path must be absolute"
           })
    else (
      try
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path env directory);
        Ok ()
      with
      | exn -> Error (Store_error.of_exn ~operation:"create journal" ~path:directory exn))
  in
  let%bind current =
    Journal_segment.create_exclusive ~env ~directory ~id:Journal_segment.Id.first
  in
  let%map () = write_current ~env (current_path directory) Journal_segment.Id.first in
  make
    ~env
    ~directory
    ~max_payload_length
    ~max_segment_bytes
    ~max_segment_frames
    ~current
    ~current_bytes:Int64.zero
    ~current_frames:0
;;

let read_current ~env path =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env ~path in
  Journal_segment.Id.of_filename (String.strip contents)
;;

let open_existing
      ~env
      ~directory
      ~max_payload_length
      ~max_segment_bytes
      ~max_segment_frames
  =
  let open Result.Let_syntax in
  let%bind () =
    validate_limits ~max_payload_length ~max_segment_bytes ~max_segment_frames
  in
  let%bind current_id = read_current ~env (current_path directory) in
  let%bind current = Journal_segment.open_existing ~env ~directory ~id:current_id in
  let%map current_scan = Journal_segment.scan ~env ~max_payload_length current in
  make
    ~env
    ~directory
    ~max_payload_length
    ~max_segment_bytes
    ~max_segment_frames
    ~current
    ~current_bytes:current_scan.valid_length
    ~current_frames:(List.length current_scan.entries)
;;

let frame_error error =
  Store_error.Corrupt
    ("journal frame encode failed: " ^ Sexp.to_string_hum ([%sexp_of: Frame.error] error))
;;

let should_rotate t =
  Int64.(t.current_bytes >= t.max_segment_bytes)
  || t.current_frames >= t.max_segment_frames
;;

let append_frame t ~durability encoded =
  let open Result.Let_syntax in
  let%map offset, next_offset =
    Journal_segment.append ~env:t.env ~durability t.current ~frame:encoded
  in
  t.current_bytes <- next_offset;
  t.current_frames <- t.current_frames + 1;
  { segment_id = Journal_segment.id t.current
  ; offset
  ; next_offset
  ; checksum_hex =
      (match
         Frame.decode ~max_payload_length:t.max_payload_length ~contents:encoded ~offset:0
       with
       | Ok (Complete { frame; _ }) -> Frame.checksum_hex frame
       | Ok (Incomplete_tail _) -> assert false
       | Error _ -> assert false)
  }
;;

let encode t ~flags payload =
  Frame.encode ~max_payload_length:t.max_payload_length ~flags payload
  |> Result.map_error ~f:frame_error
;;

let rotate t ~terminal_payload =
  let open Result.Let_syntax in
  let%bind terminal = encode t ~flags:1 terminal_payload in
  let%bind (_ : append_result) = append_frame t ~durability:Flush terminal in
  let%bind next_id = Journal_segment.Id.next (Journal_segment.id t.current) in
  let%bind next =
    Journal_segment.create_exclusive ~env:t.env ~directory:t.directory ~id:next_id
  in
  let%map () = write_current ~env:t.env t.current_path next_id in
  t.current <- next;
  t.current_bytes <- Int64.zero;
  t.current_frames <- 0
;;

let append t ~durability ~flags ~payload =
  let open Result.Let_syntax in
  let%bind encoded = encode t ~flags payload in
  let%bind result = append_frame t ~durability encoded in
  let%map () =
    if should_rotate t then rotate t ~terminal_payload:"segment sealed" else Ok ()
  in
  result
;;

let seal_checkpoint t =
  if t.current_frames = 0 then Ok () else rotate t ~terminal_payload:"checkpoint boundary"
;;

let segment_ids t =
  try
    Eio.Path.read_dir (eio_path t.env t.directory)
    |> List.filter_map ~f:(fun name ->
      if String.is_suffix name ~suffix:".log"
      then Result.ok (Journal_segment.Id.of_filename name)
      else None)
    |> List.sort ~compare:Journal_segment.Id.compare
    |> Result.return
  with
  | exn ->
    Error (Store_error.of_exn ~operation:"list journal segments" ~path:t.directory exn)
;;

let scan_segment t id =
  let open Result.Let_syntax in
  let%bind segment =
    Journal_segment.open_existing ~env:t.env ~directory:t.directory ~id
  in
  let%map scan =
    Journal_segment.scan ~env:t.env ~max_payload_length:t.max_payload_length segment
  in
  segment, scan
;;

let scan t =
  let open Result.Let_syntax in
  let%bind ids = segment_ids t in
  let%bind scans = Result.all (List.map ids ~f:(scan_segment t)) in
  let current_id = Journal_segment.id t.current in
  let noncurrent_tail =
    List.find scans ~f:(fun (segment, scan) ->
      scan.crash_tail
      && not (Journal_segment.Id.equal (Journal_segment.id segment) current_id))
  in
  match noncurrent_tail with
  | Some (segment, _) ->
    Error
      (Store_error.Corrupt
         ("incomplete frame in sealed segment "
          ^ Journal_segment.Id.filename (Journal_segment.id segment)))
  | None ->
    let entries =
      List.concat_map scans ~f:(fun (segment, scan) ->
        List.map scan.entries ~f:(fun entry ->
          { segment_id = Journal_segment.id segment
          ; offset = entry.offset
          ; next_offset = entry.next_offset
          ; frame = entry.frame
          }))
    in
    let crash_tail =
      List.find_map scans ~f:(fun (segment, scan) ->
        if scan.crash_tail
        then Some (Journal_segment.id segment, scan.valid_length)
        else None)
    in
    Ok { entries; current_segment = current_id; crash_tail }
;;

let repair_current_tail t scan =
  match scan.crash_tail with
  | None -> Error (Store_error.Corrupt "journal has no recoverable crash tail")
  | Some (segment_id, valid_length) ->
    if not (Journal_segment.Id.equal segment_id (Journal_segment.id t.current))
    then Error (Store_error.Corrupt "refusing to repair a non-current journal segment")
    else
      let open Result.Let_syntax in
      let%bind current_scan =
        Journal_segment.scan ~env:t.env ~max_payload_length:t.max_payload_length t.current
      in
      if not (Int64.equal current_scan.valid_length valid_length)
      then Error (Store_error.Corrupt "journal changed after recovery scan")
      else (
        let%map () =
          Journal_segment.truncate_crash_tail ~env:t.env t.current current_scan
        in
        t.current_bytes <- valid_length;
        t.current_frames <- List.length current_scan.entries)
;;

let transaction_sequence entry =
  match Frame.flags entry.Journal_segment.frame with
  | 0 ->
    Transaction.decode (Frame.payload entry.frame)
    |> Result.map ~f:(fun transaction -> Some transaction.transaction_sequence)
  | 1 -> Ok None
  | flags -> Error (Store_error.Corrupt (sprintf "unknown journal frame flags: %d" flags))
;;

let segment_transactions t id =
  let open Result.Let_syntax in
  let%bind segment, scan = scan_segment t id in
  let%map sequences = List.map scan.entries ~f:transaction_sequence |> Result.all in
  segment, List.filter_opt sequences
;;

let find_anchor_segment t ids transaction_sequence =
  let open Result.Let_syntax in
  let%bind segments = Result.all (List.map ids ~f:(segment_transactions t)) in
  List.find_map segments ~f:(fun (segment, sequences) ->
    Option.some_if
      (List.mem sequences transaction_sequence ~equal:Int64.equal)
      (Journal_segment.id segment))
  |> Result.of_option
       ~error:
         (Store_error.Corrupt
            "snapshot transaction is absent from retained journal segments")
;;

let remove_segment t id =
  let path = Filename.concat t.directory (Journal_segment.Id.filename id) in
  try
    Eio.Path.unlink (eio_path t.env path);
    Ok ()
  with
  | exn -> Error (Store_error.of_exn ~operation:"prune journal segment" ~path exn)
;;

let prune_before_transaction t ~transaction_sequence =
  if Int64.(transaction_sequence <= zero)
  then Ok 0
  else
    let open Result.Let_syntax in
    let%bind ids = segment_ids t in
    let%bind anchor = find_anchor_segment t ids transaction_sequence in
    let removable =
      List.take_while ids ~f:(fun id -> Journal_segment.Id.compare id anchor < 0)
      |> List.filter ~f:(fun id ->
        not (Journal_segment.Id.equal id (Journal_segment.id t.current)))
    in
    let%bind () = Result.all_unit (List.map removable ~f:(remove_segment t)) in
    let%map () =
      if List.is_empty removable
      then Ok ()
      else Durable_file.sync_directory ~env:t.env ~path:t.directory
    in
    List.length removable
;;
