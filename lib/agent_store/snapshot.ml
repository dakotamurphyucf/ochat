open Core

type t =
  { schema_version : int
  ; transaction_sequence : int64
  ; transaction_hash : string option
  ; event_sequence : int64
  ; created_at : Agent_protocol.Timestamp.t
  ; prompt_artifact : string
  ; workspace_identity : string
  ; payload : string
  }

type installed =
  { filename : string
  ; snapshot : t
  }

module Persisted = struct
  type t =
    { schema_version : int
    ; transaction_sequence : int64
    ; transaction_hash : string option
    ; event_sequence : int64
    ; created_at : string
    ; prompt_artifact : string
    ; workspace_identity : string
    ; payload : string
    }
  [@@deriving bin_io]
end

let filename transaction_sequence = sprintf "snapshot-%016Ld.bin" transaction_sequence
let current_path directory = Filename.concat directory "CURRENT"
let eio_path env path = Eio.Path.(Eio.Stdenv.fs env / path)

let to_persisted (snapshot : t) =
  Persisted.
    { schema_version = snapshot.schema_version
    ; transaction_sequence = snapshot.transaction_sequence
    ; transaction_hash = snapshot.transaction_hash
    ; event_sequence = snapshot.event_sequence
    ; created_at = Agent_protocol.Timestamp.to_string snapshot.created_at
    ; prompt_artifact = snapshot.prompt_artifact
    ; workspace_identity = snapshot.workspace_identity
    ; payload = snapshot.payload
    }
;;

let of_persisted (persisted : Persisted.t) =
  let open Result.Let_syntax in
  let%map created_at =
    Agent_protocol.Timestamp.of_string persisted.Persisted.created_at
  in
  { schema_version = persisted.schema_version
  ; transaction_sequence = persisted.transaction_sequence
  ; transaction_hash = persisted.transaction_hash
  ; event_sequence = persisted.event_sequence
  ; created_at
  ; prompt_artifact = persisted.prompt_artifact
  ; workspace_identity = persisted.workspace_identity
  ; payload = persisted.payload
  }
;;

let encode_payload snapshot =
  Bin_prot.Utils.bin_dump ~header:false Persisted.bin_writer_t (to_persisted snapshot)
  |> Bigstring.to_string
;;

let decode_payload payload =
  try
    let persisted = Bin_prot.Reader.of_string Persisted.bin_reader_t payload in
    of_persisted persisted
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.message)
  with
  | exn ->
    Error (Store_error.Corrupt ("snapshot payload decode failed: " ^ Exn.to_string exn))
;;

let frame_error = function
  | Frame.Checksum_mismatch -> Store_error.Corrupt "snapshot checksum mismatch"
  | error ->
    Store_error.Corrupt
      ("snapshot frame is invalid: " ^ Sexp.to_string_hum ([%sexp_of: Frame.error] error))
;;

let decode_file ~max_payload_length contents =
  match Frame.decode ~max_payload_length ~contents ~offset:0 with
  | Error error -> Error (frame_error error)
  | Ok (Incomplete_tail _) -> Error (Store_error.Missing "incomplete snapshot")
  | Ok (Complete { frame; next_offset }) ->
    if next_offset <> String.length contents
    then Error (Store_error.Corrupt "snapshot contains trailing bytes")
    else decode_payload (Frame.payload frame)
;;

let validate_filename filename =
  if
    String.is_prefix filename ~prefix:"snapshot-"
    && String.is_suffix filename ~suffix:".bin"
    && not (String.is_substring filename ~substring:"/")
  then Ok ()
  else Error (Store_error.Corrupt ("invalid snapshot filename: " ^ filename))
;;

let read_file ~env ~directory ~max_payload_length ~filename =
  let open Result.Let_syntax in
  let%bind () = validate_filename filename in
  let path = Filename.concat directory filename in
  try
    let contents = Eio.Path.load (eio_path env path) in
    let%map snapshot = decode_file ~max_payload_length contents in
    { filename; snapshot }
  with
  | exn -> Error (Store_error.of_exn ~operation:"read snapshot" ~path exn)
;;

let write_exclusive ~env ~path contents =
  try
    Eio.Path.with_open_out ~create:(`Exclusive 0o600) (eio_path env path) (fun flow ->
      Eio.Flow.copy_string contents flow;
      Eio.File.sync flow);
    Ok ()
  with
  | exn -> Error (Store_error.of_exn ~operation:"write snapshot" ~path exn)
;;

let install ~env ~directory ~max_payload_length snapshot =
  let open Result.Let_syntax in
  let%bind () =
    try
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (eio_path env directory);
      Ok ()
    with
    | exn ->
      Error
        (Store_error.of_exn ~operation:"create snapshot directory" ~path:directory exn)
  in
  let payload = encode_payload snapshot in
  let%bind encoded =
    Frame.encode ~max_payload_length ~flags:0 payload |> Result.map_error ~f:frame_error
  in
  let installed_filename = filename snapshot.transaction_sequence in
  let path = Filename.concat directory installed_filename in
  let%bind () = write_exclusive ~env ~path encoded in
  let%bind installed =
    read_file ~env ~directory ~max_payload_length ~filename:installed_filename
  in
  let%map () =
    Durable_file.replace
      ~env
      ~durability:Flush_file_and_directory
      ~path:(current_path directory)
      (installed_filename ^ "\n")
  in
  installed
;;

let snapshot_filenames ~env directory =
  try
    Eio.Path.read_dir (eio_path env directory)
    |> List.filter ~f:(fun name ->
      String.is_prefix name ~prefix:"snapshot-" && String.is_suffix name ~suffix:".bin")
    |> List.sort ~compare:(fun left right -> String.compare right left)
    |> Result.return
  with
  | exn -> Error (Store_error.of_exn ~operation:"list snapshots" ~path:directory exn)
;;

let fallback ~env ~directory ~max_payload_length ~excluding =
  let open Result.Let_syntax in
  let%bind filenames = snapshot_filenames ~env directory in
  let candidates =
    List.filter filenames ~f:(fun filename -> not (String.equal filename excluding))
  in
  let rec loop = function
    | [] -> Ok None
    | filename :: rest ->
      (match read_file ~env ~directory ~max_payload_length ~filename with
       | Ok installed -> Ok (Some installed)
       | Error (Store_error.Missing _) -> loop rest
       | Error error -> Error error)
  in
  loop candidates
;;

let load_current ~env ~directory ~max_payload_length =
  let current = current_path directory in
  if not (Eio.Path.is_file (eio_path env current))
  then Ok None
  else
    let open Result.Let_syntax in
    let%bind filename =
      Durable_file.load ~env ~path:current |> Result.map ~f:String.strip
    in
    match read_file ~env ~directory ~max_payload_length ~filename with
    | Ok installed -> Ok (Some installed)
    | Error (Store_error.Missing _) ->
      fallback ~env ~directory ~max_payload_length ~excluding:filename
    | Error error -> Error error
;;

let remove_snapshot ~env ~directory filename =
  let path = Filename.concat directory filename in
  try
    Eio.Path.unlink (eio_path env path);
    Ok ()
  with
  | exn -> Error (Store_error.of_exn ~operation:"prune snapshot" ~path exn)
;;

let prune_older ~env ~directory ~keep =
  if keep <= 0
  then Error (Store_error.Corrupt "snapshot retention count must be positive")
  else
    let open Result.Let_syntax in
    let%bind filenames = snapshot_filenames ~env directory in
    let removable = List.drop filenames keep in
    let%bind () =
      Result.all_unit (List.map removable ~f:(remove_snapshot ~env ~directory))
    in
    let%map () =
      if List.is_empty removable
      then Ok ()
      else Durable_file.sync_directory ~env ~path:directory
    in
    List.length removable
;;

let retention_floor ~env ~directory ~max_payload_length =
  let open Result.Let_syntax in
  let%bind filenames = snapshot_filenames ~env directory in
  match List.last filenames with
  | None -> Error (Store_error.Missing "snapshot retention anchor")
  | Some filename ->
    let%map installed = read_file ~env ~directory ~max_payload_length ~filename in
    installed.snapshot.transaction_sequence
;;
