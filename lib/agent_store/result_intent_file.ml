open! Core
module P = Agent_protocol

type t =
  | Conversation of
      { env : Eio_unix.Stdenv.base
      ; session : Session_store.Handle.t
      ; blob : P.Id.Blob.t
      }
  | Developer of
      { root : Developer_run_store.Result_root.t
      ; owner : P.Execution_owner.developer
      ; job : P.Id.Job.t
      ; attempt : int
      }

let conversation ~env ~session ~blob = Conversation { env; session; blob }

let developer ~root ~(owner : P.Execution_owner.developer) ~job ~attempt =
  if
    attempt <= 0
    || (not
          (P.Id.Server.equal owner.host_id (Developer_run_store.Result_root.host_id root)))
    || (not
          (P.Id.Developer_run.equal
             owner.run_id
             (Developer_run_store.Result_root.run_id root)))
    || not
         (P.Id.Principal.equal
            owner.principal_id
            (Developer_run_store.Result_root.principal_id root))
  then Error (Store_error.Corrupt "developer intent key is foreign or invalid")
  else Ok (Developer { root; owner; job; attempt })
;;

let corrupt message = Error (Store_error.Corrupt message)
let path env native_path = Eio.Path.(Eio.Stdenv.fs env / native_path)
let max_supported_frame_bytes = 128 * 1024

let validate_max max_frame_bytes =
  if max_frame_bytes > 0 && max_frame_bytes <= max_supported_frame_bytes
  then Ok ()
  else corrupt "result intent frame limit is invalid"
;;

let validate_frame contents ~max_frame_bytes =
  let open Result.Let_syntax in
  let%bind () = validate_max max_frame_bytes in
  if String.length contents > max_frame_bytes
  then corrupt "result intent frame exceeds its size limit"
  else (
    match Frame.decode ~max_payload_length:max_frame_bytes ~contents ~offset:0 with
    | Ok (Complete { frame; next_offset })
      when Int.equal next_offset (String.length contents)
           && Int.equal (Frame.flags frame) 0 -> Ok frame
    | _ -> corrupt "result intent frame is incomplete or corrupt")
;;

let encode document ~limits =
  Document_record.encode document ~limits ~flags:0
  |> Result.map_error ~f:Document_fields.record_error
;;

let decode contents ~limits ~max_frame_bytes =
  let open Result.Let_syntax in
  let%bind frame = validate_frame contents ~max_frame_bytes in
  let%map record =
    Document_record.of_frame frame ~limits ~expected_digest:None
    |> Result.map_error ~f:Document_fields.record_error
  in
  Document_record.document record
;;

let with_location t ~f =
  match t with
  | Conversation { env; session; blob } ->
    let base = Session_store.Handle.directory session in
    let directory = Filename.concat base "result-preparations" in
    f
      ~env
      ~parents:[ base; directory ]
      ~directory
      ~filename:(P.Id.Blob.to_string blob ^ ".frame")
  | Developer { root; owner; job; attempt } ->
    Developer_run_store.Result_root.with_storage_lease root ~f:(fun ~env ~root ->
      let base = Data_root.developer_result_intents_path root in
      let run =
        Filename.concat base ("run_" ^ P.Id.Developer_run.to_string owner.run_id)
      in
      let epoch = Filename.concat run ("epoch_" ^ Int.to_string owner.epoch) in
      let filename =
        "job_" ^ P.Id.Job.to_string job ^ "-" ^ Int.to_string attempt ^ ".frame"
      in
      f ~env ~parents:[ Data_root.path root; base; run; epoch ] ~directory:epoch ~filename)
;;

let check_parents ~env parents =
  match parents with
  | [] -> corrupt "result intent has no owned root"
  | root :: children ->
    (match Eio.Path.kind ~follow:false (path env root) with
     | `Directory ->
       List.fold_result children ~init:true ~f:(fun present native ->
         if not present
         then Ok false
         else (
           match Eio.Path.kind ~follow:false (path env native) with
           | `Directory -> Ok true
           | `Not_found -> Ok false
           | _ -> corrupt "result intent parent is linked or invalid"))
     | `Not_found -> corrupt "result intent owned root is missing"
     | _ -> corrupt "result intent owned root is linked or invalid")
;;

let ensure_parents ~env parents =
  match parents with
  | [] -> corrupt "result intent has no owned root"
  | root :: children ->
    (match Eio.Path.kind ~follow:false (path env root) with
     | `Directory ->
       List.fold_result children ~init:() ~f:(fun () native ->
         let open Result.Let_syntax in
         let%bind () =
           match Eio.Path.kind ~follow:false (path env native) with
           | `Directory -> Ok ()
           | `Not_found ->
             Eio.Path.mkdir ~perm:0o700 (path env native);
             Ok ()
           | _ -> corrupt "result intent parent is linked or invalid"
         in
         (* A prior mkdir may have succeeded before its parent sync failed.
            Re-establish that ancestor edge on every exact retry. *)
         Durable_file.sync_directory ~env ~path:(Filename.dirname native))
     | `Not_found -> corrupt "result intent owned root is missing"
     | _ -> corrupt "result intent owned root is linked or invalid")
;;

let read_file ~env ~native ~max_frame_bytes =
  let open Result.Let_syntax in
  let file = path env native in
  let%bind () =
    match Eio.Path.kind ~follow:false file with
    | `Regular_file -> Ok ()
    | `Not_found -> Error (Store_error.Missing native)
    | _ -> corrupt "result intent is linked or not a regular file"
  in
  let%bind () =
    if
      Optint.Int63.compare
        (Eio.Path.stat ~follow:false file).size
        (Optint.Int63.of_int max_frame_bytes)
      <= 0
    then Ok ()
    else corrupt "result intent file exceeds its size limit"
  in
  Eio.Path.with_open_in file (fun input ->
    let contents = Buffer.create (Int.min max_frame_bytes 4096) in
    let chunk = Cstruct.create 4096 in
    let rec loop () =
      match Eio.Flow.single_read input chunk with
      | 0 -> Ok (Buffer.contents contents)
      | count when count > max_frame_bytes - Buffer.length contents ->
        corrupt "result intent file exceeds its size limit"
      | count ->
        Buffer.add_string contents (Cstruct.to_string (Cstruct.sub chunk 0 count));
        loop ()
      | exception End_of_file -> Ok (Buffer.contents contents)
    in
    loop ())
;;

let read_at ~env ~parents ~directory ~filename ~max_frame_bytes =
  let open Result.Let_syntax in
  let%bind present = check_parents ~env parents in
  if not present
  then Ok None
  else (
    let native = Filename.concat directory filename in
    match read_file ~env ~native ~max_frame_bytes with
    | Error (Store_error.Missing _) -> Ok None
    | Error _ as error -> error
    | Ok contents ->
      let%map _ = validate_frame contents ~max_frame_bytes in
      Some contents)
;;

let protect_io ~operation ~native f =
  try f () with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn -> Error (Store_error.of_exn ~operation ~path:native exn)
;;

let save_exact t ~max_frame_bytes contents =
  let open Result.Let_syntax in
  let%bind _ = validate_frame contents ~max_frame_bytes in
  with_location t ~f:(fun ~env ~parents ~directory ~filename ->
    let native = Filename.concat directory filename in
    protect_io ~operation:"save result intent" ~native (fun () ->
      let%bind () = ensure_parents ~env parents in
      let%bind prior = read_at ~env ~parents ~directory ~filename ~max_frame_bytes in
      let%bind () =
        match prior with
        | None -> Ok ()
        | Some prior when String.equal prior contents -> Ok ()
        | Some _ -> corrupt "result intent already belongs to another selection"
      in
      Durable_file.replace ~env ~durability:Flush_file_and_directory ~path:native contents))
;;

let read_exact t ~max_frame_bytes =
  let open Result.Let_syntax in
  let%bind () = validate_max max_frame_bytes in
  with_location t ~f:(fun ~env ~parents ~directory ~filename ->
    let native = Filename.concat directory filename in
    protect_io ~operation:"read result intent" ~native (fun () ->
      read_at ~env ~parents ~directory ~filename ~max_frame_bytes))
;;

let remove_at ~env ~directory ~filename ~expected ~read_prior =
  let native = Filename.concat directory filename in
  let open Result.Let_syntax in
  let%bind prior = read_prior () in
  match prior with
  | None -> Ok ()
  | Some prior when String.equal prior expected ->
    protect_io ~operation:"remove result intent" ~native (fun () ->
      Eio.Path.unlink (path env native);
      Durable_file.sync_directory ~env ~path:directory)
  | Some _ -> corrupt "result intent changed before removal"
;;

let remove_exact t ~expected ~max_frame_bytes =
  let open Result.Let_syntax in
  let%bind _ = validate_frame expected ~max_frame_bytes in
  with_location t ~f:(fun ~env ~parents ~directory ~filename ->
    let native = Filename.concat directory filename in
    remove_at ~env ~directory ~filename ~expected ~read_prior:(fun () ->
      protect_io ~operation:"read result intent" ~native (fun () ->
        read_at ~env ~parents ~directory ~filename ~max_frame_bytes)))
;;

let remove_exact_with_retention
      t
      ~retention
      ~reader
      ~checkpoint
      ~expected
      ~max_frame_bytes
  =
  let open Result.Let_syntax in
  let%bind _ = validate_frame expected ~max_frame_bytes in
  match t with
  | Conversation _ -> corrupt "conversation intent has no developer retention borrow"
  | Developer { root; owner; job; attempt } ->
    Developer_run_store.Retention.with_root
      retention
      ~result_root:root
      ~f:(fun ~env ~root:data_root ->
        let base = Data_root.developer_result_intents_path data_root in
        let run =
          Filename.concat base ("run_" ^ P.Id.Developer_run.to_string owner.run_id)
        in
        let epoch = Filename.concat run ("epoch_" ^ Int.to_string owner.epoch) in
        let parents = [ Data_root.path data_root; base; run; epoch ] in
        let filename =
          "job_" ^ P.Id.Job.to_string job ^ "-" ^ Int.to_string attempt ^ ".frame"
        in
        let native = Filename.concat epoch filename in
        let read_prior () =
          checkpoint ();
          let%bind present =
            protect_io ~operation:"inspect result intent" ~native (fun () ->
              check_parents ~env parents)
          in
          if not present
          then Ok None
          else (
            let%bind local = Retention_reader.at_root reader ~root:epoch in
            let%bind found = Retention_reader.kind_or_missing local ~path:filename in
            match found with
            | None -> Ok None
            | Some `Directory -> corrupt "result intent is not a regular file"
            | Some `File ->
              let%bind bytes =
                Retention_reader.read_with_checkpoint
                  local
                  ~path:filename
                  ~max_bytes:max_frame_bytes
                  ~checkpoint
              in
              let%map _ = validate_frame bytes ~max_frame_bytes in
              Some bytes)
        in
        remove_at ~env ~directory:epoch ~filename ~expected ~read_prior)
;;
