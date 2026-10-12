open Core

module Entry = struct
  type t =
    | Directory of string
    | File of string
end

let is_staging_name name =
  match String.chop_prefix name ~prefix:".install-" with
  | None -> false
  | Some transaction -> Result.is_ok (Agent_protocol.Id.Transaction.of_string transaction)
;;

let plan reader roots ~checkpoint =
  let open Result.Let_syntax in
  let rec visit pending entries =
    checkpoint ();
    match pending with
    | [] -> Ok entries
    | (path, staging_root) :: rest ->
      let%bind kind = Retention_reader.kind reader ~path in
      (match kind with
       | `File ->
         if staging_root
         then
           Error (Store_error.Corrupt "artifact installation staging is not a directory")
         else visit rest (Entry.File path :: entries)
       | `Directory ->
         let%bind children =
           Retention_reader.list_with_checkpoint reader ~directory:path ~checkpoint
         in
         let pending =
           List.fold children ~init:rest ~f:(fun pending name ->
             (Filename.concat path name, false) :: pending)
         in
         (* Prepending each visited entry yields child-before-parent deletion
            order without recursive deletion or an unbounded second scan. *)
         visit pending (Entry.Directory path :: entries))
  in
  visit (List.map roots ~f:(fun path -> path, true)) []
;;

let remove ~env ~root entry =
  let relative =
    match entry with
    | Entry.Directory path | File path -> path
  in
  let path = Filename.concat root relative in
  try
    let target = Eio.Path.(Eio.Stdenv.fs env / path) in
    (match entry with
     | Entry.Directory _ -> Eio.Path.rmdir target
     | File _ -> Eio.Path.unlink target);
    Ok ()
  with
  | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
    Error (Store_error.of_exn ~operation:"discard interrupted artifact install" ~path exn)
;;

let discard ~env ~root ~max_entries ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind reader = Retention_reader.create ~env ~root ~max_entries ~max_bytes:0 in
  let%bind names =
    Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint
  in
  let roots = List.filter names ~f:is_staging_name in
  let%bind entries = plan reader roots ~checkpoint in
  let%bind () =
    List.fold_result entries ~init:() ~f:(fun () entry ->
      checkpoint ();
      remove ~env ~root entry)
  in
  let%map () =
    if List.is_empty roots then Ok () else Durable_file.sync_directory ~env ~path:root
  in
  List.length roots
;;
