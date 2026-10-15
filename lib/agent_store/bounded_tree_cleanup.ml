open! Core

module Limits = struct
  type t =
    { max_entries : int
    ; max_bytes : int
    ; max_work_steps : int
    }

  let create ~max_entries ~max_bytes ~max_work_steps =
    if max_entries <= 0 || max_bytes <= 0 || max_work_steps <= 0
    then Or_error.error_string "bounded cleanup limits must be positive"
    else Ok { max_entries; max_bytes; max_work_steps }
  ;;
end

let limit_error description =
  Store_error.Corrupt ("bounded cleanup limit exceeded: " ^ description)
;;

let remove_children
      ~env
      ~root
      ~expected_root
      ~marker_name
      ~(limits : Limits.t)
      ~checkpoint
  =
  let open Result.Let_syntax in
  let work = ref 0 in
  let bytes = ref 0 in
  let entries = ref 0 in
  let charge () =
    checkpoint ();
    if !work >= limits.max_work_steps
    then Error (limit_error "work")
    else (
      Int.incr work;
      Ok ())
  in
  let admit_name name =
    if !work >= limits.max_work_steps
    then Error (limit_error "work")
    else if !entries >= limits.max_entries
    then Error (limit_error "entries")
    else if String.length name > limits.max_bytes - !bytes
    then Error (limit_error "names")
    else (
      Int.incr work;
      Int.incr entries;
      bytes := !bytes + String.length name;
      Ok ())
  in
  let rec remove_directory ~root_entry path =
    let%bind () = charge () in
    try
      let before = Eio.Path.stat ~follow:false path in
      match before.kind with
      | `Directory ->
        if
          root_entry
          && not
               (let device, inode = expected_root in
                Int64.equal before.dev device && Int64.equal before.ino inode)
        then Error (Store_error.Corrupt "cleanup root identity changed")
        else
          Eio.Switch.run (fun sw ->
            let directory = Eio.Path.open_dir ~sw path in
            let after = Eio.Path.stat ~follow:true directory in
            if not (Int64.equal before.dev after.dev && Int64.equal before.ino after.ino)
            then Error (Store_error.Corrupt "cleanup directory identity changed")
            else (
              let%bind reader =
                Retention_reader.create_at
                  ~root:directory
                  ~max_entries:(limits.max_entries - !entries)
                  ~max_bytes:limits.max_bytes
              in
              let%bind names =
                Retention_reader.list_with_checkpoint_and_names
                  reader
                  ~directory:"."
                  ~on_name:admit_name
                  ~checkpoint
              in
              let%bind () =
                List.fold_result names ~init:() ~f:(fun () name ->
                  let%bind () = charge () in
                  if root_entry && String.equal name marker_name
                  then Ok ()
                  else (
                    let child = Eio.Path.(directory / name) in
                    let%bind () = charge () in
                    match Eio.Path.kind ~follow:false child with
                    | `Directory -> remove_directory ~root_entry:false child
                    | `Regular_file | `Symbolic_link ->
                      Eio.Path.unlink child;
                      Ok ()
                    | `Not_found -> Error (Store_error.Corrupt "cleanup child vanished")
                    | `Unknown | `Socket | `Fifo | `Character_special | `Block_device ->
                      Error (Store_error.Corrupt "cleanup child has unsupported kind")))
              in
              if root_entry
              then Ok ()
              else (
                let%bind () = charge () in
                let after = Eio.Path.stat ~follow:false path in
                if
                  not
                    (Int64.equal before.dev after.dev && Int64.equal before.ino after.ino)
                then
                  Error (Store_error.Corrupt "cleanup directory changed before removal")
                else (
                  Eio.Path.rmdir path;
                  Ok ()))))
      | `Symbolic_link -> Error (Store_error.Corrupt "cleanup root is linked")
      | `Regular_file | `Unknown | `Socket | `Fifo | `Character_special | `Block_device ->
        Error (Store_error.Corrupt "cleanup root is not a directory")
    with
    | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
      Error (Store_error.of_exn ~operation:"bounded cleanup" ~path:root exn)
  in
  remove_directory ~root_entry:true Eio.Path.(Eio.Stdenv.fs env / root)
;;
