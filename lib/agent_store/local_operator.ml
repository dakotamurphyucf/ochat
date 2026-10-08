open! Core

let maximum_bytes = 4096
let kind = "ochat.host.local_operator"

let validate_resource ~directory resource =
  match Eio_unix.Resource.fd_opt resource with
  | None ->
    Error (Store_error.Corrupt "local operator requires native ownership validation")
  | Some descriptor ->
    Eio_unix.Fd.use_exn "validate local operator ownership" descriptor (fun native ->
      Eio_unix.run_in_systhread (fun () ->
        let stat = Core_unix.fstat native in
        let kind_valid =
          match directory, stat.st_kind with
          | true, Core_unix.S_DIR | false, Core_unix.S_REG -> true
          | _ -> false
        in
        if
          kind_valid
          && Int.equal stat.st_uid (Core_unix.getuid ())
          && stat.st_perm land 0o077 = 0
        then Ok ()
        else
          Error
            (Store_error.Corrupt
               "local operator path is not private to the current OS operator")))
;;

let with_private_path ~path ~directory ~f =
  let valid_kind =
    match directory, Eio.Path.kind ~follow:false path with
    | true, `Directory | false, `Regular_file -> true
    | _ -> false
  in
  if not valid_kind
  then Error (Store_error.Corrupt "local operator path is missing or linked")
  else
    Eio.Path.with_open_in path (fun flow ->
      Result.bind (validate_resource ~directory flow) ~f:(fun () -> f flow))
;;

let private_path ~env ~path ~directory =
  with_private_path
    ~path:Eio.Path.(Eio.Stdenv.fs env / path)
    ~directory
    ~f:(fun _ -> Ok ())
;;

let validate_root ~env ~path =
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Not_found -> Ok ()
    | `Directory -> private_path ~env ~path ~directory:true
    | _ -> Error (Store_error.Corrupt "local host root is not an unlinked directory")
  with
  | Eio.Io _ as exn ->
    Error (Store_error.of_exn ~operation:"validate local root" ~path exn)
;;

let load_or_create ~env store =
  let open Result.Let_syntax in
  let root = Session_store.data_root store |> Data_root.path in
  let path = Filename.concat root "local-operator.json" in
  if Session_store.is_closed store
  then Error (Store_error.Corrupt "local operator requires current root ownership")
  else (
    try
      let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
      let%bind () =
        match Eio.Path.kind ~follow:false root_path with
        | `Directory -> Ok ()
        | _ -> Error (Store_error.Corrupt "local host root is linked or unavailable")
      in
      Eio.Path.with_open_dir root_path (fun directory ->
        let%bind () =
          with_private_path
            ~path:Eio.Path.(directory / ".")
            ~directory:true
            ~f:(fun _ -> Ok ())
        in
        let child = Eio.Path.(directory / "local-operator.json") in
        let%bind limits =
          Document_fields.limits ~max_bytes:maximum_bytes |> Document_fields.store
        in
        let decode contents =
          let%bind document =
            Document_schema.Document.decode ~limits contents |> Document_fields.store
          in
          let%bind () =
            Document_fields.expect document ~kind ~version:1 |> Document_fields.store
          in
          Document_fields.required
            (Document_schema.Document.payload document)
            "principal_id"
            (fun json ->
               Document_fields.protocol (Agent_protocol.Id.Principal.of_json json))
          |> Document_fields.store
        in
        match Eio.Path.kind ~follow:false child with
        | `Not_found ->
          let principal_id = Agent_protocol.Id.Principal.create () in
          let%bind document =
            Document_schema.Document.create
              ~limits
              ~kind
              ~version:1
              ~payload:
                (`Object
                    [ "principal_id", Agent_protocol.Id.Principal.to_json principal_id ])
            |> Document_fields.store
          in
          let%bind () =
            Durable_file.replace_in
              ~directory
              ~durability:Flush_file_and_directory
              ~basename:"local-operator.json"
              (Document_schema.Document.to_string document)
          in
          let%map () =
            with_private_path ~path:child ~directory:false ~f:(fun _ -> Ok ())
          in
          principal_id
        | `Regular_file ->
          with_private_path ~path:child ~directory:false ~f:(fun flow ->
            let buffer = Cstruct.create (maximum_bytes + 1) in
            let rec read offset =
              if offset > maximum_bytes
              then
                Error
                  (Store_error.Corrupt "local operator document exceeds its byte bound")
              else (
                match
                  Eio.Flow.single_read
                    flow
                    (Cstruct.sub buffer offset (Cstruct.length buffer - offset))
                with
                | count -> read (offset + count)
                | exception End_of_file ->
                  decode (Cstruct.to_string (Cstruct.sub buffer 0 offset)))
            in
            read 0)
        | _ ->
          Error
            (Store_error.Corrupt "local operator identity is not a regular private file"))
    with
    | Eio.Io _ as exn ->
      Error (Store_error.of_exn ~operation:"load local operator" ~path exn))
;;
