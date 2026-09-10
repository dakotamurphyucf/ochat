open Core
module P = Agent_protocol

type t =
  { version : int
  ; reference : P.Job_artifact.t
  ; metadata : Blob_store.Metadata.t
  }
[@@deriving sexp]

let reference t = t.reference
let metadata t = t.metadata
let max_payload_length = 32768

let directory session =
  Filename.concat (Session_store.Handle.directory session) "result-preparations"
;;

let filename t = P.Id.Blob.to_string t.reference.blob.id ^ ".frame"
let path session t = Filename.concat (directory session) (filename t)
let corrupt message = Error (Store_error.Corrupt message)

let validate_session session_id t =
  let open Result.Let_syntax in
  let%bind _ =
    P.Job_artifact.of_json (P.Job_artifact.to_json t.reference)
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
  in
  let%bind _ =
    P.Id.Principal.of_json (P.Id.Principal.to_json t.metadata.creating_principal)
    |> Result.map_error ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
  in
  match
    t.version = 1
    && P.Id.Session.equal t.reference.session_id session_id
    && Option.exists
         t.metadata.target_session
         ~f:(P.Id.Session.equal t.reference.session_id)
    && (not t.metadata.durable)
    && String.equal t.metadata.allowed_use (P.Job_artifact.allowed_use t.reference)
    && Jsonaf.exactly_equal
         (P.Blob.Metadata.to_json t.metadata.blob)
         (P.Blob.Metadata.to_json t.reference.blob)
    && Option.for_all t.metadata.expires_at ~f:(fun at ->
      P.Timestamp.compare at t.metadata.created_at >= 0)
  with
  | true -> Ok ()
  | false -> corrupt "invalid job result preparation intent"
;;

let validate session t = validate_session (Session_store.Handle.session_id session) t

let ensure_directory ~env session =
  let path = directory session in
  try
    let directory = Eio.Path.(Eio.Stdenv.fs env / path) in
    match Eio.Path.kind ~follow:false directory with
    | `Directory ->
      Durable_file.sync_directory ~env ~path:(Session_store.Handle.directory session)
    | `Not_found ->
      Eio.Path.mkdir ~perm:0o700 directory;
      Durable_file.sync_directory ~env ~path:(Session_store.Handle.directory session)
    | _ -> corrupt "job result preparation directory is not a regular directory"
  with
  | exn ->
    Error (Store_error.of_exn ~operation:"create job result intent directory" ~path exn)
;;

let make ~session ~reference ~metadata =
  let t = { version = 1; reference; metadata } in
  Result.map (validate session t) ~f:(fun () -> t)
;;

let read_contents file =
  Eio.Path.with_open_in file (fun input ->
    let limit = max_payload_length + 4096 in
    let contents = Buffer.create 4096 in
    let chunk = Cstruct.create 4096 in
    let rec loop () =
      match Eio.Flow.single_read input chunk with
      | 0 -> Ok (Buffer.contents contents)
      | count when count > limit - Buffer.length contents ->
        corrupt "job result intent grew beyond its size limit"
      | count ->
        Buffer.add_string contents (Cstruct.to_string (Cstruct.sub chunk 0 count));
        loop ()
      | exception End_of_file -> Ok (Buffer.contents contents)
    in
    loop ())
;;

let decode ~session_id ~filename contents =
  let open Result.Let_syntax in
  let%bind id =
    match String.chop_suffix filename ~suffix:".frame" with
    | None -> corrupt "invalid job result intent filename"
    | Some id ->
      P.Id.Blob.of_string id
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "invalid job result intent filename")
  in
  match Frame.decode ~max_payload_length ~contents ~offset:0 with
  | Ok (Complete { frame; next_offset })
    when next_offset = String.length contents && Frame.flags frame = 0 ->
    let%bind intent =
      Result.try_with (fun () -> Frame.payload frame |> Sexp.of_string |> t_of_sexp)
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "invalid job result intent payload")
    in
    let%bind () = validate_session session_id intent in
    (match P.Id.Blob.equal id intent.reference.blob.id with
     | true -> Ok intent
     | false -> corrupt "job result intent filename differs from its blob")
  | _ -> corrupt "job result intent is incomplete or corrupt"
;;

let read_owned ~env ~session_id ~directory ~filename =
  let open Result.Let_syntax in
  let path = Filename.concat directory filename in
  try
    let file = Eio.Path.(Eio.Stdenv.fs env / path) in
    let%bind () =
      match Eio.Path.kind ~follow:false file with
      | `Regular_file -> Ok ()
      | _ -> corrupt "job result intent is not a regular file"
    in
    let%bind () =
      match (Eio.Path.stat ~follow:false file).size with
      | size
        when Optint.Int63.compare size (Optint.Int63.of_int (max_payload_length + 4096))
             <= 0 -> Ok ()
      | _ -> corrupt "job result intent file exceeds its size limit"
    in
    let%bind contents = read_contents file in
    decode ~session_id ~filename contents
  with
  | exn -> Error (Store_error.of_exn ~operation:"read job result intent" ~path exn)
;;

let read ~env ~session ~filename =
  read_owned
    ~env
    ~session_id:(Session_store.Handle.session_id session)
    ~directory:(directory session)
    ~filename
;;

let protects_temporary ~env ~data_root (metadata : Blob_store.Metadata.t) =
  let open Result.Let_syntax in
  let%bind _ =
    P.Blob.Metadata.of_json (P.Blob.Metadata.to_json metadata.blob)
    |> Result.map_error ~f:(fun failure -> Store_error.Corrupt failure.P.Error.message)
  in
  match metadata.target_session with
  | None -> Ok false
  | Some session_id ->
    let%bind session_id =
      P.Id.Session.of_json (P.Id.Session.to_json session_id)
      |> Result.map_error ~f:(fun failure -> Store_error.Corrupt failure.P.Error.message)
    in
    let session_directory = Data_root.session_path data_root session_id in
    let directory = Filename.concat session_directory "result-preparations" in
    let filename = P.Id.Blob.to_string metadata.blob.id ^ ".frame" in
    let regular_directory path =
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
      | `Not_found -> Ok false
      | `Directory -> Ok true
      | _ -> corrupt "job result preparation directory is not a regular directory"
    in
    (try
       let%bind session_exists = regular_directory session_directory in
       match session_exists with
       | false -> Ok false
       | true ->
         let%bind exists = regular_directory directory in
         (match exists with
          | false -> Ok false
          | true ->
            (match
               Eio.Path.kind
                 ~follow:false
                 Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory filename)
             with
             | `Not_found -> Ok false
             | _ ->
               let%bind intent = read_owned ~env ~session_id ~directory ~filename in
               (match
                  Sexp.equal
                    (Blob_store.Metadata.sexp_of_t metadata)
                    (Blob_store.Metadata.sexp_of_t intent.metadata)
                with
                | true -> Ok true
                | false ->
                  corrupt "temporary blob differs from its private result preparation")))
     with
     | exn ->
       Error
         (Store_error.of_exn
            ~operation:"protect temporary job result"
            ~path:directory
            exn))
;;

let save ~env ~session t =
  let path = path session t in
  try
    let open Result.Let_syntax in
    let%bind () = validate session t in
    let%bind () = ensure_directory ~env session in
    let%bind () =
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
      | `Not_found -> Ok ()
      | `Regular_file ->
        let%bind actual = read ~env ~session ~filename:(filename t) in
        (match Sexp.equal (sexp_of_t actual) (sexp_of_t t) with
         | true -> Ok ()
         | false ->
           corrupt "job result preparation intent already belongs to another value")
      | _ -> corrupt "job result preparation intent is not a regular file"
    in
    let%bind contents =
      Frame.encode ~max_payload_length ~flags:0 (sexp_of_t t |> Sexp.to_string_mach)
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "job result intent exceeds its frame limit")
    in
    Durable_file.replace ~env ~durability:Flush_file_and_directory ~path contents
  with
  | exn -> Error (Store_error.of_exn ~operation:"save job result intent" ~path exn)
;;

let create ~env ~session ~reference ~metadata =
  let open Result.Let_syntax in
  let%bind t = make ~session ~reference ~metadata in
  let%map () = save ~env ~session t in
  t
;;

let list_with_reader ~reader ~session ~max_count =
  let open Result.Let_syntax in
  let%bind () =
    match
      max_count >= 0
      && String.equal
           (Retention_reader.root reader)
           (Session_store.Handle.directory session)
    with
    | true -> Ok ()
    | false -> corrupt "intent retention reader does not match its session or limits"
  in
  let%bind root_names = Retention_reader.list reader ~directory:"." in
  match List.mem root_names "result-preparations" ~equal:String.equal with
  | false -> Ok []
  | true ->
    let%bind names = Retention_reader.list reader ~directory:"result-preparations" in
    let valid_name name =
      match String.chop_suffix name ~suffix:".frame" with
      | None -> false
      | Some id -> Result.is_ok (P.Id.Blob.of_string id)
    in
    let%bind files =
      List.fold_result names ~init:[] ~f:(fun files name ->
        match valid_name name, Durable_file.temporary_target name with
        | true, _ -> Ok (name :: files)
        | false, Some target when valid_name target -> Ok files
        | _ -> corrupt "unknown file in job result preparation directory")
    in
    let%bind () =
      match List.length files <= max_count with
      | true -> Ok ()
      | false -> corrupt "job result intent scan exceeds its count limit"
    in
    List.fold_result (List.rev files) ~init:[] ~f:(fun intents filename ->
      let%bind contents =
        Retention_reader.read
          reader
          ~path:(Filename.concat "result-preparations" filename)
          ~max_bytes:(max_payload_length + 4096)
      in
      let%map intent =
        decode ~session_id:(Session_store.Handle.session_id session) ~filename contents
      in
      intent :: intents)
    |> Result.map ~f:List.rev
;;

let list ~env ~session ~max_count =
  let open Result.Let_syntax in
  let%bind () =
    match max_count >= 0 with
    | true -> Ok ()
    | false -> corrupt "job result intent scan exceeds its count limit"
  in
  let allowance ~scale ~overhead =
    match max_count <= (Int.max_value - overhead) / scale with
    | true -> (max_count * scale) + overhead
    | false -> Int.max_value
  in
  let%bind reader =
    Retention_reader.create
      ~env
      ~root:(Session_store.Handle.directory session)
      ~max_entries:(allowance ~scale:4 ~overhead:64)
      ~max_bytes:(allowance ~scale:(max_payload_length + 4096) ~overhead:0)
  in
  list_with_reader ~reader ~session ~max_count
;;

let remove ~env ~session t =
  let open Result.Let_syntax in
  let%bind () = validate session t in
  let path = path session t in
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Not_found -> Ok ()
    | `Regular_file ->
      let%bind actual = read ~env ~session ~filename:(filename t) in
      (match Sexp.equal (sexp_of_t actual) (sexp_of_t t) with
       | false -> corrupt "job result intent changed before removal"
       | true ->
         Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / path);
         Durable_file.sync_directory ~env ~path:(directory session))
    | _ -> corrupt "job result intent is not a regular file"
  with
  | exn -> Error (Store_error.of_exn ~operation:"remove job result intent" ~path exn)
;;

let discard_unreferenced ~env ~scope ~reader ~session t =
  let open Result.Let_syntax in
  let%bind () = validate session t in
  let%bind () =
    match
      String.equal (Retention_reader.root reader) (Session_store.Handle.directory session)
    with
    | true -> Ok ()
    | false -> corrupt "intent discard reader does not match its session"
  in
  let relative = Filename.concat "result-preparations" (filename t) in
  let read_current () =
    let%bind bytes =
      Retention_reader.read reader ~path:relative ~max_bytes:(max_payload_length + 4096)
    in
    let%bind actual =
      decode
        ~session_id:(Session_store.Handle.session_id session)
        ~filename:(filename t)
        bytes
    in
    match Sexp.equal (sexp_of_t actual) (sexp_of_t t) with
    | true -> Ok bytes
    | false -> corrupt "private result intent changed before staged discard"
  in
  let%bind expected = read_current () in
  let%bind names = Retention_reader.list reader ~directory:"result-preparations" in
  let%bind temporary_paths =
    List.fold_result names ~init:[] ~f:(fun paths name ->
      match
        Option.exists (Durable_file.temporary_target name) ~f:(String.equal (filename t))
      with
      | false -> Ok paths
      | true ->
        let%bind bytes =
          Retention_reader.read
            reader
            ~path:(Filename.concat "result-preparations" name)
            ~max_bytes:(String.length expected)
        in
        (match String.is_prefix expected ~prefix:bytes with
         | true -> Ok (Filename.concat (directory session) name :: paths)
         | false -> corrupt "temporary private intent differs from its preparation"))
  in
  let%bind () =
    Blob_store.discard_staged_unreferenced scope ~reader session ~metadata:t.metadata
  in
  let%bind _ = read_current () in
  let path = path session t in
  try
    let%bind () =
      List.fold_result (temporary_paths @ [ path ]) ~init:() ~f:(fun () native_path ->
        let file = Eio.Path.(Eio.Stdenv.fs env / native_path) in
        match Eio.Path.kind ~follow:false file with
        | `Not_found -> Ok ()
        | `Regular_file ->
          Eio.Path.unlink file;
          Ok ()
        | _ -> corrupt "private intent path changed before removal")
    in
    Durable_file.sync_directory ~env ~path:(directory session)
  with
  | exn -> Error (Store_error.of_exn ~operation:"finish staged result discard" ~path exn)
;;
