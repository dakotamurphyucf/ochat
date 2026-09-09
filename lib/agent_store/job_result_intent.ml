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

let validate session t =
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
    && P.Id.Session.equal t.reference.session_id (Session_store.Handle.session_id session)
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

let ensure_directory ~env session =
  let path = directory session in
  try
    let directory = Eio.Path.(Eio.Stdenv.fs env / path) in
    match Eio.Path.kind ~follow:false directory with
    | `Directory -> Ok ()
    | `Not_found ->
      Eio.Path.mkdir ~perm:0o700 directory;
      Durable_file.sync_directory ~env ~path:(Session_store.Handle.directory session)
    | _ -> corrupt "job result preparation directory is not a regular directory"
  with
  | exn ->
    Error (Store_error.of_exn ~operation:"create job result intent directory" ~path exn)
;;

let create ~env ~session ~reference ~metadata =
  let t = { version = 1; reference; metadata } in
  let path = path session t in
  try
    let open Result.Let_syntax in
    let%bind () = validate session t in
    let%bind () = ensure_directory ~env session in
    let%bind () =
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
      | `Not_found -> Ok ()
      | _ -> corrupt "job result preparation intent already exists"
    in
    let%bind contents =
      Frame.encode ~max_payload_length ~flags:0 (sexp_of_t t |> Sexp.to_string_mach)
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "job result intent exceeds its frame limit")
    in
    let%map () =
      Durable_file.replace ~env ~durability:Flush_file_and_directory ~path contents
    in
    t
  with
  | exn -> Error (Store_error.of_exn ~operation:"write job result intent" ~path exn)
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

let read ~env ~session ~filename =
  let open Result.Let_syntax in
  let%bind id =
    match String.chop_suffix filename ~suffix:".frame" with
    | None -> corrupt "invalid job result intent filename"
    | Some id ->
      P.Id.Blob.of_string id
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "invalid job result intent filename")
  in
  let path = Filename.concat (directory session) filename in
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
    match Frame.decode ~max_payload_length ~contents ~offset:0 with
    | Ok (Complete { frame; next_offset })
      when next_offset = String.length contents && Frame.flags frame = 0 ->
      let%bind intent =
        Result.try_with (fun () -> Frame.payload frame |> Sexp.of_string |> t_of_sexp)
        |> Result.map_error ~f:(fun _ ->
          Store_error.Corrupt "invalid job result intent payload")
      in
      let%bind () = validate session intent in
      (match P.Id.Blob.equal id intent.reference.blob.id with
       | true -> Ok intent
       | false -> corrupt "job result intent filename differs from its blob")
    | _ -> corrupt "job result intent is incomplete or corrupt"
  with
  | exn -> Error (Store_error.of_exn ~operation:"read job result intent" ~path exn)
;;

let list ~env ~session ~max_count =
  let path = directory session in
  try
    let directory = Eio.Path.(Eio.Stdenv.fs env / path) in
    match Eio.Path.kind ~follow:false directory with
    | `Not_found -> Ok []
    | `Directory ->
      let files =
        Eio.Path.read_dir directory |> List.filter ~f:(String.is_suffix ~suffix:".frame")
      in
      (match max_count >= 0 && List.length files <= max_count with
       | false -> corrupt "job result intent scan exceeds its count limit"
       | true ->
         files
         |> List.sort ~compare:String.compare
         |> List.map ~f:(fun filename -> read ~env ~session ~filename)
         |> Result.all)
    | _ -> corrupt "job result preparation directory is not a regular directory"
  with
  | exn -> Error (Store_error.of_exn ~operation:"list job result intents" ~path exn)
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
