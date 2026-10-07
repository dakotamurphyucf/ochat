open! Core

type id = string
type path = Eio.Fs.dir_ty Eio.Path.t

let base_dir () =
  Filename.concat (Option.value (Sys.getenv "HOME") ~default:".") ".ochat/sessions"
;;

let rel_path id = Filename.concat (base_dir ()) id
let path ~env id = Eio.Path.(Eio.Stdenv.fs env / rel_path id)

let ensure_dir ~env id =
  Io.mkdir ~exists_ok:true ~dir:(Eio.Stdenv.fs env) (rel_path id);
  path ~env id
;;

let io_result f =
  try Ok (f ()) with
  | Eio.Io _ as exn -> Error (Error.of_exn exn)
;;

let document_error error = Error.create_s [%sexp (error : Document_schema.Error.t)]

let read_document_file snapshot =
  let open Result.Let_syntax in
  let%bind bytes =
    try
      io_result (fun () ->
        Eio.Path.with_open_in snapshot (fun flow ->
          let max_size =
            Document_schema.Limits.max_bytes Document_schema.Limits.default
          in
          Eio.Buf_read.of_flow flow ~max_size |> Eio.Buf_read.take_all))
    with
    | Eio.Buf_read.Buffer_limit_exceeded ->
      Or_error.error_string "session snapshot exceeds document byte limit"
  in
  Document_schema.Document.decode ~limits:Document_schema.Limits.default bytes
  |> Result.map_error ~f:document_error
;;

let restore document =
  Session.Document.decode document |> Result.map_error ~f:document_error
;;

let read_current_file snapshot = Result.bind (read_document_file snapshot) ~f:restore

let read_owned_snapshot snapshot ~id =
  let open Result.Let_syntax in
  let%bind document = read_document_file snapshot in
  let%bind () =
    match
      Document_schema.Json.field (Document_schema.Document.payload document) ~name:"id"
    with
    | Value (`String stored_id) when String.equal stored_id id -> Ok ()
    | Absent | Null | Value _ ->
      Or_error.error_string "snapshot session ID does not match its directory"
  in
  let%bind session = restore document in
  if String.equal session.Session.id id
  then Ok session
  else Or_error.error_string "converted session ID does not match its directory"
;;

let read_existing ~env ~id =
  let snapshot = Eio.Path.(path ~env id / "snapshot.bin") in
  if Eio.Path.is_file snapshot
  then Some (read_owned_snapshot snapshot ~id |> Or_error.ok_exn)
  else None
;;

let load_prompt ~env ~prompt_file =
  let source =
    if Filename.is_absolute prompt_file then Eio.Stdenv.fs env else Eio.Stdenv.cwd env
  in
  Io.load_doc ~dir:source prompt_file
;;

let copy_prompt ~env ~directory ~prompt_file =
  let contents = load_prompt ~env ~prompt_file in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(directory / "prompt.chatmd")
    contents
;;

let load_or_create ~env ~prompt_file ?id ?(new_session = false) () =
  let id =
    if new_session
    then (Session.create ~prompt_file ()).id
    else Option.value id ~default:(Md5.digest_string prompt_file |> Md5.to_hex)
  in
  let directory = path ~env id in
  let snapshot = Eio.Path.(directory / "snapshot.bin") in
  if Eio.Path.is_file snapshot
  then read_owned_snapshot snapshot ~id |> Or_error.ok_exn
  else (
    let directory = ensure_dir ~env id in
    let local_prompt_copy =
      match io_result (fun () -> copy_prompt ~env ~directory ~prompt_file) with
      | Ok () -> Some "prompt.chatmd"
      | Error _ -> None
    in
    Session.create ~id ~prompt_file ?local_prompt_copy ())
;;

let snapshot_sequence = Atomic.make 0

let fresh_name prefix =
  sprintf "%s.%08x.%d" prefix (Random.bits ()) (Atomic.fetch_and_add snapshot_sequence 1)
;;

let unlink_if_present path =
  try Eio.Path.unlink path with
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> ()
;;

let write_snapshot_atomic directory bytes =
  let temporary = Eio.Path.(directory / (fresh_name "snapshot" ^ ".tmp")) in
  let owned = ref false in
  Fun.protect
    (fun () ->
       Eio.Path.with_open_out ~create:(`Exclusive 0o600) temporary (fun flow ->
         owned := true;
         Eio.Flow.copy_string bytes flow);
       Eio.Path.rename temporary Eio.Path.(directory / "snapshot.bin");
       owned := false)
    ~finally:(fun () ->
      if !owned then Eio.Cancel.protect (fun () -> unlink_if_present temporary))
;;

let with_lock directory ~f =
  let lock = Eio.Path.(directory / "snapshot.bin.lock") in
  match io_result (fun () -> Eio.Path.save ~create:(`Exclusive 0o600) lock "") with
  | Error error -> Error (Error.tag error ~tag:"unable to acquire session snapshot lock")
  | Ok () ->
    Fun.protect f ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> unlink_if_present lock))
;;

let save ~env session =
  let open Result.Let_syntax in
  (* No directory, lock or existing file changes until full document preflight. *)
  let%bind bytes =
    Session.Document.to_string session |> Result.map_error ~f:document_error
  in
  let%bind directory = io_result (fun () -> ensure_dir ~env session.Session.id) in
  with_lock directory ~f:(fun () ->
    io_result (fun () -> write_snapshot_atomic directory bytes))
;;

let save_exn ~env session = save ~env session |> Or_error.ok_exn

let archive_snapshot directory =
  let archive = Eio.Path.(directory / "archive") in
  if not (Eio.Path.is_directory archive) then Eio.Path.mkdir ~perm:0o700 archive;
  let destination = Eio.Path.(archive / (fresh_name "snapshot" ^ ".bin")) in
  (* Copy, never move, the old snapshot: a subsequent failed replacement leaves
     the authoritative snapshot available at its original path. *)
  Eio.Path.with_open_in
    Eio.Path.(directory / "snapshot.bin")
    (fun source ->
       Eio.Path.with_open_out ~create:(`Exclusive 0o600) destination (fun sink ->
         Eio.Flow.copy source sink));
  destination
;;

let clear_cache directory =
  let cache = Eio.Path.(directory / ".chatmd" / "cache.bin") in
  if Eio.Path.is_file cache then Eio.Path.unlink cache
;;

let replace_session ~env ~id ~transform ~prompt_file ~clear_history_cache ~verb =
  let directory = path ~env id in
  let snapshot = Eio.Path.(directory / "snapshot.bin") in
  if not (Eio.Path.is_file snapshot)
  then
    Eio.Flow.copy_string
      (sprintf "Error: session '%s' not found.\n" id)
      (Eio.Stdenv.stderr env)
  else (
    let result =
      with_lock directory ~f:(fun () ->
        let open Result.Let_syntax in
        let%bind previous = read_owned_snapshot snapshot ~id in
        let%bind prompt =
          match prompt_file with
          | None -> Ok None
          | Some prompt_file ->
            io_result (fun () ->
              let contents = load_prompt ~env ~prompt_file in
              Some (fresh_name "prompt" ^ ".chatmd", contents))
        in
        let next = transform previous in
        let next =
          match prompt with
          | None -> next
          | Some (filename, _) -> { next with Session.local_prompt_copy = Some filename }
        in
        let%bind bytes =
          Session.Document.to_string next |> Result.map_error ~f:document_error
        in
        io_result (fun () ->
          let archived = archive_snapshot directory in
          (* Use an immutable new prompt path. Updating prompt.chatmd first would
           change what the old snapshot references if its replacement failed.
           An unacknowledged commit may leave an unreferenced prompt copy; it
           must never remove a copy the committed snapshot could reference. *)
          Option.iter prompt ~f:(fun (filename, contents) ->
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(directory / filename)
              contents);
          write_snapshot_atomic directory bytes;
          if clear_history_cache then clear_cache directory;
          archived))
    in
    match result with
    | Error error ->
      Eio.Flow.copy_string
        (sprintf
           "Error: session '%s' could not be %s: %s\n"
           id
           verb
           (Error.to_string_hum error))
        (Eio.Stdenv.stderr env)
    | Ok archived ->
      Eio.Flow.copy_string
        (sprintf
           "Session '%s' %s. Archived snapshot: %s\n"
           id
           verb
           (Eio.Path.native_exn archived))
        (Eio.Stdenv.stdout env))
;;

let reset_session ~env ~id ?prompt_file ?(keep_history = false) () =
  replace_session
    ~env
    ~id
    ~prompt_file
    ~clear_history_cache:(not keep_history)
    ~verb:"reset"
    ~transform:(fun previous ->
      if keep_history
      then Session.reset_keep_history ?prompt_file previous
      else Session.reset ?prompt_file previous)
;;

let rebuild_session ~env ~id () =
  (* Rebuild preserves the allocator high-water mark and preservation context.
     Starting from a fresh value would recycle IDs and silently discard fields. *)
  replace_session
    ~env
    ~id
    ~prompt_file:None
    ~clear_history_cache:true
    ~verb:"rebuilt"
    ~transform:(fun previous ->
      { (Session.reset previous) with tasks = []; kv_store = [] })
;;

let list ~env =
  let base = Eio.Path.(Eio.Stdenv.fs env / base_dir ()) in
  if not (Eio.Path.is_directory base)
  then []
  else
    Eio.Path.read_dir base
    |> List.filter_map ~f:(fun id ->
      let snapshot = Eio.Path.(base / id / "snapshot.bin") in
      if not (Eio.Path.is_file snapshot)
      then None
      else (
        match read_owned_snapshot snapshot ~id with
        | Ok session -> Some (id, session.Session.prompt_file)
        | Error _ -> None))
;;
