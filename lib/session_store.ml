open Core

type id = string
type path = Eio.Fs.dir_ty Eio.Path.t

(*--------------------------------------------------------------------------*)
(*  Path helpers                                                            *)
(*--------------------------------------------------------------------------*)

let base_dir () : string =
  match Sys.getenv "HOME" with
  | Some home -> Filename.concat home ".ochat/sessions"
  | None -> Filename.concat "." ".ochat/sessions"
;;

let rel_path (id : id) : string = Filename.concat (base_dir ()) id

(*--------------------------------------------------------------------------*)
(*  Public API                                                              *)
(*--------------------------------------------------------------------------*)

let ensure_dir ~env (id : id) : path =
  let fs = Eio.Stdenv.fs env in
  (* Guarantee the directory hierarchy exists.  [Io.mkdir] ensures
     [~perm:0o700] and creates intermediate segments. *)
  Io.mkdir ~exists_ok:true ~dir:fs (rel_path id);
  Eio.Path.(fs / rel_path id)
;;

let path ~env (id : id) : path =
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(fs / rel_path id)
;;

let fs env = Eio.Stdenv.fs env

(*--------------------------------------------------------------------------*)
(*  High-level helpers                                                      *)
(*--------------------------------------------------------------------------*)

(* Generate a pseudo-random UUID string using md5 of timestamp and random bits. *)
let uuid_v4 () : string =
  let data =
    let open Core in
    let time_ns = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string in
    time_ns ^ Int.to_string (Random.bits ())
  in
  Md5.digest_string data |> Md5.to_hex
;;

let default_id_of_prompt (prompt_file : string) : string =
  (* Use a deterministic hash of the [prompt_file] path so that consecutive
     runs that load the same prompt without specifying [--session] still
     resolve to the same on-disk directory.  This avoids the surprising
     behaviour where conversations appear “lost” because a fresh UUID was
     generated every time.  We prefer MD5 for its compact hexadecimal
     encoding and availability in Core. *)
  Core.Md5.(digest_string prompt_file |> to_hex)
;;

let load_or_create ~env ~prompt_file ?id ?(new_session = false) () : Session.t =
  (* Decide the session identifier to use.
     Priority order: explicit [?id] parameter → random UUID for
     [new_session] → deterministic hash of [prompt_file]. *)
  let id =
    match id with
    | Some id when not new_session -> id
    | _ when new_session -> uuid_v4 ()
    | _ -> default_id_of_prompt prompt_file
  in
  let dir = ensure_dir ~env id in
  let ( / ) = Eio.Path.( / ) in
  let snapshot = dir / "snapshot.bin" in
  (* Helper to attempt reading legacy snapshot and upgrading. *)
  let read_and_upgrade () : Session.t option =
    let try_read module_ upgrade_fn =
      Or_error.try_with (fun () ->
        let v = Bin_prot_utils_eio.read_bin_prot module_ snapshot in
        upgrade_fn v)
    in
    (* First attempt using the latest schema (this should succeed for fresh or up-to-date snapshots). *)
    match Or_error.try_with (fun () -> Session.Io.File.read snapshot) with
    | Ok s -> Some s
    | Error _ ->
      (* Try legacy V1 *)
      (match try_read (module Session.Legacy.V1) Session.Legacy.upgrade_v1 with
       | Ok s -> Some s
       | Error _ ->
         (* Try legacy V0 *)
         (match try_read (module Session.Legacy.V0) Session.Legacy.upgrade_v0 with
          | Ok s -> Some s
          | Error _ -> None))
  in
  let newly_created_session () =
    let prompt_copy_path = "prompt.chatmd" in
    (* Attempt to copy the prompt file into the session directory; ignore errors. *)
    (match
       Or_error.try_with (fun () ->
         (* When [prompt_file] is an absolute path, resolve it against the
            process-wide [fs] capability.  Otherwise treat it as relative to
            the current working directory so that launching [chat_tui] from a
            parent directory (a common workflow) succeeds. *)
         let dir_for_prompt =
           if Filename.is_absolute prompt_file then fs env else Eio.Stdenv.cwd env
         in
         let contents = Io.load_doc ~dir:dir_for_prompt prompt_file in
         let dst = dir / prompt_copy_path in
         Eio.Path.save ~create:(`Or_truncate 0o600) dst contents)
     with
     | _ -> ());
    Session.create ~id ~prompt_file ~local_prompt_copy:prompt_copy_path ()
  in
  (* Decide whether to load an existing snapshot or create a new one. *)
  if (not new_session) && Eio.Path.is_file snapshot
  then (
    match read_and_upgrade () with
    | Some s -> s
    | None -> newly_created_session ())
  else newly_created_session ()
;;

(*--------------------------------------------------------------------------*)
(*  Persistence – write snapshot                                            *)
(*--------------------------------------------------------------------------*)

let save ~env (session : Session.t) : unit =
  let dir = ensure_dir ~env session.id in
  let ( / ) = Eio.Path.( / ) in
  (*------------------------------------------------------------------*)
  (*  Advisory lock                                                   *)
  (*------------------------------------------------------------------*)
  let lock = dir / "snapshot.bin.lock" in
  let acquire_lock () =
    (* Try to create the lock file with exclusive semantics.  This will
       raise if the file already exists. *)
    Eio.Path.save ~create:(`Exclusive 0o600) lock ""
  in
  let release_lock () =
    try Eio.Path.unlink lock with
    | _ -> ()
  in
  (try acquire_lock () with
   | _ ->
     Core.eprintf
       "Error: session '%s' is currently locked by another process.\n"
       session.id;
     exit 1);
  protectx
    ()
    ~finally:(fun () -> release_lock ())
    ~f:(fun () ->
      let snapshot = dir / "snapshot.bin" in
      Session.Io.File.write snapshot session)
;;

(*--------------------------------------------------------------------------*)
(*  Reset / archive                                                          *)
(*--------------------------------------------------------------------------*)

let reset_session ~env ~(id : id) ?prompt_file ?(keep_history = false) () : unit =
  let dir = ensure_dir ~env id in
  let ( / ) = Eio.Path.( / ) in
  let snapshot = dir / "snapshot.bin" in
  (* Ensure the session exists. *)
  if not (Eio.Path.is_file snapshot)
  then Core.eprintf "Error: session '%s' not found.\n" id
  else (
    (* Read current snapshot. *)
    let session = Session.Io.File.read snapshot in
    (* Create archive directory. *)
    let archive_dir = dir / "archive" in
    (match Eio.Path.is_directory archive_dir with
     | true -> ()
     | false -> Eio.Path.mkdir ~perm:0o700 archive_dir);
    (* Generate timestamped file name. *)
    let timestamp () : string =
      let open Core in
      let tm = Core_unix.localtime (Core_unix.time ()) in
      Printf.sprintf
        "%04d%02d%02d-%02d%02d"
        (tm.tm_year + 1900)
        (tm.tm_mon + 1)
        tm.tm_mday
        tm.tm_hour
        tm.tm_min
    in
    let archived_snapshot =
      archive_dir / Printf.sprintf "%s.snapshot.bin" (timestamp ())
    in
    (* Move the existing snapshot to the archive path (overwrite if needed). *)
    (try Eio.Path.rename snapshot archived_snapshot with
     | _ -> ());
    (* Build a reset session value. *)
    let session_reset =
      if keep_history
      then Session.reset_keep_history ?prompt_file session
      else Session.reset ?prompt_file session
    in
    (* When [prompt_file] is provided, attempt to copy it into the session dir
       (prompt.chatmd) to keep the self-contained copy up-to-date. *)
    let session_reset =
      match prompt_file with
      | None -> session_reset
      | Some pf ->
        let copy_name = "prompt.chatmd" in
        (match
           Or_error.try_with (fun () ->
             let contents = Io.load_doc ~dir:(fs env) pf in
             let dst = dir / copy_name in
             Eio.Path.save ~create:(`Or_truncate 0o600) dst contents)
         with
         | _ -> ());
        { session_reset with local_prompt_copy = Some copy_name }
    in
    (* Save the new snapshot. *)
    save ~env session_reset;
    (* ------------------------------------------------------------------ *)
    (*  Cache handling: remove cache unless [keep_history] is true.          *)
    (* ------------------------------------------------------------------ *)
    (match keep_history with
     | true -> ()
     | false ->
       let chatmd_dir = dir / ".chatmd" in
       let cache_file = Eio.Path.(chatmd_dir / "cache.bin") in
       (try if Eio.Path.is_file cache_file then Eio.Path.unlink cache_file with
        | _ -> ()));
    (* Print confirmation summary. Uses [Core.printf] for simplicity. *)
    let history_len_before = List.length session.history in
    printf
      "Session '%s' reset%s. Archived snapshot: %s (history %d → %d)\n"
      id
      (if keep_history then " (history retained)" else "")
      (Eio.Path.native_exn archived_snapshot)
      history_len_before
      (List.length session_reset.history))
;;

(*--------------------------------------------------------------------------*)
(*  Rebuild session from prompt                                             *)
(*--------------------------------------------------------------------------*)

let rebuild_session ~env ~(id : id) () : unit =
  let dir = ensure_dir ~env id in
  let ( / ) = Eio.Path.( / ) in
  let snapshot = dir / "snapshot.bin" in
  if not (Eio.Path.is_file snapshot)
  then Core.eprintf "Error: session '%s' not found.\n" id
  else (
    let old_session = Session.Io.File.read snapshot in
    (* Archive old snapshot *)
    let archive_dir = dir / "archive" in
    (match Eio.Path.is_directory archive_dir with
     | true -> ()
     | false -> Eio.Path.mkdir ~perm:0o700 archive_dir);
    let timestamp () : string =
      let open Core in
      let tm = Core_unix.localtime (Core_unix.time ()) in
      Printf.sprintf
        "%04d%02d%02d-%02d%02d"
        (tm.tm_year + 1900)
        (tm.tm_mon + 1)
        tm.tm_mday
        tm.tm_hour
        tm.tm_min
    in
    let archived_snapshot =
      archive_dir / Printf.sprintf "%s.snapshot.bin" (timestamp ())
    in
    (try Eio.Path.rename snapshot archived_snapshot with
     | _ -> ());
    (* Remove cache file *)
    let chatmd_dir = dir / ".chatmd" in
    let cache_file = chatmd_dir / "cache.bin" in
    (try if Eio.Path.is_file cache_file then Eio.Path.unlink cache_file with
     | _ -> ());
    (* Create fresh session with same prompt info *)
    let new_session =
      Session.create
        ~id
        ~prompt_file:old_session.prompt_file
        ?local_prompt_copy:old_session.local_prompt_copy
        ()
    in
    save ~env new_session;
    Core.printf
      "Session '%s' rebuilt from prompt. Archived snapshot: %s\n"
      id
      (Eio.Path.native_exn archived_snapshot))
;;

(*--------------------------------------------------------------------------*)
(*  Enumerate stored sessions                                               *)
(*--------------------------------------------------------------------------*)

let list ~env : (id * string) list =
  let fs = Eio.Stdenv.fs env in
  let base_dir_path =
    (* Ensure [base_dir] is expressed relative to the FS capability.  We
       cannot assume the directory exists – return an empty list if it
       does not. *)
    Eio.Path.(fs / base_dir ())
  in
  if not (Eio.Path.is_directory base_dir_path)
  then []
  else (
    (* The special Unix entries "." and ".." are not included in the results. *)
    let entries = Eio.Path.read_dir base_dir_path in
    List.filter_map entries ~f:(fun entry ->
      let dir_path = Eio.Path.(base_dir_path / entry) in
      match Eio.Path.is_directory dir_path with
      | false -> None
      | true ->
        let snapshot = Eio.Path.(dir_path / "snapshot.bin") in
        (match Eio.Path.is_file snapshot with
         | false -> None
         | true ->
           (try
              let session = Session.Io.File.read snapshot in
              Some (entry, session.prompt_file)
            with
            | _ -> None))))
;;
