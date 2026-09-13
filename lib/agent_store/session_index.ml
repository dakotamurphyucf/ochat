open Core

module Entry = struct
  type t =
    { session : Agent_protocol.Session.t
    ; runnable_job_count : int
    ; deliverable_job_count : int
    ; earliest_schedule_due : Agent_protocol.Timestamp.t option
    ; owner_grace_deadline : Agent_protocol.Timestamp.t option
    ; pending_initial_start : bool [@sexp.default false]
    ; archived : bool
    }
  [@@deriving sexp]
end

module Persisted = struct
  type t =
    { version : int
    ; entries : Entry.t list
    }
  [@@deriving sexp]
end

type t =
  { env : Eio_unix.Stdenv.base
  ; path : string
  ; mutex : Eio.Mutex.t
  ; mutable entries : (Agent_protocol.Id.Session.t, Entry.t) Map.Poly.t
  }

let version = 1

let map_of_entries entries =
  List.fold entries ~init:Map.Poly.empty ~f:(fun map entry ->
    Map.set map ~key:entry.Entry.session.id ~data:entry)
;;

let entries_of_map map =
  Map.data map
  |> List.sort ~compare:(fun left right ->
    Agent_protocol.Timestamp.compare
      left.Entry.session.created_at
      right.Entry.session.created_at)
;;

let save t entries =
  let persisted = Persisted.{ version; entries = entries_of_map entries } in
  Durable_file.replace
    ~env:t.env
    ~durability:Flush_file_and_directory
    ~path:t.path
    (Sexp.to_string_mach ([%sexp_of: Persisted.t] persisted))
;;

let load ~env ~path =
  let open Result.Let_syntax in
  let%bind contents = Durable_file.load ~env ~path in
  try
    let persisted = [%of_sexp: Persisted.t] (Sexp.of_string contents) in
    if persisted.version = version
    then Ok (map_of_entries persisted.entries)
    else if persisted.version > version
    then Error (Store_error.Schema_too_new persisted.version)
    else Error (Store_error.Migration_required persisted.version)
  with
  | exn ->
    Error (Store_error.Corrupt ("session index decode failed: " ^ Exn.to_string exn))
;;

let make ~env ~path entries = { env; path; mutex = Eio.Mutex.create (); entries }

let rebuild_missing ~env ~path ~rebuild =
  let open Result.Let_syntax in
  let%bind entries = rebuild () |> Result.map ~f:map_of_entries in
  let t = make ~env ~path entries in
  let%map () = save t entries in
  t
;;

let open_or_rebuild ~env ~path ~rebuild =
  if not (Filename.is_absolute path)
  then
    Error
      (Store_error.Io
         { operation = "open session index"; path; message = "path must be absolute" })
  else (
    try
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
      | `Not_found -> rebuild_missing ~env ~path ~rebuild
      | `Regular_file -> load ~env ~path |> Result.map ~f:(make ~env ~path)
      | _ -> Error (Store_error.Corrupt "session index is not a regular file")
    with
    | exn -> Error (Store_error.of_exn ~operation:"open session index" ~path exn))
;;

let open_or_create ~env ~path = open_or_rebuild ~env ~path ~rebuild:(fun () -> Ok [])
let list t = Eio.Mutex.use_ro t.mutex (fun () -> entries_of_map t.entries)
let find t id = Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries id)

let update t f =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let entries = f t.entries in
    Result.map (save t entries) ~f:(fun () -> t.entries <- entries))
;;

let upsert t entry =
  update t (fun entries -> Map.set entries ~key:entry.Entry.session.id ~data:entry)
;;

let remove t id = update t (fun entries -> Map.remove entries id)
let replace_all t entries = update t (fun _ -> map_of_entries entries)
