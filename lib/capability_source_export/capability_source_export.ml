module Native_unix = Unix
open! Core
module Low = Eio_posix.Low_level
module Fd = Eio_unix.Fd
module Sources = Capability_sources

module Error = struct
  type progress =
    | Not_created
    | Incomplete
  [@@deriving equal, sexp_of]

  type code =
    | Invalid_name
    | Exists
    | Denied
    | Unsupported_provider
    | Changed
    | Limit_exceeded
    | Io
  [@@deriving equal, sexp_of]

  type t =
    { progress : progress
    ; code : code
    }
  [@@deriving sexp_of]

  let progress t = t.progress
  let code t = t.code
end

module Directory_name = struct
  type t = string

  let create name =
    match Source.Portable_path.create name with
    | Ok _ when not (String.contains name '/') -> Ok name
    | Ok _ | Error _ -> Error Error.{ progress = Not_created; code = Invalid_name }
  ;;

  let to_string t = t
end

module Receipt = struct
  type t =
    { resource : Sources.Id.t
    ; digest : Chatml_source.Digest.t
    ; entry : Source.Portable_path.t
    ; members : int
    ; bytes : int
    }

  let resource t = t.resource
  let digest t = t.digest
  let entry t = t.entry
  let members t = t.members
  let bytes t = t.bytes
end

type identity =
  { device : int64
  ; inode : int64
  ; kind : Eio.File.Stat.kind
  }

type owned_entry =
  { parent : Fd.t
  ; name : string
  ; descriptor : Fd.t
  ; identity : identity
  }

exception Refused of Error.code

let native_code = function
  | Native_unix.EEXIST -> Error.Exists
  | EACCES | EPERM -> Denied
  | ELOOP | ENOTDIR | ENOENT -> Changed
  | _ -> Io
;;

let io f =
  try f () with
  | Native_unix.Unix_error (code, _, _) -> raise (Refused (native_code code))
  | Eio.Io (error, _) ->
    let code =
      match error with
      | Eio.Fs.E (Already_exists _) -> Error.Exists
      | Eio.Fs.E (Permission_denied _) -> Denied
      | Eio.Fs.E (Not_found _) -> Changed
      | Eio.Exn.X (Eio_unix.Unix_error (code, _, _)) -> native_code code
      | _ -> Io
    in
    raise (Refused code)
;;

let observed stat = { device = Low.dev stat; inode = Low.ino stat; kind = Low.kind stat }
let same a b = Int64.equal a.device b.device && Int64.equal a.inode b.inode

let is_directory identity =
  match identity.kind with
  | `Directory -> true
  | _ -> false
;;

let is_file identity =
  match identity.kind with
  | `Regular_file -> true
  | _ -> false
;;

let entry_identity parent name =
  let stat = Low.create_stat () in
  io (fun () -> Low.fstatat ~buf:stat ~follow:false (Fd parent) name);
  observed stat
;;

let descriptor_identity fd =
  let stat = Low.create_stat () in
  io (fun () -> Low.fstat ~buf:stat fd);
  observed stat
;;

let verify entry =
  let current = entry_identity entry.parent entry.name in
  let retained = descriptor_identity entry.descriptor in
  if not (same current entry.identity && same retained entry.identity)
  then raise (Refused Changed);
  match entry.identity.kind with
  | `Directory ->
    if not (is_directory current && is_directory retained) then raise (Refused Changed)
  | `Regular_file ->
    if not (is_file current && is_file retained) then raise (Refused Changed)
  | _ -> raise (Refused Changed)
;;

(* Public Eio's confined fallback may resolve a symlink even with nofollow flags.
   Admit the exact no-follow directory entry first and bind the opened FD to that
   identity before any writes. Never use the test-only Resolve API. *)
let open_directory ~sw ~checkpoint parent name =
  let before = entry_identity parent name in
  if not (is_directory before) then raise (Refused Changed);
  checkpoint ();
  let descriptor =
    io (fun () ->
      Low.openat
        ~sw
        ~mode:0
        (Fd parent)
        name
        Low.Open_flags.(rdonly + directory + nofollow))
  in
  let entry = { parent; name; descriptor; identity = before } in
  verify entry;
  entry
;;

let export resource ~parent ~directory ~checkpoint:caller_checkpoint =
  let created = ref false in
  let steps = ref 0 in
  let checkpoint () =
    Eio.Fiber.check ();
    caller_checkpoint ();
    if !steps >= 1_000_000 then raise (Refused Limit_exceeded);
    Int.incr steps
  in
  let error error_code =
    Error
      Error.
        { progress = (if !created then Incomplete else Not_created); code = error_code }
  in
  let run () =
    let _, path = parent in
    if not (String.is_empty path) then raise (Refused Unsupported_provider);
    Eio.Switch.run (fun sw ->
      checkpoint ();
      (* Directory resources do not expose Unix FDs on all Eio providers. An
         opened "." file lends one from this exact retained capability. *)
      let parent_flow = io (fun () -> Eio.Path.open_in ~sw Eio.Path.(parent / ".")) in
      let borrowed =
        match Eio_unix.Resource.fd_opt parent_flow with
        | None -> raise (Refused Unsupported_provider)
        | Some fd -> fd
      in
      let parent_fd =
        io (fun () ->
          Low.openat
            ~sw
            ~mode:0
            (Fd borrowed)
            "."
            Low.Open_flags.(rdonly + directory + nofollow))
      in
      let name = Directory_name.to_string directory in
      checkpoint ();
      io (fun () -> Low.mkdir ~mode:0o700 (Fd parent_fd) name);
      created := true;
      let root = open_directory ~sw ~checkpoint parent_fd name in
      let directories = ref (String.Map.singleton "" root.descriptor) in
      let entries = ref [ root ] in
      let rec directory_fd components =
        checkpoint ();
        let key = String.concat ~sep:"/" components in
        match Map.find !directories key with
        | Some fd -> fd
        | None ->
          let parent_components, leaf =
            match List.rev components with
            | [] -> assert false
            | leaf :: rest -> List.rev rest, leaf
          in
          let parent = directory_fd parent_components in
          checkpoint ();
          io (fun () -> Low.mkdir ~mode:0o700 (Fd parent) leaf);
          let opened = open_directory ~sw ~checkpoint parent leaf in
          entries := opened :: !entries;
          directories := Map.set !directories ~key ~data:opened.descriptor;
          opened.descriptor
      in
      let members = Sources.Resource.members resource in
      let priority member =
        let path = Source.Portable_path.to_string (Sources.Member.path member) in
        if
          Source.Portable_path.equal
            (Sources.Member.path member)
            (Sources.Resource.entry resource)
        then 2
        else if String.equal (Filename.basename path) "chatml.project.json"
        then 1
        else 0
      in
      let ordered =
        List.stable_sort members ~compare:(fun a b ->
          Int.compare (priority a) (priority b))
      in
      List.iter ordered ~f:(fun member ->
        checkpoint ();
        let components =
          String.split
            (Source.Portable_path.to_string (Sources.Member.path member))
            ~on:'/'
        in
        let parent_components, name =
          match List.rev components with
          | [] -> assert false
          | name :: rest -> List.rev rest, name
        in
        let parent = directory_fd parent_components in
        checkpoint ();
        (* POSIX O_EXCL rejects existing and dangling symlinks. No truncate flag;
           a failed post-open check is never used to undo an unsafe overwrite. *)
        let descriptor =
          io (fun () ->
            Low.openat
              ~sw
              ~mode:0o600
              (Fd parent)
              name
              Low.Open_flags.(wronly + creat + excl + nofollow))
        in
        let identity = descriptor_identity descriptor in
        if not (is_file identity) then raise (Refused Changed);
        let entry = { parent; name; descriptor; identity } in
        verify entry;
        entries := entry :: !entries;
        let contents = Sources.Member.contents member in
        let bytes = Bytes.of_string contents in
        let rec write offset =
          checkpoint ();
          if offset < Bytes.length bytes
          then (
            let count =
              io (fun () ->
                Low.write
                  descriptor
                  bytes
                  offset
                  (Int.min 4096 (Bytes.length bytes - offset)))
            in
            if count <= 0 then raise (Refused Io);
            write (offset + count))
        in
        write 0;
        io (fun () -> Low.fsync descriptor);
        verify entry);
      List.iter !entries ~f:(fun entry ->
        checkpoint ();
        verify entry);
      Map.iteri !directories ~f:(fun ~key:_ ~data:fd ->
        checkpoint ();
        io (fun () -> Low.fsync fd));
      io (fun () -> Low.fsync parent_fd);
      checkpoint ();
      List.iter !entries ~f:(fun entry ->
        checkpoint ();
        verify entry);
      let receipt : Receipt.t =
        { resource = Sources.Resource.id resource
        ; digest = Sources.Resource.digest resource
        ; entry = Sources.Resource.entry resource
        ; members = List.length members
        ; bytes = Sources.Resource.byte_length resource
        }
      in
      Ok receipt)
  in
  try run () with
  | Refused code -> error code
;;
