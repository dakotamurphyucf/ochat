(** Persistent credential cache for OAuth 2.0 Dynamic Client Registration.

    The implementation is intentionally minimalist:

    • The cache lives in a single JSON file under the user’s XDG config
      directory.  Each top-level key is an issuer URL and the value is a
      {!Credential.t} record.

    • Reads are best-effort — any IO or decoding error degrades to an
      empty map so that callers do not have to handle exceptions.

    • Writes are atomic — data is first written to [registered.json.tmp]
      and then renamed into place.

    The helpers never synchronise accesses from multiple processes at the
    OS level.  Concurrent writers may lose updates, although the atomic
    rename minimises the window for partial writes.  External locking is
    required in such scenarios. *)

open Core
module Jsonaf = Jsonaf_ext
open Jsonaf.Export

module Credential = struct
  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"] [@jsonaf.option]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

let config_dir () : string =
  match Sys.getenv "XDG_CONFIG_HOME" with
  | Some d -> Filename.concat d "ocamlochat"
  | None ->
    (match Sys.getenv "HOME" with
     | Some home -> Filename.concat home ".config/ocamlochat"
     | None -> Filename.concat "." ".config/ocamlochat")
;;

let file_path () = Filename.concat (config_dir ()) "registered.json"

let load_map env : (string, Credential.t, String.comparator_witness) Map.t =
  let fs = Eio.Stdenv.fs env in
  let path = Eio.Path.(fs / file_path ()) in
  (* if true then failwith @@ Eio.Path.native_exn path; *)
  (* If the file does not exist, return an empty map *)
  try
    let contents = Eio.Path.load path in
    let json = Jsonaf.of_string contents in
    match json with
    | `Object assoc ->
      assoc
      |> List.filter_map ~f:(fun (issuer, v) ->
        try Some (issuer, Credential.t_of_jsonaf v) with
        | _ -> None)
      |> Map.of_alist (module String)
      |> (function
       | `Ok m -> m
       | `Duplicate_key _ -> Map.empty (module String))
    | _ -> Map.empty (module String)
  with
  | _ -> Map.empty (module String)
;;

let save_map ~env map : unit =
  let fs = Eio.Stdenv.fs env in
  (* ensure directory exists *)
  Io.mkdir ~exists_ok:true ~dir:fs (config_dir ());
  let tmp = file_path () ^ ".tmp" in
  let path_tmp = Eio.Path.(fs / tmp) in
  let path_final = Eio.Path.(fs / file_path ()) in
  let json_obj =
    `Object
      (Map.to_alist map
       |> List.map ~f:(fun (issuer, cred) -> issuer, Credential.jsonaf_of_t cred))
    |> Jsonaf.to_string
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path_tmp json_obj;
  try Eio.Path.rename path_tmp path_final with
  | _ -> ()
;;

let lookup ~env ~issuer : Credential.t option =
  let map = load_map env in
  Map.find map issuer
;;

let store ~env ~issuer (cred : Credential.t) : unit =
  let map = load_map env in
  let map = Map.set map ~key:issuer ~data:cred in
  save_map ~env map
;;
