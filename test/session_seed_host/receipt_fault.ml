open! Core

(** Test-owned native adapter: retain all provider bindings and delegate every
    operation. Before the first terminal-outcome rename, create a real directory
    at its native destination. This obstructs the actual durable writer. *)
let rec wrap_directory
  :  'tag.
     ([> `Dir ] as 'tag) Eio.Resource.t
  -> obstruct:(string -> unit)
  -> 'tag Eio.Resource.t
  =
  fun (Eio.Resource.T (directory, handler)) ~obstruct ->
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let open_dir directory ~sw path =
      wrap_directory (Original.open_dir directory ~sw path) ~obstruct
    ;;

    let rename directory source destination target =
      if
        String.is_prefix (Filename.basename target) ~prefix:"idempotency-outcome-"
        && String.is_suffix target ~suffix:".json"
      then (
        match Original.native directory target with
        | Some path -> obstruct path
        | None -> failwith "receipt fault requires actual native destination");
      Original.rename directory source destination target
    ;;
  end
  in
  let bindings =
    Eio.Resource.bindings handler
    |> List.filter ~f:(function
      | H (Eio.Fs.Pi.Dir, _) -> false
      | _ -> true)
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler (H (Eio.Fs.Pi.Dir, (module Directory)) :: bindings))
;;

let wrap env ~obstruct =
  let directory, path = Eio.Stdenv.fs env in
  object
    method fs = wrap_directory directory ~obstruct, path
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;
