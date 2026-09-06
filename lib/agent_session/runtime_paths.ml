open Core

type t =
  { tool_dir : Eio.Fs.dir_ty Eio.Path.t
  ; workspace : Eio.Fs.dir_ty Eio.Path.t
  ; prompt_dir : Eio.Fs.dir_ty Eio.Path.t
  ; session_dir : Eio.Fs.dir_ty Eio.Path.t
  ; cache_dir : Eio.Fs.dir_ty Eio.Path.t
  ; home : Eio.Fs.dir_ty Eio.Path.t
  }

let create ~env ~tool_dir ~workspace ~prompt_dir ~session_dir ~cache_dir ~home =
  let paths = [ tool_dir; workspace; prompt_dir; session_dir; cache_dir; home ] in
  if List.exists paths ~f:(fun path -> not (Filename.is_absolute path))
  then Error (Agent_store.Store_error.Corrupt "runtime paths must be absolute")
  else (
    let fs = Eio.Stdenv.fs env in
    Ok
      { tool_dir = Eio.Path.(fs / tool_dir)
      ; workspace = Eio.Path.(fs / workspace)
      ; prompt_dir = Eio.Path.(fs / prompt_dir)
      ; session_dir = Eio.Path.(fs / session_dir)
      ; cache_dir = Eio.Path.(fs / cache_dir)
      ; home = Eio.Path.(fs / home)
      })
;;
