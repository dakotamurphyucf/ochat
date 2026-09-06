(** Runtime path coordinates supplied to ChatMD/ChatML/tool construction. *)

type t =
  { tool_dir : Eio.Fs.dir_ty Eio.Path.t
  ; workspace : Eio.Fs.dir_ty Eio.Path.t
  ; prompt_dir : Eio.Fs.dir_ty Eio.Path.t
  ; session_dir : Eio.Fs.dir_ty Eio.Path.t
  ; cache_dir : Eio.Fs.dir_ty Eio.Path.t
  ; home : Eio.Fs.dir_ty Eio.Path.t
  }

val create
  :  env:Eio_unix.Stdenv.base
  -> tool_dir:string
  -> workspace:string
  -> prompt_dir:string
  -> session_dir:string
  -> cache_dir:string
  -> home:string
  -> (t, Agent_store.Store_error.t) result
