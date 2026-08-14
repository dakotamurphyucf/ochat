open! Core

(** Explicit live host capabilities used to instantiate a shell manifest. *)

type t =
  { env : Eio_unix.Stdenv.base
  ; workspace : Eio.Fs.dir_ty Eio.Path.t
  ; tool_dir : Eio.Fs.dir_ty Eio.Path.t
  ; prompt_dir : Eio.Fs.dir_ty Eio.Path.t
  ; session_dir : Eio.Fs.dir_ty Eio.Path.t
  ; cache_dir : Eio.Fs.dir_ty Eio.Path.t
  ; home : Eio.Fs.dir_ty Eio.Path.t
  ; source_dirs : Eio.Fs.dir_ty Eio.Path.t String.Map.t
  ; process_environment : string array
  ; session_id : string
  ; resource_runner : string option
  }

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

(** [resolve_path t ~source expression] resolves a path expression through
    capabilities supplied by [t]. Imported source-relative paths require an
    entry keyed by [source.file] in [t.source_dirs]. *)
val resolve_path
  :  t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Path_expr.t
  -> (Eio.Fs.dir_ty Eio.Path.t, error) result

(** [resolve_existing_directory t ~source expression] resolves [expression]
    and requires it to identify an existing directory. *)
val resolve_existing_directory
  :  t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Path_expr.t
  -> (Eio.Fs.dir_ty Eio.Path.t, error) result
