open! Core
module P = Chatmd_shell_spec.Path_expr
module Source_ref = Chatmd_shell_spec.Source_ref

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

let error code message = Error { code; message }

let source_dir t source =
  match Map.find t.source_dirs source.Source_ref.file with
  | Some path -> Ok path
  | None ->
    error
      "shell.source_dir_unavailable"
      (sprintf "source directory is unavailable for %s" source.file)
;;

let base_path t source = function
  | P.Workspace -> Ok t.workspace
  | Tool_dir -> Ok t.tool_dir
  | Prompt_dir -> Ok t.prompt_dir
  | Session_dir -> Ok t.session_dir
  | Cache_dir -> Ok t.cache_dir
  | Home -> Ok t.home
  | Source_dir -> source_dir t source
;;

let resolve_path t ~source = function
  | P.Absolute path -> Ok Eio.Path.(Eio.Stdenv.fs t.env / path)
  | Relative { base; path } ->
    Result.map (base_path t source base) ~f:(fun directory -> Eio.Path.(directory / path))
;;

let resolve_existing_directory t ~source expression =
  Result.bind (resolve_path t ~source expression) ~f:(fun path ->
    if Eio.Path.is_directory path
    then Ok path
    else
      error
        "shell.directory_unavailable"
        (sprintf "required directory is unavailable: %s" (P.to_string expression)))
;;
