open! Core
open Jsonaf.Export

type base =
  | Workspace
  | Source_dir
  | Prompt_dir
  | Tool_dir
  | Session_dir
  | Cache_dir
  | Home
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  | Absolute of string
  | Relative of
      { base : base
      ; path : string
      }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let variables =
  [ "workspace", Workspace
  ; "source_dir", Source_dir
  ; "prompt_dir", Prompt_dir
  ; "tool_dir", Tool_dir
  ; "session_dir", Session_dir
  ; "cache_dir", Cache_dir
  ; "home", Home
  ]
;;

let variable_name base =
  List.find_map_exn variables ~f:(fun (name, candidate) ->
    Option.some_if (equal_base base candidate) name)
;;

let base_to_string = variable_name

let split_variable value =
  match String.chop_prefix value ~prefix:"${" with
  | None -> None
  | Some rest ->
    Option.map (String.lsplit2 rest ~on:'}') ~f:(fun (name, suffix) -> name, suffix)
;;

let path_after_variable suffix =
  match String.chop_prefix suffix ~prefix:"/" with
  | Some path -> Ok path
  | None when String.is_empty suffix -> Ok ""
  | None -> Error "path variable must be followed by '/' or the end of the value"
;;

let parse_variable value =
  match split_variable value with
  | None -> Ok None
  | Some (name, suffix) ->
    (match List.Assoc.find variables name ~equal:String.equal with
     | None -> Error (sprintf "unknown path variable ${%s}" name)
     | Some base ->
       Result.map (path_after_variable suffix) ~f:(fun path -> Some (base, path)))
;;

let parse ?(default_base = Source_dir) value =
  if String.is_empty value
  then Error "path must not be empty"
  else if String.mem value '\000'
  then Error "path must not contain NUL"
  else if Filename.is_absolute value
  then Ok (Absolute value)
  else
    Result.map (parse_variable value) ~f:(function
      | Some (base, path) -> Relative { base; path }
      | None -> Relative { base = default_base; path = value })
;;

let to_string = function
  | Absolute path -> path
  | Relative { base; path = "" } -> sprintf "${%s}" (variable_name base)
  | Relative { base; path } -> sprintf "${%s}/%s" (variable_name base) path
;;
