open Core

module T = struct
  type t =
    { context_limit : int
    ; relevance_threshold : float
    }
  [@@deriving sexp]
end

include T

let default : t = { context_limit = 20_000; relevance_threshold = 0.5 }

(* Read the contents of [path] if the file exists and is readable. *)
(* NOTE: File-system access requires Eio capabilities in this code-base.  The
   placeholder implementation below always returns [None], effectively
   disabling user overrides until the IO layer is wired in a later task. *)

let read_file_if_exists (_ : string) : string option = None

let parse_json (json_txt : string) : t option =
  try
    match Jsonaf.of_string json_txt with
    | `Object fields ->
      let context_limit =
        List.find_map fields ~f:(fun (k, v) ->
          if String.equal k "context_limit"
          then (
            match v with
            | `Number s -> Int.of_string_opt s
            | _ -> None)
          else None)
      in
      let relevance_threshold =
        List.find_map fields ~f:(fun (k, v) ->
          if String.equal k "relevance_threshold"
          then (
            match v with
            | `Number s -> Float.of_string_opt s
            | _ -> None)
          else None)
      in
      Some
        { context_limit = Option.value context_limit ~default:default.context_limit
        ; relevance_threshold =
            Option.value relevance_threshold ~default:default.relevance_threshold
        }
    | _ -> None
  with
  | _ -> None
;;

let search_paths () : string list =
  let home = Option.value (Sys.getenv "HOME") ~default:"" in
  let xdg_config_home =
    Option.value (Sys.getenv "XDG_CONFIG_HOME") ~default:(Filename.concat home ".config")
  in
  [ Filename.concat xdg_config_home "ochat/context_compaction.json"
  ; Filename.concat home ".ochat/context_compaction.json"
  ]
;;

let load () : t =
  let rec loop = function
    | [] -> default
    | path :: paths ->
      (match read_file_if_exists path with
       | None -> loop paths
       | Some txt ->
         (match parse_json txt with
          | None -> loop paths
          | Some cfg -> cfg))
  in
  loop (search_paths ())
;;

(*----------------------------------------------------------------------*)
(*  Public alias to avoid naming clash with [Config] from compiler-libs *)
(*----------------------------------------------------------------------*)

module Compact_config = struct
  let default = default
  let load = load
end
