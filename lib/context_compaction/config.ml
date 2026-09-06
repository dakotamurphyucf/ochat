open Core

module T = struct
  type t =
    { context_limit : int
    ; relevance_threshold : float
    ; relevance_filtering : bool
    }
  [@@deriving sexp]
end

include T

let default : t =
  { context_limit = 20_000; relevance_threshold = 0.5; relevance_filtering = false }
;;

let read_file_if_exists ~env path =
  try Some (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)) with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | _ -> None
;;

let is_valid t =
  t.context_limit > 0
  && Float.is_finite t.relevance_threshold
  && Float.(t.relevance_threshold >= 0. && t.relevance_threshold <= 1.)
;;

let field fields name fallback decode =
  match List.Assoc.find fields ~equal:String.equal name with
  | None -> Some fallback
  | Some value -> decode value
;;

let parse_fields fields =
  let open Option.Let_syntax in
  let%bind () =
    if List.contains_dup (List.map fields ~f:fst) ~compare:String.compare
    then None
    else Some ()
  in
  let%bind context_limit =
    field fields "context_limit" default.context_limit (function
      | `Number text -> Int.of_string_opt text
      | _ -> None)
  in
  let%bind relevance_threshold =
    field fields "relevance_threshold" default.relevance_threshold (function
      | `Number text -> Float.of_string_opt text
      | _ -> None)
  in
  let%bind relevance_filtering =
    field fields "relevance_filtering" false (function
      | `True -> Some true
      | `False -> Some false
      | _ -> None)
  in
  let config = { context_limit; relevance_threshold; relevance_filtering } in
  if is_valid config then Some config else None
;;

let parse_json text =
  try
    match Jsonaf.of_string text with
    | `Object fields -> parse_fields fields
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

let load_paths ~env paths : t =
  let rec loop = function
    | [] -> default
    | path :: paths ->
      (match read_file_if_exists ~env path with
       | None -> loop paths
       | Some txt ->
         (match parse_json txt with
          | None -> loop paths
          | Some cfg -> cfg))
  in
  loop paths
;;

let load ?env () =
  Option.value_map env ~default ~f:(fun env -> load_paths ~env (search_paths ()))
;;

(*----------------------------------------------------------------------*)
(*  Public alias to avoid naming clash with [Config] from compiler-libs *)
(*----------------------------------------------------------------------*)

module Compact_config = struct
  let default = default
  let load = load
end
