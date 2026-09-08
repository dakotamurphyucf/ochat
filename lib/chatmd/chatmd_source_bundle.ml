open Core

type limits =
  { max_source_bytes : int
  ; max_bundle_bytes : int
  ; max_files : int
  }

let default_limits =
  { max_source_bytes = 256 * 1024; max_bundle_bytes = 2 * 1024 * 1024; max_files = 128 }
;;

type t =
  { root_file : string
  ; sources : (string * string) list
  ; limits : limits
  ; fingerprint : string
  }

let root_file t = t.root_file
let sources t = t.sources
let limits t = t.limits
let fingerprint t = t.fingerprint
let loader t ~root = Source_loader.generated_filesystem ~root ~sources:t.sources

let valid_path path =
  (not (String.is_empty path))
  && String.length path <= 1024
  && Stdlib.String.is_valid_utf_8 path
  && (not
        (String.exists path ~f:(function
           | '\\' | ':' | '\000' | '\r' | '\n' -> true
           | _ -> false)))
  && List.for_all (String.split path ~on:'/') ~f:(fun part ->
    not (String.is_empty part || String.equal part "." || String.equal part ".."))
;;

let create ?(limits = default_limits) ~root_file ~sources () =
  let open Result.Let_syntax in
  let%bind () =
    if
      limits.max_source_bytes <= 0
      || limits.max_source_bytes > 1024 * 1024
      || limits.max_bundle_bytes <= 0
      || limits.max_bundle_bytes > 8 * 1024 * 1024
      || limits.max_files <= 0
      || limits.max_files > 256
    then Error "generated source limits exceed supported bounds"
    else if List.length sources > limits.max_files
    then Error "generated bundle file limit exceeded"
    else if not (valid_path root_file)
    then Error "invalid generated root path"
    else Ok ()
  in
  let%bind _, seen =
    List.fold
      sources
      ~init:(Ok (0, String.Set.empty))
      ~f:(fun acc (path, text) ->
        let%bind bytes, seen = acc in
        let key = String.lowercase path in
        if not (valid_path path)
        then Error "invalid generated source path"
        else if Set.mem seen key
        then Error "duplicate or case-colliding generated source paths"
        else if String.length text > limits.max_source_bytes
        then Error "generated source byte limit exceeded"
        else if String.length text > limits.max_bundle_bytes - bytes
        then Error "generated bundle byte limit exceeded"
        else Ok (bytes + String.length text, Set.add seen key))
  in
  let exact_paths = String.Set.of_list (List.map sources ~f:fst) in
  let%bind () =
    if not (Set.mem exact_paths root_file)
    then Error "generated bundle is missing its root source"
    else if
      List.exists sources ~f:(fun (path, _) ->
        let parts = String.split path ~on:'/' in
        let rec check prefix = function
          | [] | [ _ ] -> false
          | part :: rest ->
            let prefix = if String.is_empty prefix then part else prefix ^ "/" ^ part in
            Set.mem seen (String.lowercase prefix) || check prefix rest
        in
        check "" parts)
    then Error "generated source paths conflict with directories"
    else Ok ()
  in
  let sources = List.sort sources ~compare:(fun (a, _) (b, _) -> String.compare a b) in
  let fingerprint =
    [%sexp
      ("ochat.generated-sources.v1" : string)
    , (root_file : string)
    , (List.map sources ~f:(fun (path, text) ->
         path, Chatmd_shell_spec.Source_ref.digest text)
       : (string * string) list)
    , (limits.max_source_bytes : int)
    , (limits.max_bundle_bytes : int)
    , (limits.max_files : int)]
    |> Sexp.to_string
    |> Chatmd_shell_spec.Source_ref.digest
  in
  Ok { root_file; sources; limits; fingerprint }
;;
