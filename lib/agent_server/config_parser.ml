open Core

module Raw_config = struct
  type t =
    { source_file : string
    ; forms : Sexp.t list
    }
end

let diagnostic ~source_file message =
  Config.Diagnostic.
    { code = "config.syntax"
    ; config_path = "$"
    ; source_file
    ; message
    ; remediation = "Fix the S-expression syntax and validate the file again."
    }
;;

let parse_string ~source_file contents =
  try
    match Sexp.of_string ("(" ^ contents ^ ")") with
    | Sexp.List forms -> Ok Raw_config.{ source_file; forms }
    | _ -> Error [ diagnostic ~source_file "configuration must contain top-level forms" ]
  with
  | exn -> Error [ diagnostic ~source_file (Exn.to_string exn) ]
;;

let load ~env ~path =
  if not (Filename.is_absolute path)
  then Error [ diagnostic ~source_file:path "configuration path must be absolute" ]
  else (
    try
      Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) |> parse_string ~source_file:path
    with
    | exn -> Error [ diagnostic ~source_file:path (Exn.to_string exn) ])
;;
