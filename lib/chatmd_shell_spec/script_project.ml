open Core
open Jsonaf.Export

type t =
  { project : string
  ; environment : string option [@jsonaf.option]
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let create ~project ~environment =
  let valid value =
    (not (String.is_empty (String.strip value))) && not (String.contains value '\000')
  in
  if not (valid project)
  then Or_error.error_string "script project must be nonblank and contain no NUL"
  else if Option.exists environment ~f:(fun value -> not (valid value))
  then Or_error.error_string "script environment must be nonblank and contain no NUL"
  else Ok { project; environment }
;;
