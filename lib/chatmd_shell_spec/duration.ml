open! Core
open Jsonaf.Export

type t = float [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]
type bytes = int64 [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let duration_units = [ "ms", 0.001; "s", 1.; "m", 60.; "h", 3600. ]
let byte_units = [ "GiB", 1_073_741_824L; "MiB", 1_048_576L; "KiB", 1024L; "B", 1L ]

let split_suffix value units =
  List.find_map units ~f:(fun (suffix, multiplier) ->
    Option.map (String.chop_suffix value ~suffix) ~f:(fun number -> number, multiplier))
;;

let parse_number value =
  match Float.of_string_opt value with
  | Some number when Float.is_finite number && Float.(number > 0.) -> Ok number
  | _ -> Error "duration must contain a positive finite decimal value"
;;

let parse value =
  match split_suffix value duration_units with
  | None -> Error "duration must end in ms, s, m, or h"
  | Some (number, multiplier) -> Result.map (parse_number number) ~f:(( *. ) multiplier)
;;

let to_seconds t = t
let to_string t = sprintf "%.17gs" t

let parse_nonnegative_int64 value =
  match Int64.of_string_opt value with
  | Some number when Int64.(number >= 0L) -> Ok number
  | _ -> Error "byte size must contain a non-negative integer"
;;

let multiply_bytes value multiplier =
  if Int64.(value > max_value / multiplier)
  then Error "byte size exceeds the supported range"
  else Ok Int64.(value * multiplier)
;;

let parse_bytes value =
  let number, multiplier =
    Option.value (split_suffix value byte_units) ~default:(value, 1L)
  in
  Result.bind (parse_nonnegative_int64 number) ~f:(fun value ->
    multiply_bytes value multiplier)
;;

let bytes_to_int64 bytes = bytes
let bytes_to_string bytes = Int64.to_string bytes ^ "B"
