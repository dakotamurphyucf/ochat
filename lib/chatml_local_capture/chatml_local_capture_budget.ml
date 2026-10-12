open Core

module Limits = struct
  type t =
    { directory_entries : int
    ; sources : int
    ; source_bytes : int
    ; total_source_bytes : int
    ; path_components : int
    ; work_steps : int
    }

  let create
        ~max_directory_entries
        ~max_sources
        ~max_source_bytes
        ~max_total_source_bytes
        ~max_path_components
        ~max_work_steps
    =
    if
      max_directory_entries < 0
      || max_sources < 0
      || max_source_bytes <= 0
      || max_total_source_bytes < 0
      || max_path_components <= 0
      || max_work_steps < 0
    then Or_error.error_string "invalid local capture limits"
    else
      Ok
        { directory_entries = max_directory_entries
        ; sources = max_sources
        ; source_bytes = max_source_bytes
        ; total_source_bytes = max_total_source_bytes
        ; path_components = max_path_components
        ; work_steps = max_work_steps
        }
  ;;

  let directory_entries t = t.directory_entries
  let sources t = t.sources
  let source_bytes t = t.source_bytes
  let total_source_bytes t = t.total_source_bytes
  let path_components t = t.path_components
  let work_steps t = t.work_steps
end

module Accounting = struct
  type t =
    { directory_entries : int
    ; source_count : int
    ; source_bytes : int
    ; work_steps : int
    }

  let directory_entries t = t.directory_entries
  let source_count t = t.source_count
  let source_bytes t = t.source_bytes
  let work_steps t = t.work_steps
end

module Limit = struct
  type t =
    | Directory_entries
    | Source_count
    | Source_bytes
    | Total_source_bytes
    | Path_components
    | Work_steps
  [@@deriving equal, sexp_of]
end

type problem =
  | Exceeded of
      { limit : Limit.t
      ; maximum : int
      }
  | Overflow

type t =
  { limits : Limits.t
  ; mutable entries : int
  ; mutable sources : int
  ; mutable bytes : int
  ; mutable work : int
  }

let create limits = { limits; entries = 0; sources = 0; bytes = 0; work = 0 }
let remaining_entries t = Limits.directory_entries t.limits - t.entries
let remaining_bytes t = Limits.total_source_bytes t.limits - t.bytes
let source_available t = t.sources < Limits.sources t.limits
let read_allowance t = Int.min (Limits.source_bytes t.limits) (remaining_bytes t)
let exceeded limit maximum = Error (Exceeded { limit; maximum })

let check_source_slot t =
  if source_available t then Ok () else exceeded Source_count (Limits.sources t.limits)
;;

let sum left right =
  if left < 0 || right < 0 || left > Int.max_value - right
  then Error Overflow
  else Ok (left + right)
;;

let product left right =
  if left < 0 || right < 0 || (right > 0 && left > Int.max_value / right)
  then Error Overflow
  else Ok (left * right)
;;

let bounded_add current increment limit maximum =
  let open Result.Let_syntax in
  let%bind next = sum current increment in
  if next > maximum then exceeded limit maximum else Ok next
;;

let add_entries t count =
  Result.map
    (bounded_add t.entries count Directory_entries (Limits.directory_entries t.limits))
    ~f:(fun next -> t.entries <- next)
;;

let add_source t ~bytes =
  let open Result.Let_syntax in
  let%bind () = check_source_slot t in
  if bytes > Limits.source_bytes t.limits
  then exceeded Source_bytes (Limits.source_bytes t.limits)
  else (
    let%map next =
      bounded_add t.bytes bytes Total_source_bytes (Limits.total_source_bytes t.limits)
    in
    t.sources <- t.sources + 1;
    t.bytes <- next)
;;

let reserve t units =
  Result.map
    (bounded_add t.work units Work_steps (Limits.work_steps t.limits))
    ~f:(fun next -> t.work <- next)
;;

let chunks bytes =
  if bytes < 0
  then Error Overflow
  else Ok ((bytes / 4096) + if bytes mod 4096 = 0 then 0 else 1)
;;

let reserve_scan t ~bytes =
  let open Result.Let_syntax in
  let%bind chunks = chunks bytes in
  let%bind units = sum chunks 1 in
  reserve t units
;;

let reserve_read t ~byte_allowance ~components =
  let open Result.Let_syntax in
  let%bind traversals = product components 2 in
  let%bind units = sum byte_allowance traversals in
  let%bind units = sum units 6 in
  reserve t units
;;

let rec height n levels =
  if n <= 1 then levels else height ((n / 2) + (n mod 2)) (levels + 1)
;;

let reserve_observation t ~lookup_count =
  let open Result.Let_syntax in
  let%bind lookup_units = product 4 (height lookup_count 0) in
  let%bind units = sum lookup_units 8 in
  reserve t units
;;

let reserve_ordering t ~count ~key_bytes ~passes =
  let open Result.Let_syntax in
  let%bind chunks = chunks key_bytes in
  let%bind per_compare = sum chunks 1 in
  let%bind visits = product count (height count 0 + 1) in
  let%bind units = product visits per_compare in
  let%bind units = product units passes in
  reserve t units
;;

let snapshot t =
  { Accounting.directory_entries = t.entries
  ; source_count = t.sources
  ; source_bytes = t.bytes
  ; work_steps = t.work
  }
;;
