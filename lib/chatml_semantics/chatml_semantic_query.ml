open Core

module Limits = struct
  type t =
    { max_results : int
    ; max_work_steps : int
    }

  let create ~max_results ~max_work_steps =
    if max_results <= 0 || max_work_steps <= 0
    then Or_error.error_string "Semantic query limits must be positive"
    else Ok { max_results; max_work_steps }
  ;;

  let max_results t = t.max_results
end

module Error = struct
  type t =
    | Foreign_point
    | Foreign_symbol
    | Foreign_context
    | Unknown_source
    | Unknown_module
    | Invalid_location
    | Nonzero_point_range
    | Query_work_limit
  [@@deriving sexp_of]
end

exception Rejected of Error.t

let reject error = raise (Rejected error)

let run limits ~checkpoint ~f =
  Chatml.Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    let remaining = ref limits.Limits.max_work_steps in
    let checkpoint () =
      if !remaining <= 0 then reject Query_work_limit;
      Int.decr remaining;
      checkpoint ()
    in
    try Ok (f ~checkpoint) with
    | Rejected error -> Error error)
;;

let compare_string left right ~checkpoint =
  let length = Int.min (String.length left) (String.length right) in
  let rec chunk start =
    checkpoint ();
    let stop = Int.min length (start + Int.min 256 (length - start)) in
    let rec compare index =
      if index >= stop
      then
        if stop >= length
        then Int.compare (String.length left) (String.length right)
        else chunk stop
      else (
        let order = Char.compare (String.get left index) (String.get right index) in
        if order = 0 then compare (index + 1) else order)
    in
    compare start
  in
  chunk 0
;;

let equal_source left right ~checkpoint =
  compare_string
    (Chatml_source.Id.package_namespace left)
    (Chatml_source.Id.package_namespace right)
    ~checkpoint
  = 0
  && compare_string
       (Chatml_source.Id.relative_path left)
       (Chatml_source.Id.relative_path right)
       ~checkpoint
     = 0
;;
