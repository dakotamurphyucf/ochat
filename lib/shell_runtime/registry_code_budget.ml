open Core

module Error = struct
  type t =
    | Invalid_limit
    | Work_limit
  [@@deriving sexp, equal]
end

type t =
  { maximum : int
  ; checkpoint : unit -> unit
  ; mutable used : int
  ; owner : unit ref
  ; mutable rejected : bool
  ; mutable checkpoint_failure : (exn * Stdlib.Printexc.raw_backtrace) option
  ; mutable active : bool
  }

exception Rejected of unit ref

let work t =
  if not t.active then invalid_arg "registry code budget escaped its invocation";
  Option.iter t.checkpoint_failure ~f:(fun (exn, trace) ->
    Stdlib.Printexc.raise_with_backtrace exn trace);
  if t.rejected || t.used >= t.maximum
  then (
    t.rejected <- true;
    raise_notrace (Rejected t.owner));
  t.used <- t.used + 1;
  try
    Eio.Fiber.check ();
    t.checkpoint ();
    Eio.Fiber.check ()
  with
  | exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    t.checkpoint_failure <- Some (exn, trace);
    Stdlib.Printexc.raise_with_backtrace exn trace
;;

let equal_string_with_checkpoint ~checkpoint left right =
  checkpoint ();
  let length = String.length left in
  if not (Int.equal length (String.length right))
  then false
  else (
    let rec chunks offset =
      if offset >= length
      then true
      else (
        checkpoint ();
        let stop = offset + Int.min 1024 (length - offset) in
        let rec equal_bytes offset =
          if offset >= stop
          then true
          else if Char.equal (String.get left offset) (String.get right offset)
          then equal_bytes (offset + 1)
          else false
        in
        equal_bytes offset && chunks stop)
    in
    chunks 0)
;;

let equal_string t = equal_string_with_checkpoint ~checkpoint:(fun () -> work t)

let run ~max_work_steps ~checkpoint f =
  if max_work_steps < 0
  then Error Error.Invalid_limit
  else (
    let t =
      { maximum = max_work_steps
      ; checkpoint
      ; used = 0
      ; owner = ref ()
      ; rejected = false
      ; checkpoint_failure = None
      ; active = true
      }
    in
    Exn.protect
      ~finally:(fun () -> t.active <- false)
      ~f:(fun () ->
        try
          let result = f t in
          Option.iter t.checkpoint_failure ~f:(fun (exn, trace) ->
            Stdlib.Printexc.raise_with_backtrace exn trace);
          if t.rejected
          then Error Error.Work_limit
          else Ok (Result.map result ~f:(fun value -> value, t.used))
        with
        | Rejected owner when phys_equal owner t.owner -> Error Error.Work_limit))
;;

let compare_string t left right =
  work t;
  let left_length = String.length left in
  let right_length = String.length right in
  let length = Int.min left_length right_length in
  let rec chunks offset =
    if offset >= length
    then Int.compare left_length right_length
    else (
      work t;
      let stop = offset + Int.min 1024 (length - offset) in
      let rec compare_bytes offset =
        if offset >= stop
        then 0
        else (
          let result = Char.compare (String.get left offset) (String.get right offset) in
          if Int.equal result 0 then compare_bytes (offset + 1) else result)
      in
      let result = compare_bytes offset in
      if Int.equal result 0 then chunks stop else result)
  in
  chunks 0
;;
