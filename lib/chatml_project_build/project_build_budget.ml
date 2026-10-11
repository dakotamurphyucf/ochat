open Core
module Model = Project_build_model
module Captured = Chatml_source

type t =
  { limits : Model.Limits.t
  ; poll : unit -> unit
  ; fail : Model.Error.problem -> unit
  ; mutable remaining_steps : int
  ; mutable input_bytes : int
  }

let checkpoint t =
  t.poll ();
  if t.remaining_steps = 0 then t.fail Limit_exceeded;
  t.remaining_steps <- t.remaining_steps - 1
;;

let charge_bytes t bytes =
  checkpoint t;
  if bytes < 0 || bytes > Int.max_value - t.input_bytes then t.fail Accounting_overflow;
  let total = t.input_bytes + bytes in
  if
    Option.exists (Model.Limits.max_input_bytes t.limits) ~f:(fun maximum ->
      total > maximum)
  then t.fail Limit_exceeded;
  t.input_bytes <- total
;;

let compare_string t left right =
  let length = Int.min (String.length left) (String.length right) in
  let rec loop offset =
    if offset mod 4096 = 0 then checkpoint t;
    if offset = length
    then Int.compare (String.length left) (String.length right)
    else (
      let order = Char.compare left.[offset] right.[offset] in
      if order = 0 then loop (offset + 1) else order)
  in
  loop 0
;;

let compare_id t left right =
  let order =
    compare_string
      t
      (Captured.Id.package_namespace left)
      (Captured.Id.package_namespace right)
  in
  if order = 0
  then compare_string t (Captured.Id.relative_path left) (Captured.Id.relative_path right)
  else order
;;

let equal_source t left right =
  compare_id t (Captured.id left) (Captured.id right) = 0
  && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
  && Int.equal (Captured.byte_length left) (Captured.byte_length right)
  && compare_string t (Captured.contents left) (Captured.contents right) = 0
;;

let run limits ~checkpoint:poll ~f =
  let exception Stop of Model.Error.problem in
  let maximum = Model.Limits.max_work_steps limits in
  let t =
    { limits
    ; poll
    ; fail = (fun problem -> raise (Stop problem))
    ; remaining_steps = maximum
    ; input_bytes = 0
    }
  in
  try
    checkpoint t;
    let open Result.Let_syntax in
    let%bind result = f t in
    checkpoint t;
    Ok (result, Model.Accounting.create ~work_steps:(maximum - t.remaining_steps))
  with
  | Stop problem -> Error (Model.Error.create problem)
;;
