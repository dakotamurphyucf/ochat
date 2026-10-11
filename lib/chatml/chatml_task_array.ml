open Core
open Chatml_lang

module Output = struct
  type t =
    | Collect of value array
    | Discard
end

module Cursor = struct
  type t =
    | Before_first
    | Waiting_for_result of int
    | Complete
end

type t =
  { input : value array
  ; f : value
  ; output : Output.t
  ; control : execution_control option
  ; mutable cursor : Cursor.t
  }

module Step = struct
  type t =
    | Call of
        { fn : value
        ; argument : value
        }
    | Finished of value
end

let create input ~f ~mode ~control =
  Option.iter control ~f:(fun control ->
    control.checkpoint ();
    control.check_value (VArray input);
    control.allocate_array (Array.length input);
    match mode with
    | Chatml_task_array_mode.Map -> control.allocate_array (Array.length input)
    | Iter -> ());
  let input = Array.copy input in
  let output =
    match mode with
    | Chatml_task_array_mode.Map ->
      Output.Collect (Array.create ~len:(Array.length input) VUnit)
    | Iter -> Output.Discard
  in
  { input; f; output; control; cursor = Before_first }
;;

let next t index =
  Option.iter t.control ~f:(fun control -> control.checkpoint ());
  if index = Array.length t.input
  then (
    t.cursor <- Complete;
    Step.Finished
      (match t.output with
       | Collect output -> VArray output
       | Discard -> VUnit))
  else (
    t.cursor <- Waiting_for_result index;
    Step.Call { fn = t.f; argument = t.input.(index) })
;;

let start t =
  match t.cursor with
  | Before_first -> next t 0
  | Waiting_for_result _ | Complete ->
    failwith "internal: array task interpretation started twice"
;;

let continue_with t value =
  match t.cursor with
  | Before_first | Complete -> failwith "internal: array task has no pending callback"
  | Waiting_for_result index ->
    Option.iter t.control ~f:(fun control -> control.check_value value);
    (match t.output, value with
     | Collect output, value ->
       output.(index) <- value;
       Ok (next t (index + 1))
     | Discard, VUnit -> Ok (next t (index + 1))
     | Discard, _ ->
       t.cursor <- Complete;
       Error "Task.iter_array: callback task must return unit")
;;
