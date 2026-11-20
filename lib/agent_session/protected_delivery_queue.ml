open! Core
module Delivery = Inference_runtime.Protected_delivery

type state =
  | Open of Delivery.t list
  | Closed

type t = state Atomic.t

let create () = Atomic.make (Open [])

let close t =
  match Atomic.exchange t Closed with
  | Closed -> ()
  | Open prior -> List.iter prior ~f:Delivery.retire
;;

let rec add t delivery =
  match Atomic.get t with
  | Closed ->
    Delivery.retire delivery;
    Error (Agent_protocol.Error.invalid_request "protected result operation has ended")
  | Open prior as before ->
    if Stdlib.List.compare_length_with prior Delivery.max_outputs_per_submission >= 0
    then (
      close t;
      Delivery.retire delivery;
      Error
        (Agent_protocol.Error.create
           Resource_limit
           ~message:"protected result batch exceeds the input limit"
           ~retryable:false
           ()))
    else if Atomic.compare_and_set t before (Open (delivery :: prior))
    then Ok ()
    else add t delivery
;;

let rec take t =
  match Atomic.get t with
  | Closed | Open [] -> []
  | Open prior as before ->
    if Atomic.compare_and_set t before (Open []) then List.rev prior else take t
;;
