open! Core

module Exit = struct
  type kind =
    | Returned
    | Cancelled
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  type completion =
    | Selecting
    | Sample of Recurrence_runtime.Completion.sample
    | Clock_failed of exn * Stdlib.Printexc.raw_backtrace

  type t =
    { kind : kind
    ; completion : completion
    }
end

type extent =
  | Waiting
  | Running
  | Exited of Exit.t

type t =
  { domain : int
  ; sample : unit -> Recurrence_runtime.Completion.sample
  ; mutable extent : extent
  }

let check_domain t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "constructor Model producer belongs to another Eio domain"
;;

let create ~sample = { domain = (Stdlib.Domain.self () :> int); sample; extent = Waiting }

let record_exit t kind =
  t.extent <- Exited { kind; completion = Selecting };
  let completion =
    try Exit.Sample (t.sample ()) with
    | exn -> Exit.Clock_failed (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  t.extent <- Exited { kind; completion }
;;

let with_extent t f =
  check_domain t;
  (match t.extent with
   | Waiting -> t.extent <- Running
   | Running | Exited _ -> invalid_arg "constructor Model producer was already entered");
  match f () with
  | value ->
    record_exit t Returned;
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let kind =
      match exn with
      | Eio.Cancel.Cancelled _ -> Exit.Cancelled
      | _ -> Exit.Raised (exn, backtrace)
    in
    record_exit t kind;
    Exn.raise_with_original_backtrace exn backtrace
;;

let exit t =
  check_domain t;
  match t.extent with
  | Exited exit -> Some exit
  | Waiting | Running -> None
;;
