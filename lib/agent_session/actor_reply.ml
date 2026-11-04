open! Core

type 'a t =
  | Returned of ('a, Agent_protocol.Error.t) result
  | Raised of exn * Stdlib.Printexc.raw_backtrace

let resolve resolver result = Eio.Promise.resolve resolver (Returned result)

let resolve_exception resolver exn backtrace =
  Eio.Promise.resolve resolver (Raised (exn, backtrace))
;;

let await promise =
  match Eio.Promise.await promise with
  | Returned result -> result
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;
