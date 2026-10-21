open! Core

(** Join resource fibers owned by one claimed issuing revision. Returning from
    the callback cancels and joins children before returning its value. A
    callback exception or cancellation retains its original exception and raw
    backtrace after children join; a secondary cleanup failure cannot replace
    it. If the callback succeeds but a child fails, report that child failure.
    This switch lends no Claim, source, registry or Store authority. *)
val with_switch : (Eio.Switch.t -> 'a) -> 'a
