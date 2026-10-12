open Core

(** Captured parser boundary shared by replay and reparse. Tag only exceptions
    from the borrowed checkpoint, restore their exact identity/raw backtrace,
    and preserve Eio cancellation/timeout. Interpret other failures using the
    caller's existing parser diagnostic policy. No callback may escape [f]. *)
val run
  :  checkpoint:(unit -> unit)
  -> f:((unit -> unit) -> 'a)
  -> on_parser_error:(exn -> 'a)
  -> 'a
