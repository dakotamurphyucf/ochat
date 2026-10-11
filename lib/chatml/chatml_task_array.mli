open Chatml_lang

(** Private state owned by one interpretation, never by a reusable task
    description. Input and output have fresh outer arrays and shared payloads.
    Allocation, value checks and polling use the current host control. *)
type t

module Step : sig
  type t =
    | Call of
        { fn : value
        ; argument : value
        }
    | Finished of value
end

(** Snapshot the input now and reserve result storage before any callback runs.
    Control failures propagate unchanged. No callback is executed here. *)
val create
  :  value array
  -> f:value
  -> mode:Chatml_task_array_mode.t
  -> control:execution_control option
  -> t

(** Call once, then supply each completed callback result to [continue_with].
    Protocol misuse raises an internal invariant failure. *)
val start : t -> Step.t

(** Store one result and return the next step. Iteration requires unit results;
    malformed results return a runtime diagnostic. Control failures propagate.
    Total traversal costs O(n) plus callbacks, with O(n) private storage. *)
val continue_with : t -> value -> (Step.t, string) result
