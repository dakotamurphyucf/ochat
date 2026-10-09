(** Revisioned authored decision, collected transactionally with the actual
    moderator checkpoint. Reading/constructing an action conveys no authority. *)
type t =
  | Continue
  | Wait of Run_wake.t
  | Finish of
      { terminal : Run.Terminal.t
      ; relinquish : Run_work.t list
        (** Pending independent work transferred to retained session ownership.
              Actor denies active callback/permission/action obligations and
              already-recorded adverse outcomes. Evidence remains immutable. *)
      }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** No action is neutral; identical requests coalesce. Incompatible surviving
    actions reject before checkpoint persistence. Task.catch discarded actions
    do not participate. *)
val combine : t option -> t option -> (t option, Error.t) result
