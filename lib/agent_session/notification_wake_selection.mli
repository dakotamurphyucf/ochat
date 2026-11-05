(** Pure routing of actual retained notification wake obligations. This does not
    grant execution or create a Run/Operation association. Publication of matching
    history remains factual and separate from admission of a requested turn. *)
module Context : sig
  type t =
    | Ordinary
    | Goal_wake of Agent_protocol.Run_wake.t
    (** [Goal_wake] is selection DATA. Its Actor caller must independently validate
      the actual physical installed runtime scope and unconsumed Goal wake before
      using the result for admission. Constructing this value gives no authority. *)
end

(** Preserve input order. An ordinary execution may consume unrelated ordinary
    notifications, but cannot consume an exact still-Waiting Goal occurrence,
    even when its older Operation belongs to that same Run. A properly admitted
    Goal wake names only its exact retained obligation and must remain eligible.
    Conflicting or invalid actual Wait associations fail closed. *)
val select
  :  Session_state.t
  -> context:Context.t
  -> Agent_protocol.Delivery.t list
  -> (Agent_protocol.Delivery.t list, Agent_protocol.Error.t) result
