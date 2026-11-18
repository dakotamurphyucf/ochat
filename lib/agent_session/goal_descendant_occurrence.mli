(** Immutable coordinates of a prospective actual leaf candidate. Construction and
    decoding convey DATA only, never canonical admission or execution authority. *)
module Frame : sig
  type t [@@deriving equal, sexp_of]

  (** Generation is nonnegative; candidate session revision is positive. The
      complete State transition validates the actual adjacent revision relation. *)
  val create
    :  session:Agent_protocol.Session_ref.t
    -> generation:int
    -> candidate_session_revision:int64
    -> (t, Agent_protocol.Error.t) result

  val session : t -> Agent_protocol.Session_ref.t
  val generation : t -> int
  val candidate_session_revision : t -> int64
end

module Kind : sig
  type t =
    | Attempt of
        { ordinal : int64
        ; operation_id : Agent_protocol.Id.Operation.t option
        ; invocation_id : Agent_protocol.Id.Invocation.t option
        }
    | Host_turn of { operation_id : Agent_protocol.Id.Operation.t }
  [@@deriving equal, sexp_of]
end

type t [@@deriving equal, sexp_of]

(** Attempt ordinal is positive. The actual canonical owner validates the optional
    operation/invocation attribution; their presence is not authority. *)
val attempt
  :  Frame.t
  -> ordinal:int64
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> invocation_id:Agent_protocol.Id.Invocation.t option
  -> (t, Agent_protocol.Error.t) result

val host_turn
  :  Frame.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> (t, Agent_protocol.Error.t) result

val frame : t -> Frame.t
val kind : t -> Kind.t
val dimension : t -> Goal_admission_policy.dimension
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Invalid capacity witness reserving maximum future identity/counter lengths;
    never decoded as canonical evidence. *)
val capacity_projection : unit -> Jsonaf.t

(** Maximum future coordinates retaining this occurrence's kind, so compatible
    unknown members remain preservable by the actual enclosing carrier. *)
val capacity_projection_for : t -> Jsonaf.t
