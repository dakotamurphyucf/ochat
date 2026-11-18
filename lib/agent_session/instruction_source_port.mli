open! Core
module P = Agent_protocol
module Request = Instruction_source_request

type t

(** Trusted installation adapter over the existing source owner. Constructing
    callbacks is not evidence of a source read. Production is installed only by
    the genuine Authoring_installation facade and qualified through that owner.
    [check_now] is nonyielding and checks the exact original actor and real roots.
    All callbacks enforce finite shared request/result/work bounds. *)
module Backend : sig
  type t

  val create
    :  roots:Instruction_content.Root_id.t list
    -> check_now:(Instruction_content.Root_id.t list -> (unit, P.Error.t) result)
    -> refresh:
         (Request.Selection.t
          -> checkpoint:(unit -> unit)
          -> (Request.Discovery.t, P.Error.t) result)
    -> capture:
         (Request.t
          -> checkpoint:(unit -> unit)
          -> (Instruction_capture.t, P.Error.t) result)
    -> t
end

val create
  :  open_use:
       (Operator_authorization.t
        -> checkpoint:(unit -> unit)
        -> (Backend.t, P.Error.t) result)
  -> t

module Use : sig
  (** Callback-owned joined use. Retaining a value never extends its epoch. *)
  type t

  val roots : t -> Instruction_content.Root_id.t list

  val check_now
    :  t
    -> roots:Instruction_content.Root_id.t list
    -> (unit, P.Error.t) result
end

module Captured : sig
  (** Opaque port/use issuer plus genuine callback-produced content. There is no
      public Loaded/Snapshot-to-Captured constructor and no serializer. *)
  type t

  val contents : t -> Instruction_capture.t
  val check_now : t -> use:Use.t -> (unit, P.Error.t) result
end

(** Keep this bracket OPEN across the actual source operation/actor mutation or
    provider call, including all submission guards. Never return a guard which
    retains this Use after its callback returns. This bracket does not recheck
    after [f]: a later revocation cannot relabel an acknowledged mutation or
    successful provider completion. Read/disclosure checks and final effect/
    submission admission belong to their actual consumer. Cancellation is not converted. *)
val with_use
  :  t
  -> actor:Operator_authorization.t
  -> checkpoint:(unit -> unit)
  -> (Use.t -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

val refresh
  :  Use.t
  -> selection:Request.Selection.t
  -> checkpoint:(unit -> unit)
  -> (Request.Discovery.t, P.Error.t) result

val capture
  :  Use.t
  -> request:Request.t
  -> checkpoint:(unit -> unit)
  -> (Captured.t, P.Error.t) result
