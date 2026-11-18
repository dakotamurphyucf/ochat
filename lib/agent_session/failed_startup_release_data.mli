open! Core

(** Private naming cells and atomic publication for ONE supported failed startup.
    These cells alone grant no permission. Only Actor's validated nonyielding
    release handler calls Gate.release; there is no public issuer or setter. *)
module Receipt : sig
  type t

  val check_domain_exn : t -> unit
  val same_startup : t -> Failed_startup_data.t -> bool
end

type t

val create : issuer:unit ref -> domain:int -> startup:Failed_startup_data.t -> t
val belongs_to : t -> issuer:unit ref -> startup:Failed_startup_data.t -> bool
val released : t -> (Receipt.t option, Agent_protocol.Error.t) result

module Gate : sig
  type request = t
  type t

  val create : unit -> t

  (** Creating-domain read. Foreign reads are invariant violations. *)
  val is_closed : t -> bool

  (** Open accepts naming data; closed accepts only the SAME published request.
      This never substitutes the Actor handler's actual custody checks. *)
  val allows : t -> request -> bool

  (** Actor-only actual publication after all original/fresh validations. Stores
      SAME receipt on request and gate atomically, without yielding or callbacks.
      A repeated SAME request returns that receipt; other requests cannot publish.
      Foreign/wrong affinity refuses before mutation. *)
  val release
    :  t
    -> request
    -> issuer:unit ref
    -> startup:Failed_startup_data.t
    -> (Receipt.t, Agent_protocol.Error.t) result
end

(** Pure DATA classifier, not authority. Only completed writer-entered failure
    without any ACK is eligible for the concrete backend check. *)
val failed_before_ack
  :  progress:Session_commit_observation.Progress.t
  -> outcome:Session_commit_observation.Outcome.t
  -> bool

(** Read only the SAME bounded original startup/input facts. Requires an original
    failed End, actual input producer exit, positive normal Root_attempt_unconfirmed
    close and the requested root's completed writer-entered failure without ACK.
    Producer Error/exception still counts as an exit. Progress/outcome are read
    only after the original receipt prerequisites, preserving the Actor's old
    short-circuit order. This Boolean is not disposal or execution authority. *)
val original_input_joined : Failed_startup_data.t -> bool

type authored_cut =
  | First_lifecycle of Failed_startup_data.Authored.t
  | Final_aggregate of
      Failed_startup_data.Authored.t * Failed_startup_data.Authored.settled_lifecycle

(** Classify the original joined producer's exact failed write and original
    failed End. Late settlement also requires the actual acknowledged legacy
    lifecycle or the actual successful extension callback exit, Finish and
    release. The Actor independently checks current scope and retirement. *)
val original_authored_joined
  :  Failed_startup_data.t
  -> (authored_cut option, Agent_protocol.Error.t) result

(** The exact historical scope may still list its finished graphs. Each must
    belong to the original Actor and generation and retain its own native ACK
    after final clear. *)
val authored_graphs_joined
  :  Inference_graph_owner.t list
  -> issuer:unit ref
  -> generation:int
  -> (bool, Agent_protocol.Error.t) result
