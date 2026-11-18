(** Private complete preserved carrier, capacity and exact canonical/witness checks.
    This is the sole JSON/component representation; no runtime authority. *)
module Data = Policy_canonical_usage_data

module Cause = Policy_cause_data
module Context = Cause.Context
module Choice = Data.Choice
module Binding = Data.Binding
module Stream = Data.Stream

type t = Data.t Document_schema.Extension_carrier.t

val invalid : string -> ('a, Agent_protocol.Error.t) result
val domain : ('a, Document_schema.Error.t) result -> ('a, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
val max_causes : int
val max_memberships : int
val max_streams : int
val max_bindings : int
val codec : Data.t Document_schema.Domain_codec.t
val probe_codec : Jsonaf.t Document_schema.Domain_codec.t
val value : t -> Data.t
val bindings : t -> Data.Binding.t list
val encoded : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result
val equal_encoded : t -> t -> (bool, Agent_protocol.Error.t) result

val marks
  :  Inference_ledger.t
  -> (Canonical_usage_fold.Marks.t, Agent_protocol.Error.t) result

val same_ledger
  :  Inference_ledger.t
  -> Inference_ledger.t
  -> (bool, Agent_protocol.Error.t) result

val admit : t -> (t, Agent_protocol.Error.t) result
val with_value : t -> Data.t -> (t, Agent_protocol.Error.t) result
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result
val of_document : Document_schema.Document.t -> (t, Agent_protocol.Error.t) result
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result

val exact_current
  :  t
  -> context:Context.t
  -> ledger:Inference_ledger.t
  -> (unit, Agent_protocol.Error.t) result

val validate
  :  t
  -> context:Context.t
  -> ledger:Inference_ledger.t
  -> (unit, Agent_protocol.Error.t) result

val protected_ordinals
  :  t
  -> ledger:Inference_ledger.t
  -> (Core.Int64.Set.t, Agent_protocol.Error.t) result

(** One call-local capacity receiver prepared from the same complete immutable
    carrier. Preparation captures current and future documents only; it does not
    reserve growth or measure depth. Each operation retains its
    own validation order, and [validate] checks the supplied complete raw child.
    A complete [Export] can retain this receiver for its same serializer's later
    host check. Independent State or raw-admission checks prepare separately. *)
module Capacity_documents : sig
  type carrier = t
  type t

  val prepare : carrier -> (t, Agent_protocol.Error.t) result
  val required_depth : t -> (int, Agent_protocol.Error.t) result
  val validate : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result
  val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
end

(** One complete export for a single serializer operation. Preparation performs
    all [to_document] checks in their original order and retains the exact complete
    document and its capacity receiver. A State serializer may reuse that receiver
    only for its own later host check, which still validates its actual supplied
    raw child. No result is attached to a carrier or retained across operations,
    revisions or domains; no authority or independent admission proof is granted. *)
module Export : sig
  type source = t
  type t

  val prepare : source -> (t, Agent_protocol.Error.t) result
  val document : t -> Document_schema.Document.t
  val capacity : t -> Capacity_documents.t
end

val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val required_depth : t -> (int, Agent_protocol.Error.t) result
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result

val projection_raw
  :  t
  -> Document_schema.Document.t
  -> Data.Partition.t
  -> session_revision:int64
  -> usage:Jsonaf.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

val witness_shape : Document_schema.Shape.t
val head_shape : Document_schema.Shape.t
val projection_shape : Document_schema.Shape.t

(** Establish complete copied Cause/Stream children through existing decode/adopt,
    preserving the exact planned known value and existing protected extensions.
    Full current and projection capacity is checked before returning. *)
val retain_children : t -> cause:Cause.t option -> (t, Agent_protocol.Error.t) result
