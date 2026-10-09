(** Pure storage planning for admitted lifecycle authority and its complete
    catalog projection. No filesystem, actor, authorization or runtime effects. *)
open! Core

module R = Session_archive_record

type t

(** Absence means initial Automatic authority; callers separately establish that
    absence is consistent with their owned index. A document must match owner. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> Session_archive_document.t option
  -> (t, Store_error.t) Result.t

val value : t -> R.t
val document : t -> Session_archive_document.t option

(** Complete preserved carrier equality, including receipts and unknown data. *)
val equal : t -> t -> bool

module Target : sig
  type t =
    | Upsert of Session_index_entry.t
    | Remove of Agent_protocol.Id.Session.t
end

module Prepared : sig
  type t

  val document : t -> Session_archive_document.t
  val bytes : t -> string
  val outcome : t -> R.Outcome.t
  val target : t -> Target.t

  (** Exact outcome belongs to the admitted current head; old replay results do
      not prove current admission, even if generic completion is legitimate. *)
  val has_current_outcome : t -> bool
end

(** Validate exact old basis, coherent hints/identity and canonical anchor before
    preserving the bounded next carrier and full lifecycle projection. *)
val prepare
  :  t
  -> current_entry:Session_index_entry.t
  -> transition:R.Prepared.t
  -> now:Agent_protocol.Timestamp.t
  -> (Prepared.t, Store_error.t) Result.t

(** Caller proves exact original generic durable completion before this lawful
    immutable receipt acknowledgement. Terminal Remove proof stays retained. *)
val acknowledge
  :  t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> (t, Store_error.t) Result.t

val encoded_document : Session_archive_document.t -> (string, Store_error.t) Result.t
