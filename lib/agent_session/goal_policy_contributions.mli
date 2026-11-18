(** Stable per-Goal causal contribution books. Bounded DATA only: decoding,
    receipt mutation and projections grant no execution/original authority or
    proof of ACK/writer absence. The physical canonical ledger remains sole
    provider usage owner. Compatible raw unknown children remain preserved. *)
module PC = Policy_canonical_usage

module Cause = Policy_cause_data
module P = Agent_protocol
module D = Document_schema

(* Fixed complete document profile:4MiB/depth96/280000fields/560000nodes;
   at most16 stable Goal books;64 LIFETIME streams TOTAL, counting stored
   remote slots PLUS self-targeted streams in actual595 inventory. No duplicate
   inline rows. Component has one owner Session_ref, not current-source key.
   No slot/book deletion or reuse; no stream/nonce/prefix reset. *)
(* Intentional nominal bridge to the ONE private carrier implementation, as for
   canonical policy usage. The Dune-private representation stays hidden from
   installed consumers; trusted in-library capacity checks need no conversion. *)
type t = Goal_policy_contributions_carrier.t

val limits : D.Limits.t
val max_books : int
val max_streams : int

(** Retained full owner DATA, not runtime authority. *)
val owner : t -> P.Session_ref.t

module History : sig
  type t =
    | Known_from_creation
    | Historical_unknown
  [@@deriving equal, sexp_of]
end

module Admission_context : sig
  type t =
    | Ordinary_existing
    | Delegated_policy_causal
  [@@deriving equal, sexp_of]
end

module Pending : sig
  type t [@@deriving equal, sexp_of]

  (** Exact root-specific forwarding witness digest, derived by Request from the
      same Cause; absent only for a direct stream. Never admission/ACK evidence. *)
  val forwarding_sha256 : t -> string option

  val owner : t -> P.Session_ref.t
  val goal_id : t -> P.Id.Goal.t
  val stream_id : t -> string
  val nonce : t -> int64
  val next_prefix : t -> int64
  val occurrence : t -> Goal_descendant_occurrence.t
  val cause : t -> Cause.Key.t
  val work : t -> Cause.Allocation_work.t

  (* Also retains expected parent Event, nullable caller_origin, original
     child_invocation/admission hash. Source/path/target are the immutable
     slot descriptor, not duplicated in every receipt. DATA never proves ACK. *)
  val shape : D.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

module Request : sig
  type t

  val create
    :  Cause.t
    -> target:Cause.Goal_target.t
    -> work:Cause.Allocation_work.t
    -> occurrence:Goal_descendant_occurrence.t
    -> (t, P.Error.t) result

  val stream : t -> PC.Stream.t
  (* Future Attempt only: same callee/full source/generation, exact nominal
     work coordinates. Full cause is retained in Reserve reason for replay;
     actual extant member/original selection remains597. *)
end

module Disposition : sig
  type t =
    | Added
    | Replaced
    | Duplicate
    | Stale
  [@@deriving equal, sexp_of]
end

module Update : sig
  type accounting = t

  module Reason : sig
    type t =
      | Initialize_goal of P.Id.Goal.t
      | Reserve of Request.t
      | Import of PC.Projection.t
      | Close_before_writer of Pending.t
  end

  type t

  val candidate : t -> accounting
  val reason : t -> Reason.t
  val shape : D.Shape.t
  val to_json : t -> (Jsonaf.t, P.Error.t) result
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

(* No unchecked public empty/create function. State validates this ONLY at
   its actual ordered missing-to-present Goal/account creation witness. *)
val initialize_goal
  :  t option
  -> owner:P.Session_ref.t
  -> goal:P.Goal.t
  -> account:Goal_accounting_document.t
  -> before_ledger:Inference_ledger.t
  -> (Update.t, P.Error.t) result

module Reserve_result : sig
  type t =
    | Reserved of Update.t * Pending.t
    | Already_pending of Pending.t
    | Busy of Pending.t
end

val reserve
  :  t option
  -> owner:P.Session_ref.t
  -> request:Request.t
  -> (Reserve_result.t, P.Error.t) result
(* Pure receipt mutation AFTER the shared fresh view allows it. Actual State
   prefix validation repeats that check. A missing existing Goal book starts
   Historical_unknown, never Known_from_creation. No causal receipt for self:
   request.stream.callee=owner rejects; inline derives from actual H/PC ACK. *)

val import
  :  t
  -> projection:PC.Projection.t
  -> (Update.t * Disposition.t, P.Error.t) result

(* Exact next-prefix pending + matching occurrence/cause/work/original witness
   only. Same prefix/newer real observation replaces whole usage, not +usage.
   Duplicate/Stale preserves component bytes and cannot consume any pending.
   Import into self callee rejects; it cannot bypass actual local canonical. *)
val close_before_writer : t -> pending:Pending.t -> (Update.t, P.Error.t) result
(* Exact current pending or latest exact No_writer replay. Changes no admitted
   prefix/usage. Runtime597 supplies actual same no-future-writer seal. *)

(* Inline count/head/usage is ALWAYS derived from the exact self-targeted
   lifetime595 stream inventory, including valid pre596 streams with no book.
   No stored inline descriptor/action or local pending/nonce/RPC exists. New
   inline H uses actual existing595 one-State ACK and ordered count/capacity
   check; correction/retirement/reset immediately changes derived totals. *)

val validate_context
  :  t
  -> owner:P.Session_ref.t
  -> catalog:Goal_catalog.t option
  -> accounts:Goal_accounts.t option
  -> canonical:PC.t option
  -> ledger:Inference_ledger.t
  -> session_revision:int64
  -> (unit, P.Error.t) result

(* Stable Goal ID/account must exist, including Closed. Original frozen target
   source/episode may outlive current installation. Actual derived-inline plus
   stored-remote stream count is<=64; remote slots contain no foreign Handle. *)
val adopt : t -> incoming:t -> (t, P.Error.t) result
val reserved_growth : t -> (Capacity_growth.t, P.Error.t) result
val required_depth : t -> (int, P.Error.t) result
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, P.Error.t) result
val of_document : D.Document.t -> (t, P.Error.t) result
val to_document : t -> (D.Document.t, P.Error.t) result
val shape : D.Shape.t

module Subtotal : sig
  type t

  val usage : t -> Goal_usage.t
  val history : t -> History.t
  val pending : t -> Goal_descendant_occurrence.t list
end

(** Validate complete owner/book/account and actual595 frontier once. Select the
    named actual Goal and full owner; historical sources/episodes remain in their
    immutable stream descriptors, not rebound to the current Goal source. Include
    all actual self-targeted595 lifetime streams, even if the new book is absent.
    Historical absence remains unknown while its known595 usage still counts.
    Return remote pending occurrences, never a reservation/confirmation witness. *)
val subtotal
  :  t option
  -> owner:Agent_protocol.Session_ref.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> catalog:Goal_catalog.t option
  -> accounts:Goal_accounts.t option
  -> canonical:Policy_canonical_usage.t option
  -> ledger:Inference_ledger.t
  -> session_revision:int64
  -> (Subtotal.t, Agent_protocol.Error.t) result
