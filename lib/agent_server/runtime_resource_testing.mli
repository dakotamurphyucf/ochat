open! Core
module A = Agent_session
module P = Agent_protocol
module B = A.Runtime_builder
module Actor = A.Session_actor
module Owner = Runtime_owner
module CO = A.Close_observation

type 'a result = ('a, P.Error.t) Result.t

(** Intentional trusted-host testing boundary, absent from protocol transport
    and production handlers. Owns one actual private Cell with the supplied
    Actor/Handle/source/profile. No permission, source or Joined receipt is issued.
    Existing current Caller/restoration/Owner admission checks remain necessary.

    Domain and original-open checks precede fresh selection, disclosure, pin,
    publication and resource use. Exiting [with_probe] expires only those uses:
    it neither marks the Cell closed nor reports cleanup success. Existing
    callback/finalizer order remains the caller's responsibility. Retained
    cleanup and pure target facts remain available on the same creating domain;
    expiration cannot strand an acquired loan or suppress its real failure. *)
type t

type probe = t

val with_probe
  :  actor:Actor.t
  -> handle:Agent_store.Session_store.Handle.t
  -> source:B.Source.t
  -> profile:A.Permission_policy.t
  -> (t -> 'a)
  -> 'a

(** Pure factual pending query and exact same-Cell cleanup. Valid after expiry;
    no fresh selection or cleanup success is inferred from an absent borrower. *)
val pending : t -> bool

val close_pending : t -> unit result
val close_pending_reported : t -> unit result CO.t

(** Pure same-Cell cleanup adapter, available on the creating domain before or
    after expiry. Its callbacks retain no fresh selection/use authority and
    forward the actual Cell cleanup results unchanged. *)
val cleanup_port : t -> Owner.Policy_resource_cleanup.t

module Materialization : sig
  (** Actual successful public Builder objects from this invocation. *)
  type t =
    { product : B.Product.t
    ; binding : B.Product.Binding.t
    ; physical : B.t
    }
end

module Target : sig
  (** Actual Cell-elected immutable identity, never permission or completion. *)
  type t

  module Status : sig
    type t =
      | Held of
          { native : B.Native_identity.t option
          ; product : B.Product_identity.t option
          }
      | Released
  end

  val equal : t -> t -> bool

  (** Forward only this genuine issued target to the Owner's fixed Cell check.
      No target, cleanup authority or new physical owner is issued. *)
  val capture_consumer : t -> owner:Owner.t -> Owner.Resource_consumer.t result

  (** Domain and exact Cell association, then pure factual current view. Allowed
      after scope expiry; old identity is never rebound to a replacement unit. *)
  val status : t -> cell:probe -> Status.t result

  (** Execute the existing exact foreign-Cell check, returning its actual Error.
      The internal distinct empty Cell supplies no execution/cleanup authority. *)
  val check_foreign : t -> unit result
end

module Loan : sig
  (** SAME already-public abstract loan identity; no Cell MLI is required. *)
  type t = Owner.Policy_resource_access.loan

  (** Cleanup is domain-checked and remains available after root scope expiry. *)
  val close : t -> unit result

  val close_reported : t -> unit result CO.t
  val target : t -> Target.t

  (** Disclosure requires the root probe open and the SAME actual loan/Cell. *)
  val product : t -> cell:probe -> B.Product.t result

  val runtime : t -> cell:probe -> B.t result
end

module Native_loan : sig
  type t

  val target : t -> Target.t
  val close : t -> unit result
  val close_reported : t -> unit result CO.t
  val check_current : t -> cell:probe -> expected:A.Session_state.t -> unit result

  val check_retained_source
    :  t
    -> source:B.Source.t
    -> expected:A.Session_state.t
    -> unit result

  val with_resources : t -> f:(B.resources -> 'a result) -> 'a result

  val with_source_resources
    :  t
    -> f:(source:B.Source.t -> B.resources -> 'a result)
    -> 'a result
end

module Ordinary : sig
  type t

  (** Selects actual Cell construction after genuine restoration/current checks.
      [build] captures each actual Product before its next yield. Returned
      physical runtime is recorded before [f]; old capture/finally order remains.
      No private Key/Build or arbitrary closed-success setter escapes. Actual
      caller/restoration/source admission remains the host's responsibility;
      this adapter does not validate an extra restoration token. Original
      Key/select protocol failures keep their raised projection before callback
      entry; the new expired-scope refusal is returned as Error. *)
  val with_probe
    :  probe
    -> basis:A.Policy_restore_basis.t
    -> check:(unit -> unit result)
    -> expected:A.Session_state.t
    -> build:(t -> capture_tracking:(Graph_tracking.t -> unit) -> B.t result)
    -> f:(t -> 'a result)
    -> 'a result

  val capture_product
    :  t
    -> product:B.Product.t
    -> binding:B.Product.Binding.t
    -> unit result

  val expected : t -> A.Session_state.t
  val access : t -> B.Restored_policy_access.t result
  val publish : t -> Loan.t result
  val target : t -> Target.t

  (** Cleanup uses THIS actual original physical closer, followed by the existing
      Build acknowledgement only on success. Checks exact current ownership
      before invoking effects. Repeated successful calls re-observe the actual
      completion; a released old target never closes a replacement. Valid after
      expiry, domain-first; actual failure and retry behavior remain unchanged. *)
  val close_original : t -> unit result
end

module Native : sig
  type t

  (** Attach actual Cell scope before Driver fork. Actual host/graph callback,
      Child capture, input callback and Prepared_native.prepare keep their old
      order; SAME Lifetime owns local/outer cleanup. Genuine restoration and
      current source validation remain host-owned and run in their original
      order; no extra restoration token is accepted or claimed validated here.
      Original Key/select protocol failures may raise before callback entry.
      No Scope/Lifetime handle escapes. *)
  val with_probe
    :  probe
    -> sw:Eio.Switch.t
    -> expected:A.Session_state.t
    -> host:(capture_tracking:(Graph_tracking.t -> unit) -> B.Native_host.t)
    -> input:(unit -> B.Native_input.t result)
    -> on_physical_release:(unit -> unit)
    -> f:(t -> 'a result)
    -> 'a result

  val expected : t -> A.Session_state.t
  val basis : t -> A.Policy_restore_basis.t
  val prepared : t -> B.Prepared_native.t result
  val loan : t -> Native_loan.t
  val target : t -> Target.t
end

module Attachment : sig
  type t

  (** Capture the actual Product_attachment before [build] can yield; attach
      actual returned Product/binding/physical runtime before [f]. The actual
      original reservation preserves its raised protocol-failure projection. *)
  val with_probe
    :  Native.t
    -> build:
         (B.Prepared_native.t
          -> attachment:B.Product_attachment.t
          -> Materialization.t result)
    -> f:(t -> B.Product.t -> 'a result)
    -> 'a result

  val prepared : t -> B.Prepared_native.t result
  val binding : t -> B.Product.Binding.t result
  val target : t -> Target.t

  val publish
    :  t
    -> basis:A.Policy_restore_basis.t
    -> expected:A.Session_state.t
    -> (Loan.t * B.Restored_policy_access.t) result

  (** Delegates to THIS actual Cell promotion close, which already performs the
      physical close and acknowledgement together. Never separately pre-closes
      its runtime. Exact attachment ownership is checked before effects; repeated
      success re-observes that completion without retiring a later attachment.
      Domain-first and available after expiry; no free success setter. *)
  val close : t -> unit result
end

module Empty_partial : sig
  (** Actual unallocated ordinary selection, scoped to its original callback.
      A callback escape records that SAME actual Raised failure on its selection
      before preserving the exception/trace; it never leaves a Building unit. *)
  type t

  val with_ordinary
    :  probe
    -> expected:A.Session_state.t
    -> failure:P.Error.t
    -> during:(t -> unit)
    -> after_failure:(Target.t -> unit)
    -> unit result

  (** Native has no positive-acknowledgement operation; observe its real target
      before/after actual failure, then caller explicitly closes pending work. *)
  val with_native
    :  probe
    -> expected:A.Session_state.t
    -> failure:P.Error.t
    -> during:(Target.t -> unit)
    -> after_failure:(Target.t -> unit)
    -> unit result

  val target : t -> Target.t

  (** Try the existing unallocated acknowledgement only for THIS actual empty
      ordinary unit. Cannot acknowledge another resource or an expired callback. *)
  val try_unallocated_acknowledgement : t -> unit result
end

module Parent_cleanup : sig
  (** Cleanup-only opaque probe. No permission, producer or Completion issuer. *)
  type t

  val create : sw:Eio.Switch.t -> t
  val capture_release : t -> Owner.Native_release.t -> unit

  (** Exact actual provider Owner accompanies the original report. Duplicate
        record preserves the existing Invalid_argument invariant. *)
  val record : t -> owner:Owner.t -> 'a Owner.Native_borrow_outcome.t -> unit

  val close : t -> unit result CO.t
end

(** Fixed adapters use SAME Cell/Actor/Handle/profile and existing original Owner
    admission checks. Pin/disclosure requires root-open; retained cleanup does
    not. No private Native_lease type leaks into these public signatures. *)
val pin_native
  :  t
  -> expected_native:B.Native_identity.t
  -> expected:A.Session_state.t
  -> Native_loan.t result

val native_access
  :  t
  -> prepare:
       (admission:Owner.Native_resource_admission.t
        -> stage:Owner.Native_source_stage.t
        -> unit result)
  -> Owner.Native_resource_access.t

val policy_access
  :  t
  -> restore:
       (stage:Owner.Policy_restoration_source.t
        -> request:A.Script_tool_calls.preparation
        -> governance:Actor.Causal_governance.t
        -> authorize:(unit -> unit result)
        -> unit result)
  -> Owner.Policy_resource_access.t
