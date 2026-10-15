open! Core
open Authoring_context_service_core

(** Current owner/grant/selection checks live outside the short ledger gate.
    The final variants are bounded and nonyielding; the full variants may
    perform joined Store verification and must complete before publication. *)
val source_host_error : Sources.Error.t -> Error.t

val upload_host_error : Upload.Error.t -> Error.t

val check_context_owner
  :  ?ledger:Ledger.t
  -> t
  -> context_entry
  -> actor:Operator_authorization.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result

val check_context_selection_now
  :  ?ledger:Ledger.t
  -> t
  -> context_entry
  -> actor:Operator_authorization.t
  -> (unit, Error.t) result

module Candidate_pin : sig
  type t

  (** Freeze only comparison evidence while a genuine [Current] borrow is
      live: the physical Core/connection/negotiation/context entries, exact
      Expected identity+basis+generation, and a Document's retained Parsed
      capture head. No Current borrow, live read grant or mutable cache owner
      escapes. Unrelated analysis result heads need not remain unchanged. *)
  val of_current
    :  Authoring_context_service_core.t
    -> Authoring_context_service_core.Current.t
    -> connection:Connection_context.t
    -> (t, Error.t) result

  (** Called within each later [with_current] full check; rejects a replacement
      context or Parsed head even when a label/digest happens to match. *)
  val matches_current
    :  t
    -> Authoring_context_service_core.Current.t
    -> (unit, Error.t) result

  (** Bounded NONYIELDING final check on the creating domain. Core.try_with_gate
      compares the active physical connection/negotiation lease, context object,
      Expected generation/basis and relevant Document capture head. A busy gate
      rejects instead of waiting. After releasing that short gate, the existing
      original/current actor and source/upload/supplied selection checks run,
      followed by the host's exact developer target-policy comparison. No
      native read, Store I/O, callback or wait occurs under the Core gate. *)
  val check_now : t -> actor:Operator_authorization.t -> (unit, Error.t) result
end

(** [feature] chooses the negotiated sibling; absent means Core authoring.
    [final_check] adds a bounded nonyielding current host selection check
    immediately before the final gate, after the ordinary actor/source/context
    recheck. [publish] runs inside that gate only after the exact connection,
    context and expected generation are compared again. It may perform a pure
    compare-and-swap of already prepared immutable values only after comparing
    BOTH analysis cache heads, the certified analysis-index head, the
    provisional-index head, checked-template head and retained document-capture
    head borrowed
    at request admission; any changed head rejects the result. It constructs
    no new graph, parser result or wire response. The borrowed [Current] token
    has expired by then; neither callback may use it or perform I/O, wait or
    invoke a yielding checkpoint. An error publishes nothing. *)
val with_current
  :  ?feature:Selected_feature.t
  -> ?final_check:(unit -> (unit, Error.t) result)
  -> ?publish:('a -> ('a, Error.t) result)
  -> t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> checkpoint:(unit -> unit)
  -> f:(Current.t -> checkpoint:(unit -> unit) -> ('a, Error.t) result)
  -> ('a, Error.t) result

(** Borrow a parent from the SAME registered physical Core connection and
    context as [with_current], with Bundle_upload negotiated. [expected] is a
    lookup coordinate only: the issuer checks the actual Current and owner,
    then freezes a live operation scope for [f]. Project Inputs.base or one
    physically associated retained Document row set supplies prior-Build
    evidence when the manifest requires a previous lock. A Document parent
    without a required prior remains available before Analyze or after
    unrelated retained-capture eviction; an uploaded Project parent supplies its exact current
    Inputs.base plus the verified Context_capture Store pin. The scope
    expires before this function returns.
    Core's outer final actor/selection/head/context CAS remains in force after
    all yielding Host/Store work. No Core gate is held across [f]. *)
val with_parent_scope
  :  ?script_selection:Authoring_script_selection.Policy.t
  -> t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> expected:P.Authoring.Context.Expected.t
  -> operation:Authoring_parent_scope.Operation.t
  -> checkpoint:(unit -> unit)
  -> f:(Authoring_parent_scope.t -> checkpoint:(unit -> unit) -> ('a, Error.t) result)
  -> ('a, Error.t) result

val view_of_context
  :  t
  -> Ledger.t
  -> id:P.Id.Authoring_context.t
  -> identity:Ledger.Identity.t
  -> origin:P.Authoring.Context.origin
  -> checkpoint:(unit -> unit)
  -> (P.Authoring.Context.t, Error.t) result

val checked_response : t -> W.Response.t -> (W.Response.t, Error.t) result
