open! Core
module P = Agent_protocol
module Bundle = P.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Upload = Authoring_upload_host
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Rows = Authoring_document_selection_rows
module Selection = Authoring_script_selection

(** One operation-scoped physical Core parent borrow. Only
    [Authoring_context_owner] issues it from its live registered Current for the
    same connection and selected Bundle_upload feature. It never owns a second
    source reader, Store or mutable context registry. *)
type t

module Error : sig
  type t =
    | Foreign_operation
    | Stale_parent
    | Unauthorized
    | Busy
    | Prior_unavailable
  [@@deriving sexp_of]
end

module Operation = Upload.Parent_admission.Operation

module Origin : sig
  type selected_document_evidence =
    { capture : Sources.Document_context.Parsed_capture.t
    ; rows : Rows.t
    ; original_actor : Operator_authorization.t
    ; script_selection : Selection.Policy.t option
    }

  type document_evidence =
    { inputs : Ledger.Document.Inputs.t
    ; selected : selected_document_evidence option
    }

  type t =
    | Project of Chatml_project_build.t
    | Document of document_evidence
    | Uploaded_project of
        { previous : Chatml_project_build.t
        ; pin : Store.Published_pin.t
        }
end

(** Private issuer called only by Owner after genuine Current/owner validation.
    [check_current_now] closes over the actual Core owner/context, original and
    request actors, negotiation lease and borrowed heads. It uses Core's
    nonyielding try-lock, then compares the current physical connection/context,
    expected generation, grants and selected policy. Busy is returned rather
    than waiting while Upload_host or Store owns its gate. It does no source or
    Store I/O and does not call either gate recursively. A Document parent
    always carries current Inputs and may carry selected parser/row evidence.
    The issuer validates that optional evidence's exact physical association;
    missing or stale retained evidence does not deny a no-prior publication.
    An uploaded Project's [previous] is the exact CURRENT Ledger Inputs.base
    borrowed with its physical Context_capture pin, not a stale copy of the
    initial Rebound. Context_open_uploaded admits Project basis only. [live]
    becomes false when Owner.with_current exits; the scope cannot be
    reused on another request, connection or domain. *)
val issue
  :  operation:Operation.t
  -> host:P.Id.Server.t
  -> connection:Connection_context.t
  -> actor:Operator_authorization.t
  -> expected:P.Authoring.Context.Expected.t
  -> live:bool ref
  -> check_current_now:(unit -> (unit, Error.t) result)
  -> origin:Origin.t
  -> (t, Error.t) result

(** Repeat the issuing operation, physical connection and host, exact persisted
    parent Expected, live request/domain, original/current actors and current
    selection through the issuer's bounded try-lock. This is required before
    Host/Store admission, after yielding checkpoints and at the Store marker's
    final admission. No wire Expected alone passes this check. *)
val check_now
  :  t
  -> operation:Operation.t
  -> host:P.Id.Server.t
  -> connection:Connection_context.t
  -> actor:Operator_authorization.t
  -> expected:P.Authoring.Context.Expected.t
  -> (unit, Error.t) result

(** Select an authentic immutable prior Build for this exact manifest target.
    Project parents use their admitted original Inputs.base, including when an
    unsaved overlay has changed; Exact_lock requires no previous Build. Document
    parents require retained selected evidence only when Compose names a
    selected_previous_lock. Then they require one unique parser
    input/declaration/selector and selected row associated with the retained
    physical capture. An independently
    selected Document row additionally requires the original installed script
    policy to reselect that same admitted Build with both original and current
    actors, actual Parsed/declaration/metadata and current Inputs. Missing or
    changed policy selection is unavailable; no native fallback is inferred.
    Uploaded Project
    parents use their current original Inputs.base, including after an edited
    overlay, plus the same-Store pin. For Compose
    with a required selected_previous_lock, missing, ambiguous or mismatched
    evidence returns Prior_unavailable, never an invented None. The lower
    captured-only validator still checks exact lock ID/digest/length and all
    authored route, source, environment and contract constraints. The walk is
    finite and charged through [checkpoint]; it performs no read or parse. *)
val previous_for
  :  t
  -> Bundle.Manifest.t
  -> selector:Bundle.Manifest.Script.Selector.t option
  -> checkpoint:(unit -> unit)
  -> (Chatml_project_build.t option, Error.t) result

(** The actual uploaded parent's Store pin, if any. This comparison value is
    never a read grant; Store.publish checks it inside its already-held gate. *)
val parent_pin : t -> Store.Published_pin.t option

(** Adapt this private physical borrow to Upload_host's explicitly trusted
    callback port. The installed bundle handler obtains the port only here;
    the public port constructor is a host integration contract and never
    manufactures authority from a wire Expected. No private Core/Rows type is
    exposed through Upload_host's public interface. *)
val admission : t -> Upload.Parent_admission.t
