(** Local canonical and prepared-effective item traversal. The Factory owns
    its actor/source borrow and issues renewed, operation-scoped authority. *)
open! Core

module P = Agent_protocol
module Text = Recovery_item_text
module Source = Recovery_source_reader

type family = P.Recovery_history_request.family =
  | Canonical
  | Effective

type scope =
  | Current_generation
  | Window of P.Recovery_window.Scope.t

type action = P.Recovery_history_request.action =
  | List_items
  | Search of P.Search_term.t

type t

(** One selected family and one explicitly authorized text view. Ordinary
    includes both user and assistant. [ordinals] narrows half-open manifest or
    prepared request ordinals. Lists select one exact window. *)
val create
  :  source:P.Session_ref.t
  -> scope:scope
  -> family:family
  -> view:Text.view
  -> ?role:Text.role
  -> ?ordinals:int * int
  -> ?limit:int
  -> ?max_reply_bytes:int
  -> action
  -> (t, P.Error.t) Result.t

(** Shares the validated request parameters without bringing a cursor into the query digest. *)
val of_request
  :  ?include_image_parts:bool
  -> P.Recovery_history_request.t
  -> (t, P.Error.t) Result.t

val source : t -> P.Session_ref.t
val family : t -> family
val scope : t -> scope
val view : t -> Text.view
val action : t -> action
val digest : t -> Chatml_source.Digest.t

module Continuation : sig
  type highwater =
    { windows : int
    ; last_window_members : int
    ; last_preparation_revision : int64 option
    }

  type t = private
    { query : Chatml_source.Digest.t
    ; basis : Chatml_source.Digest.t
    ; highwater : highwater
    ; next_position : int
    }

  (** Private host payload. Integrity, issuer, origin, expiry and generation
      must be checked by the host before decoding or using this continuation. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, P.Error.t) Result.t
end

(** The admitted prefix and exact selections represented by a released page.
    Checks use only bounded accepted-state metadata, including current source
    and visibility coordinates. The host supplies fresh origin and owner
    checks and guards the physical write after this returns. *)
module Release : sig
  type t

  (** Recheck the bounded selections on the exact state already validated by
      the host. Used after source custody is released and before returning. *)
  val authorize_selections
    :  t
    -> state:Session_state.t
    -> authorize:(Session_state.t -> Text.selection -> Text.view -> (unit, 'e) Result.t)
    -> (unit, 'e) Result.t

  val check
    :  t
    -> current:(unit -> (Session_state.t, unit) Result.t)
    -> authorize:(Session_state.t -> Text.selection -> Text.view -> (unit, unit) Result.t)
    -> checkpoint:(unit -> unit)
    -> (unit, unit) Result.t
end

type error =
  | Source of Source.error
  | Refresh_required
  | Reply_limit
  | Invalid_cursor

(** This entry point only works under the host's exact Store and Handle; the
    current-state guard must be nonyielding and enforce the caller's authority.
    The signer is an actor-local pure callback. Its token is bounded by the
    encoded reply cap. [current] and the exact basis are checked before source
    access and immediately before returning any previously verified output.
    An async delivery must recheck authority again at its actual write boundary. *)
val run
  :  t
  -> Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> current:(unit -> (Session_state.t, P.Error.t) Result.t)
  -> authorize:
       (Session_state.t -> Text.selection -> Text.view -> (unit, P.Error.t) Result.t)
  -> max_frame_bytes:int
  -> max_payload_bytes:int
  -> ?checkpoint:(unit -> unit)
  -> ?continuation:Continuation.t
  -> ?on_release:(Release.t -> unit)
  -> sign:(Continuation.t -> (string, P.Error.t) Result.t)
  -> unit
  -> (Jsonaf.t, error) Result.t
