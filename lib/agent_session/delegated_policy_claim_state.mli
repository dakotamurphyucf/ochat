open! Core

(** Private custody for ONE delegated policy claim. This module records actual
    host facts; it invokes no callback and grants no effect, source or admission.
    The Actor validates its own issuer and source before these transitions. *)
type 'borrow t

type 'borrow attempt

module Phase : sig
  type 'borrow t =
    | Waiting
    | Prepared of 'borrow attempt
    | Activated of 'borrow attempt
    | Callback_acknowledged of 'borrow attempt
    | Released
end

module Extent : sig
  type t =
    | Not_started
    | Not_entered
    | Entered
    | Joined_without_entry
    | Joined_after_entry
end

(** Nonyielding association only. Called on the actual Actor's domain before
    acquiring its server-side holder or making the claim RPC. *)
val create : issuer:unit ref -> 'borrow t

(** Domain is checked before reading mutable capture state. *)
val check_owner : 'borrow t -> issuer:unit ref -> (unit, Agent_protocol.Error.t) result

val phase : 'borrow t -> 'borrow Phase.t
val extent : 'borrow t -> Extent.t
val cancellation_requested : 'borrow t -> bool

(** Negative cancellation, not join proof. Waiting may be released immediately;
    Prepared/Activated retain their exact ownership until factual cleanup. A
    foreign-domain call is a no-op and installs no cancellation fact. *)
val cancel : 'borrow t -> unit

(** Record the real wrapper's entry and exit. Entry never grants permission;
    the Actor must first validate the installed borrow and current policy. *)
val enter : 'borrow t -> (unit, Agent_protocol.Error.t) result

(** One-shot real host wrapper extent. Installs Not_entered nonyieldingly before
    f, then finally records its actual join. Reentry rejects without joining or
    changing the owning extent. Preserves original return/error/exception. *)
val with_extent
  :  'borrow t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

module Attempt : sig
  (** [original] is the independently transferred, speculative child admission.
      Its lexical creator owns failure cleanup until [prepare] publishes it. *)
  val create
    :  previous:Session_state.t
    -> candidate:Session_transition.t
    -> receipt:Agent_protocol.Moderator_execution.t
    -> borrow:'borrow
    -> original:Execution_custody.Admission.t option
    -> 'borrow attempt

  val previous : 'borrow attempt -> Session_state.t
  val candidate : 'borrow attempt -> Session_transition.t
  val receipt : 'borrow attempt -> Agent_protocol.Moderator_execution.t
  val borrow : 'borrow attempt -> 'borrow
  val original : 'borrow attempt -> Execution_custody.Admission.t option

  (** SAME shared one-shot observation/election kernel. Its [perform]
      retains the first enclosing guard/writer/install Error or exception. The
      existing persistence callback remains the sole canonical writer. *)
  val writer : 'borrow attempt -> Session_commit_attempt.t

  (** Only for an uninstalled speculative attempt which was never published,
      or which the Actor proved [Session_commit_attempt.no_writer]. *)
  val discard : 'borrow attempt -> unit
end

(** Publish SAME complete attempt before any canonical writer. Failure transfers
    no custody; the caller must discard its unowned speculative attempt. *)
val prepare : 'borrow t -> 'borrow attempt -> (unit, Agent_protocol.Error.t) result

(** Trusted Actor-only factual installation, immediately after SAME candidate's
    positive State ACK and before publication. This is not an ACK constructor. *)
val activate : 'borrow t -> 'borrow attempt -> (unit, Agent_protocol.Error.t) result

(** The Actor must have genuinely joined the wrapper, even when cancellation
    already marked Waiting as Released. Retire only a proved no-writer child;
    unknown retains the original first outcome. *)
val release_no_writer : 'borrow t -> (unit, Agent_protocol.Error.t) result

module Callback_acknowledgement : sig
  type 'borrow capture := 'borrow t
  type 'borrow t

  (** Prevalidate SAME actual Activated capture/borrow before the terminal writer
      and before existing release cleanup. Repeated/displaced ACKs reject. All
      normal Actor/source/terminal checks remain required. No authority issued. *)
  val prepare
    :  'borrow capture
    -> borrow:'borrow
    -> ('borrow t, Agent_protocol.Error.t) result

  val check : 'borrow t -> (unit, Agent_protocol.Error.t) result

  (** Trusted nonfallible Actor assignment ONLY after SAME terminal ACK and all
      existing release cleanup succeed. Check immediately before that nonyielding
      release suffix; no callbacks/yields between check and assignment. The
      remembered ACK does not prove wrapper join and cannot release a loan. *)
  val install : 'borrow t -> unit
end

(** After the existing 599 terminal ACK retires SAME actual borrow. This clears
    only the matching Callback_acknowledged link after genuine wrapper join,
    never an Activated, replacement or unresolved Prepared claim. Admission
    retirement itself remains the existing callback owner's job. *)
val release_activated
  :  'borrow t
  -> borrow:'borrow
  -> (unit, Agent_protocol.Error.t) result
