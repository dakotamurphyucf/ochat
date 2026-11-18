open! Core

(** Restart-bound administrative execution policy, separate from planning.
    IDs are local selectors; this DATA issues no proof or runtime permission. *)
module Controller : sig
  type t = private
    { session_id : Agent_protocol.Id.Session.t
    ; existing_targets : Agent_protocol.Id.Session.t list
    ; captured_children : bool
    }
  [@@deriving compare, equal, sexp]

  (** Exact target IDs, including self only when explicitly listed. No wildcard
      or remote Session_ref. [captured_children] is a separate explicit grant;
      it does not imply any existing-target grant. *)
  val create
    :  session_id:Agent_protocol.Id.Session.t
    -> existing_targets:Agent_protocol.Id.Session.t list
    -> captured_children:bool
    -> (t, Agent_protocol.Error.t) result
end

type t = private
  { principal_id : Agent_protocol.Id.Principal.t
  ; controllers : Controller.t list
  ; expires_at : Agent_protocol.Timestamp.t option
  }
[@@deriving compare, equal, sexp]

(** At most 256 distinct controllers, 256 targets per controller, 1024 total
    controller-target edges and 64 KiB complete S-expression metadata. Typed IDs,
    duplicate controllers/edges, empty grant rows and complete bounds validate
    before allocation of derived maps/proofs. A grant row must contain at least
    one existing target or explicit captured permission. Targets may be shared
    by controllers; no execution serialization is implied by this DATA.

    Expiry is the original absolute expiry. None is explicit nonexpiring policy
    limited by actual host lifetime, never a recovered credential. Decoders run
    the same bounded syntax/constructor validation, not generated restoration. *)
val create
  :  principal_id:Agent_protocol.Id.Principal.t
  -> controllers:Controller.t list
  -> expires_at:Agent_protocol.Timestamp.t option
  -> (t, Agent_protocol.Error.t) result

val validate : t -> (unit, Agent_protocol.Error.t) result
