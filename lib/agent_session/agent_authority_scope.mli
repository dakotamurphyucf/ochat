open! Core

(** Actor-private custody of the original authentication object. No codec,
    principal lookup or ambient binding exists. Creation/transfer belongs only to
    an actual admitted execution owner. This grants no source or native effect. *)
type t

(** [is_current] must be nonyielding and track the physical owner's live epoch.
    It is combined with the ORIGINAL object's currentness on every use. *)
val create : actor:Operator_authorization.t -> is_current:(unit -> bool) -> t

(** Return an attenuated proof, retaining both original authentication and owner
    lifetime checks. Keeping the returned object cannot extend either lifetime. *)
val actor : t -> (Operator_authorization.t, Agent_protocol.Error.t) result

(** Nonyielding liveness of this exact original/owner epoch. Exceptions from an
    original host guard remain exceptions; this does not authenticate or renew. *)
val is_current : t -> bool

(** Acknowledged ownership handoff only. Checks the lending owner now, then keeps
    the ORIGINAL authentication with the new owner's independent live epoch.
    The child does not retain a callback-limited wrapper. Never call this for a
    saved receipt, an unacknowledged launch or a principal-only restoration. *)
val transfer : t -> is_current:(unit -> bool) -> (t, Agent_protocol.Error.t) result

(** Exact retained original object identity, never principal equality. A narrower
    original wrapper remains the original through transfers; this operation does
    not unwrap it or cancel its expiry. It grants no currentness or authority. *)
val same_origin : t -> t -> bool
