open! Core

(** Host-only proof of the original authenticated actor. No wire codec, token,
    token digest, or reconstruction from client attributes/principal lookup. *)
type t

val principal : t -> Agent_protocol.Principal.t

(** Non-yielding currentness check. Unexpected host guard exceptions propagate;
    callers must not replace them with authorization success. *)
val is_current : t -> bool

(** Explicit local trusted contexts; never a remote authentication default. *)
val trusted_local : Agent_protocol.Principal.t -> t

(** Exact immutable static record with no expiry; separate explicit constructor
    prevents mistaking missing remote proof for a nonexpiring authentication. *)
val nonexpiring_static : Agent_protocol.Principal.t -> t

(** Captures the exact record's expiry and host wall-clock callback. The callback
    must not yield. Construction checks the original expiry; later checks preserve
    that lifetime even if another credential has the same principal/scopes. *)
val bounded
  :  principal:Agent_protocol.Principal.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> expires_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

(** Trusted custom bearer/reverse-proxy policy. Required currentness includes
    original authentication lifetime, not scope equality or a principal rematch. *)
val guarded : principal:Agent_protocol.Principal.t -> is_current:(unit -> bool) -> t
