open! Core

(** Explicit restart-bound administrative planning policy. Configuration alone
    grants no target visibility, child creation, native effect or Run admission.
    Every resource-opening entry repeats validation before IO. *)
module Zone_directory : sig
  type t = private
    { directory : string
    ; source : string
    ; from : Time_ns.Alternate_sexp.t
    ; until : Time_ns.Alternate_sexp.t
    }
  [@@deriving compare, equal, sexp]

  (** Absolute canonical spelling: at most 4096 bytes/64 components, no NUL,
      dot/parent/empty interior components. Actual componentwise no-follow open
      belongs to the host. Inclusive finite coverage is complete or unavailable. *)
  val create
    :  directory:string
    -> source:string
    -> from:Time_ns.Alternate_sexp.t
    -> until:Time_ns.Alternate_sexp.t
    -> (t, Agent_protocol.Error.t) result
end

type t = private
  { principal_id : Agent_protocol.Id.Principal.t
  ; controllers : Agent_protocol.Id.Session.t list
  ; expires_at : Agent_protocol.Timestamp.t option
  ; zone_directory : Zone_directory.t option
  }
[@@deriving compare, equal, sexp]

(** At most 256 distinct typed controller IDs and 64 KiB aggregate metadata.
    Expiry is the original absolute expiry, never refreshed by recovery/retry.
    A configured stable principal is an audit selector, not a restored proof.
    Derived S-expression decoders independently enforce bounded traversal and
    these same constructor invariants. *)
val create
  :  principal_id:Agent_protocol.Id.Principal.t
  -> controllers:Agent_protocol.Id.Session.t list
  -> expires_at:Agent_protocol.Timestamp.t option
  -> zone_directory:Zone_directory.t option
  -> (t, Agent_protocol.Error.t) result

val validate : t -> (unit, Agent_protocol.Error.t) result
