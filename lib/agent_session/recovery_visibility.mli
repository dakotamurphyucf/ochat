open! Core
module P = Agent_protocol

(** Recovery selection against one admitted source-state snapshot. The caller
    owns session authorization. Build a fresh view at the actual disclosure
    boundary after any yielding operation. *)
type t

type selection =
  | Unavailable
  | Withdrawn
  | Expired
  | Eligible of Recovery_window_ledger.Member.t

(** Require the runtime host's source session identity. A malformed moderator
    checkpoint prevents selection, including reads of archived members. *)
val create
  :  ?work:Recovery_source_work.t
  -> Session_state.t
  -> owner:P.Session_ref.t
  -> (t, P.Error.t) result

(** Consult exact membership, the active source generation, a committed
    withdrawal, and the source's current moderator tombstones. Selection does
    not open an archive, access a blob, or authorize model delivery. *)
val select : t -> P.Recovery_window.Citation.t -> selection

(** A retained preparation establishes effective membership. Check that member's
    present window and expiry, exact canonical base when known, and moderator suppression
    before following either a live or archived source. *)
val check_effective
  :  t
  -> P.Recovery_window.Citation.t
  -> (unit, [ `Unavailable | `Withdrawn | `Expired ]) result

(** Custody retirement is irreversible only under a newer source generation
    or the exact member's committed window expiry. Policy overlays and missing
    membership never authorize physical reclamation. *)
val irrevocably_retired : t -> P.Recovery_window.Citation.t -> bool
