open! Core

(** Private immutable conversion reuse, never an admission or replay proof. *)
module Budget : sig
  type t

  (** Bounds entry count and original record bytes, not decoded OCaml heap.
      Negative values reject; zero disables restoration reuse. *)
  val create : max_entries:int -> max_original_bytes:int -> (t, Store_error.t) Result.t

  (** 128 entries and 16 MiB of aggregate original record bytes. *)
  val default : t
end

type 'state t

val empty : budget:Budget.t -> 'state t

(** Match the same route and exact original flags, digest and bytes. The caller
    has freshly checked all original metadata, chains and anchors first. *)
val find_snapshot
  :  'state t
  -> filename:string
  -> covered:int64
  -> original:Document_record.t
  -> (Snapshot.installed * 'state) option

val find_transaction : 'state t -> original:Document_record.t -> Transaction.t option

(** Replace only after a completely successful retention check. Select currently
    observed snapshots in order, then newest transactions, within the captured
    count and original-byte allowances. An oversized entry is simply omitted.
    Snapshot tuple filenames must match their installed filename; transactions
    must be the freshly verified ascending chain. No state or cache is mutated,
    no IO occurs and no failure is introduced. *)
val replace
  :  'state t
  -> snapshots:(string * Snapshot.installed * 'state) list
  -> transactions:Transaction.t list
  -> 'state t
