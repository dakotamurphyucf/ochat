open Core

module Error : sig
  type t =
    | Invalid of Recurrence_rules.Timing_error.t
    | Filesystem of Chatml_local_source.Error.t
    | Data of Recurrence_zone_data.Error.t
  [@@deriving sexp_of]
end

(** Load one canonical regular timezone file beneath an already-open, explicitly
    admitted, closeable directory capability with an empty Eio path. The caller
    keeps its root/switch alive throughout this call. No ambient filesystem,
    working directory, TZ variable, installed database or UTC fallback is used.

    Validate name, source label and ordered inclusive coverage before any IO or
    checkpoint. Borrow the original capability and acquire only an independent
    joined duplicate through Chatml_local_source.with_root. Its componentwise
    no-follow reader rejects all symlink aliases, including internal aliases;
    callers must deploy canonical regular files. Nonempty root paths and
    unsupported providers reject through the original finite filesystem error.

    Read once using Recurrence_zone_data.maximum_bytes. Exact bytes provide the
    decoder's SHA256; source is a bounded host label, not proof of an IANA release.
    The existing reader observes file replacement/revision changes without
    claiming an atomic filesystem snapshot. All newly owned native resources
    close before pure decoding; the returned immutable Zone retains no descriptor,
    callback or raw bytes. Coverage is complete or an explicit Data error.

    Preserve original reader operation/code errors. Cancellation and checkpoint
    exceptions propagate after joined cleanup, with no partial Zone result.
    Check cancellation before and after each adapter checkpoint, including the
    final checkpoint before publication. No unexpected exception is converted. *)
val load
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> source:string
  -> name:string
  -> from:Time_ns.Alternate_sexp.t
  -> until:Time_ns.Alternate_sexp.t
  -> checkpoint:(unit -> unit)
  -> (Recurrence_rules.Zone.t, Error.t) Result.t

(** Borrow an existing admitted Local directory owner for one joined bounded
    read. The caller keeps it live through this call and owns its release. Name,
    source and coverage validation, exact byte cap/hash, finite decoding and final
    cancellation checks match [load]. No new retained root is acquired; every
    per-read native resource closes before pure decoding. Useful when the host
    itself acquired its configured directory through Local.open_subroot's actual
    componentwise no-follow traversal. No Eio capability is reconstructed. *)
val load_local
  :  Chatml_local_source.t
  -> source:string
  -> name:string
  -> from:Time_ns.Alternate_sexp.t
  -> until:Time_ns.Alternate_sexp.t
  -> checkpoint:(unit -> unit)
  -> (Recurrence_rules.Zone.t, Error.t) Result.t
