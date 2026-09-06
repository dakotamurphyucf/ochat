(** [run env ~case] runs stock-daemon compaction against a gated local JSON
    Responses endpoint. Select [http.atomic-replacement-restart],
    [http.cancel-blocked-summary], or [http.failed-summary-no-replacement].

    Check exact precommit history, busy and stale mutation conflicts, a single
    atomic replacement with retained IDs, full canonical history after restart,
    and explicit operation terminal events. Release cancelled provider requests,
    check an exact unnormalized terminal snapshot and durable history on restart.
    Provider return is not proof of actor delivery: the separate Session_actor
    mailbox regression acknowledges consumption of a late successful result and
    requires exact unchanged state and durable events.
    Each fixture uses a private temporary cwd for both daemon launches, keeping
    provider-response and summarizer-error diagnostics outside the repository. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
