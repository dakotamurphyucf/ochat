(** [run env ~case] runs all data-integrity checks, or one exact case name:

    - [http.audit-attribution-pagination-tamper-restart]
    - [http.blob-bounds-digest-foreign-ownership]
    - [http.export-atomic-success]
    - [http-peer.export-atomic-failures]
    - [http-peer.export-atomic-cancellation]
    - [maintenance-fixture.retention]
    - [daemon-timer.retention-active-protection]

    HTTP cases use the production daemon host; restart reopens its durable store.
    The fault peer is a real HTTP server but not the daemon. Fixture retention
    invokes Maintenance.run_once with an injected protection set. Timer retention
    exercises the production periodic service and registry-derived foreground
    protection through completion across two real 60-second cycles.
    No external provider or executable environment override is required. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
