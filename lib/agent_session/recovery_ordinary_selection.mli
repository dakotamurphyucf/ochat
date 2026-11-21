open! Core

(** Ordinary history excludes derived summaries. The caller independently enforces message role and part visibility. *)
val visible : Agent_protocol.Recovery_window.Citation.t -> Session_state.t -> bool

(** Bind a previously read ordinary member to its source and ordinal. Unrelated
    appends are allowed. A generation change, expiry, withdrawal or changed
    source rejects. The caller must hold the same registered owner, separately
    reauthorize the reader and recheck at the physical delivery boundary. *)
val check
  :  Session_state.t
  -> Agent_protocol.Recovery_window.Citation.t
  -> (Session_state.t -> bool, Recovery_source_reader.error) result
