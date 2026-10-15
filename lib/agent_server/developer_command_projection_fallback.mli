open! Core
module Receipt = Agent_store.Developer_command_receipt

(** Fixed bounded truth when a completed portable result's detailed diagnostic,
    logs or output exceeds its admitted whole-receipt limit. The host verifies
    these exact projections fit the configured physical Store terminal cap
    before admitting any command. No callback or Store write occurs here. *)

(** A Check that genuinely rejected compilation remains Rejected when its
    detailed diagnostic is too large to retain. *)
val check_rejected : Receipt.value

(** Keep the actual completed fixture identity and matched-step count while
    reporting that its detailed result exceeded the receipt limit. *)
val fixture_output_limit
  :  graph_fingerprint:Chatml_source.Digest.t
  -> fixture_digest:Chatml_source.Digest.t
  -> matched_steps:int
  -> Receipt.value

(** Worst encoded size of [fixture_output_limit] for a nonnegative OCaml int;
    digests have fixed-width encodings. This is configuration proof only, not a
    fabricated fixture receipt. *)
val fixture_max_size_probe : Receipt.value
