open! Core

(** Neutral ChatMD presentation. Canonical rendering preserves host identity and
    complete known semantic content; opaque items include inspectable neutral
    payload JSON. No provider DTO decoding occurs. *)
val render : History_entry.t list -> string

(** Private canonical entries with explicit provenance annotations. An annotation
    remains presentation metadata and is not authority when imported. *)
val render_protocol
  :  Agent_protocol.History.entry list
  -> (string, Agent_protocol.Error.t) result

(** Public read views retain their disclosure. Visible/Redacted export uses
    explicit presentation markup, never fake canonical messages or placeholders.
    The caller selects and projects entries under the current principal first. *)
val render_public : Agent_protocol.Public.History.t list -> string

(** Same disclosure-aware templates and exact legacy bytes, under complete
    output/work/JSON admission. Full canonical export requires callers to admit
    only Full bodies separately; bounded rendering does not upgrade disclosure.
    No input-sized visible body/header/provenance string is assembled before its
    admission. Original checkpoint failures/cancellation propagate; this pure
    renderer relies on the supplied checkpoint to check cancellation. *)
val render_public_bounded
  :  Agent_protocol.Public.History.t list
  -> limits:History_chatmd.Rendering.Limits.t
  -> checkpoint:(unit -> unit)
  -> (string, History_chatmd.Rendering.Error.t) Result.t
