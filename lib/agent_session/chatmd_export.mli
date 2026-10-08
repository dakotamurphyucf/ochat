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
