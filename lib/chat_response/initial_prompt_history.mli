open Core

(** Initial-history admission shared by generated and authored delegation.
    Only literal messages are accepted. Dynamic document/image loading, embedded
    agents, provider results and stored conversation identities require their own
    admitted conversion and are rejected rather than executed or silently omitted.
    Other prompt declarations remain available for separate runtime preparation. *)
val plain_message : Prompt.Chat_markdown.msg -> bool

(** Validate every message before allocating fresh child-scoped history IDs.
    Preserves message order, roles and literal text parts. Does not read files,
    initialize scripts, invoke tools/models, or install a session. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> Prompt.Chat_markdown.top_level_elements list
  -> (History_entry.t list * int, Agent_protocol.Error.t) result
