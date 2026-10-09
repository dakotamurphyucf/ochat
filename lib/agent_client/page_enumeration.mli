(** Complete bounded enumeration for the shared cursor-page contract. The
    supplied page reader owns transport/authorization; this helper neither
    consumes notifications nor retries failed or conflicted requests. *)
val collect
  :  Agent_protocol.Page.Request.t
  -> max_items:int
  -> max_pages:int
  -> read:
       (Agent_protocol.Page.Request.t
        -> ('a Agent_protocol.Page.t, Agent_protocol.Error.t) result)
  -> ('a list, Agent_protocol.Error.t) result
(** Requires a fresh query and positive bounds. Empty pages with a continuation
    remain partial; exhaustion, a repeated cursor or any read failure returns an
    error instead of a partial list. Only the cursor changes between reads. *)
