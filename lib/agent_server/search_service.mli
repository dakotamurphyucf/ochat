(** Nonactivating conversation search. Injected readers own filesystem/mailbox
    capabilities and reauthorize current session visibility. This service never
    repairs storage, installs an actor or starts inference. Cancellation is not
    caught. Cache and signed cursors belong to the host, not the client. *)
type t

module Catalog : sig
  type t =
    { organization_revision : int64
    ; sessions : Agent_protocol.Session_catalog.t list
    }
end

val create
  :  server_id:Agent_protocol.Id.Server.t
  -> principal:Agent_protocol.Principal.t
  -> cache:Search_cache.t
  -> cursors:Search_cursor.t
  -> read_catalog:
       (Agent_protocol.Session.List_request.t
        -> (Catalog.t, Agent_protocol.Error.t) result)
  -> read_state:
       (Agent_protocol.Id.Session.t
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> t

(** At most 64 source sessions and requested scan_limit entries per page.
    Raw payload examination admits 6 MiB plus one boundary probe of at most 2 MiB.
    Up to 100 hit revalidation reads follow, each bounded
    to its original source entry. Recheck catalog filters and exact current
    source identity/revision before regenerating every disclosed snippet.
    Source changes return Conflict/refresh_required without unbounded retry.
    Zero-hit bounded progress returns a continuation, never false completion. *)
val query
  :  t
  -> Agent_protocol.Search_query.t
  -> (Agent_protocol.Search_page.t, Agent_protocol.Error.t) result

(** Reauthorize before reading; distinguish edited, unavailable, and denied hits.
    Current results contain at most five source positions of selected plain text.
    Inspect at most 10 MiB of raw payload, then revalidate the source revision and
    catalog basis before disclosure. Never open retained edit/reset archives. *)
val navigate
  :  t
  -> Agent_protocol.Search_navigation.Request.t
  -> (Agent_protocol.Search_navigation.Response.t, Agent_protocol.Error.t) result
