(** Shared direct/socket/HTTP/stdio search client. Host owns canonical history,
    cache, cursor signing and current authorization. The client retains no
    transcript copy and must treat source-change conflicts as an explicit refresh. *)
type t

val create : Connection.t -> server_id:Agent_protocol.Id.Server.t -> t

(** Host identity is checked against both the request and completed handshake.
    Follow next_cursor even for zero hits; only reached_end means completion. *)
val page
  :  t
  -> Agent_protocol.Search_query.t
  -> (Agent_protocol.Search_page.t, Agent_protocol.Error.t) result

(** No activation and no fallback to retained evidence. A changed entry requires
    an explicit refresh; permission failures remain ordinary protocol errors. *)
val navigate
  :  t
  -> Agent_protocol.Search_navigation.Request.t
  -> (Agent_protocol.Search_navigation.Response.t, Agent_protocol.Error.t) result
