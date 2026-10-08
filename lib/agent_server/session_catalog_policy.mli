(** Pure query policy; visibility is applied by the server before this module.
    Querying never loads a runtime or grants access to sessions. *)
val matches
  :  Agent_protocol.Session.List_request.t
  -> Agent_protocol.Session_catalog.t
  -> bool

val compare
  :  Agent_protocol.Session_catalog_query.Sort.t
  -> Agent_protocol.Session_catalog.t
  -> Agent_protocol.Session_catalog.t
  -> int

val active_owner
  :  now:Agent_protocol.Timestamp.t
  -> Agent_protocol.Session.Attachment.t list
  -> Agent_protocol.Id.Principal.t option
