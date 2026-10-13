(** Transport-neutral authoring client over an already initialized OChat
    connection. Every operation checks the selected feature, actual advertised
    method, result variant and echoed host/context/source affinity before
    returning a value. The client tracks the latest context generation it
    observed through this connection, so an older read cannot publish after
    a newer local mutation. A selected feature or an opaque ID is never a
    substitute for server authorization. *)

module P = Agent_protocol.Authoring

type t

(** [create connection] reads the successful initialization selected for this
    connection and [authoring.capabilities]. It does not initialize, open a
    context or replay an earlier request. The returned client becomes stale
    when that connection closes or initializes again, even if the server and
    principal IDs are unchanged. Create a new client to negotiate fresh method
    capabilities after reinitialization. *)
val create : Connection.t -> (t, Agent_protocol.Error.t) result

(** Snapshot from [create]. It may be stale after a later initialize; calls
    reject that client and require a new [create]. This is not authorization. *)
val capabilities : t -> P.Capabilities.t

(** False for an unknown, closed, refreshed or superseded context. A UI checks
    this before applying a previously returned result after another edit. *)
val is_current_context : t -> P.Context.Expected.t -> bool

val roots : t -> (P.Source_read.Roots_result.t, Agent_protocol.Error.t) result

val list_host_members
  :  t
  -> P.Source_read.Host_list_request.t
  -> (P.Source_read.Host_list_result.t, Agent_protocol.Error.t) result

val read_host_member
  :  t
  -> P.Source_read.Host_read_request.t
  -> (P.Source_read.Host_read_result.t, Agent_protocol.Error.t) result

val open_context
  :  t
  -> P.Context.Open_request.t
  -> (P.Context.t, Agent_protocol.Error.t) result

val refresh_context
  :  t
  -> P.Context.Refresh_request.t
  -> (P.Context.t, Agent_protocol.Error.t) result

val close_context
  :  t
  -> P.Context.Close_request.t
  -> (P.Context.Close_result.t, Agent_protocol.Error.t) result

val list_sources
  :  t
  -> P.Source_read.List_request.t
  -> (P.Source_read.List_result.t, Agent_protocol.Error.t) result

val read_source
  :  t
  -> P.Source_read.Read_request.t
  -> (P.Source_read.Read_result.t, Agent_protocol.Error.t) result

val document_version
  :  t
  -> P.Document.Version_request.t
  -> (P.Document.Version.t, Agent_protocol.Error.t) result

val replace_document
  :  t
  -> P.Document.Replace_request.t
  -> (P.Document.Result.t, Agent_protocol.Error.t) result

val close_document
  :  t
  -> P.Document.Close_request.t
  -> (P.Document.Result.t, Agent_protocol.Error.t) result

(** Each semantic call requires selected [Features.semantic], the installed
    method, and the same live context generation before and after transport.
    Analyze's [Unavailable] is a typed successful result, not a failed check. *)
val analyze
  :  t
  -> P.Analysis.Analyze.Request.t
  -> (P.Analysis.Analyze.t, Agent_protocol.Error.t) result

val analyze_prefix
  :  t
  -> P.Analysis.Prefix.Request.t
  -> (P.Analysis.Prefix.t, Agent_protocol.Error.t) result

val query_semantics
  :  t
  -> P.Analysis.Query.Request.t
  -> (P.Analysis.Query.t, Agent_protocol.Error.t) result

(** Reconnect creates a new authoring context. A caller must reauthorize its
    origin and resend a complete overlay set explicitly against fresh CAS
    values. No context ID or document version survives it. *)
