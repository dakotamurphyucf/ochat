open Core

val project
  :  Authoring_query_evidence.Evidence.t
  -> request:Agent_protocol.Authoring_analysis.Query.Request.t
  -> sites:Authoring_query_site.Site_projection.t
  -> limits:Authoring_query_budget.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Agent_protocol.Authoring_analysis.Query.payload, Authoring_query_error.t) Result.t

val syntax_at
  :  Chatml_semantics.Partial.t
  -> request:Agent_protocol.Authoring_analysis.Query.Request.t
  -> sites:Authoring_query_site.Site_projection.t
  -> limits:Authoring_query_budget.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Agent_protocol.Authoring_analysis.Query.payload, Authoring_query_error.t) Result.t
