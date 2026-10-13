(** Closed, portable dispatch for the authoring core method family. The sum
    constructors keep each request and result tied to its method-specific DTO;
    accepted syntax alone does not advertise an installed server handler. *)

module Request : sig
  type t =
    | Capabilities
    | Roots
    | Host_list of Authoring_context.Source_read.Host_list_request.t
    | Host_read of Authoring_context.Source_read.Host_read_request.t
    | Context_open of Authoring_context.Context.Open_request.t
    | Context_refresh of Authoring_context.Context.Refresh_request.t
    | Context_close of Authoring_context.Context.Close_request.t
    | Source_list of Authoring_context.Source_read.List_request.t
    | Source_read of Authoring_context.Source_read.Read_request.t
    | Document_version of Authoring_context.Document.Version_request.t
    | Document_replace of Authoring_context.Document.Replace_request.t
    | Document_close of Authoring_context.Document.Close_request.t
    | Bundle_begin of Authoring_bundle.Bundle.Begin.request
    | Bundle_chunk of Authoring_bundle.Bundle.Chunk.request
    | Bundle_commit of Authoring_bundle.Bundle.Commit.request
    | Bundle_status of Authoring_bundle.Bundle.Status.request
    | Analyze of Authoring_analysis.Analyze.Request.t
    | Prefix of Authoring_analysis.Prefix.Request.t
    | Query of Authoring_analysis.Query.Request.t
  [@@deriving sexp_of]

  val method_name : t -> string
  val params : t -> Jsonaf.t
  val of_method_and_params : method_:string -> params:Jsonaf.t -> (t, Error.t) result
  val supported_methods : string list
end

module Response : sig
  type t =
    | Capabilities of Authoring_common.Capabilities.t
    | Roots of Authoring_context.Source_read.Roots_result.t
    | Host_list of Authoring_context.Source_read.Host_list_result.t
    | Host_read of Authoring_context.Source_read.Host_read_result.t
    | Context_open of Authoring_context.Context.t
    | Context_refresh of Authoring_context.Context.t
    | Context_close of Authoring_context.Context.Close_result.t
    | Source_list of Authoring_context.Source_read.List_result.t
    | Source_read of Authoring_context.Source_read.Read_result.t
    | Document_version of Authoring_context.Document.Version.t
    | Document_replace of Authoring_context.Document.Result.t
    | Document_close of Authoring_context.Document.Result.t
    | Bundle_begin of Authoring_bundle.Bundle.Begin.response
    | Bundle_chunk of Authoring_bundle.Bundle.Chunk.response
    | Bundle_commit of Authoring_bundle.Bundle.Commit.response
    | Bundle_status of Authoring_bundle.Bundle.Status.t
    | Analyze of Authoring_analysis.Analyze.t
    | Prefix of Authoring_analysis.Prefix.t
    | Query of Authoring_analysis.Query.t
  [@@deriving sexp_of]

  val method_name : t -> string
  val to_json : t -> Jsonaf.t
  val of_method_and_json : method_:string -> Jsonaf.t -> (t, Error.t) result
  val supported_methods : string list
end
