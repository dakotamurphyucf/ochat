open Core
module Captured = Chatml_source
module Semantics = Chatml_semantics
module Wire = Agent_protocol.Authoring_analysis
module Limits = Authoring_query_budget.Limits
module Error = Authoring_query_error
module Evidence = Authoring_query_evidence.Evidence
module Site_projection = Authoring_query_site.Site_projection

module Prefix_origin = struct
  type t = Authoring_query_prefix_origin.t

  let map_error = function
    | Authoring_query_prefix_origin.Evidence_mismatch -> Error.Evidence_mismatch
    | Stale_source -> Stale_source
  ;;

  let create provisional ~full_source ~checkpoint =
    Authoring_query_prefix_origin.create provisional ~full_source ~checkpoint
    |> Result.map_error ~f:map_error
  ;;

  let full_location t location ~checkpoint =
    Authoring_query_prefix_origin.full_location t location ~checkpoint
    |> Result.map_error ~f:map_error
  ;;
end

let project = Authoring_query_dispatch.project
let syntax_at = Authoring_query_dispatch.syntax_at
