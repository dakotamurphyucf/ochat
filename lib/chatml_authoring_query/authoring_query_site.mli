open Core
module Captured = Chatml_source
module Wire = Agent_protocol.Authoring_analysis

module Site_projection : sig
  type t =
    Captured.Location.t
    -> checkpoint:(unit -> unit)
    -> (Wire.Site.t, Authoring_query_error.t) Result.t
end

val project
  :  Captured.t list
  -> Captured.Location.t
  -> projection:Site_projection.t
  -> budget:Authoring_query_budget.t
  -> (Wire.Site.t, Authoring_query_error.t) Result.t
