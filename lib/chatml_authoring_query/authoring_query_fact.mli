open Core
module Semantics = Chatml_semantics
module Wire = Agent_protocol.Authoring_analysis

type t

val create
  :  Semantics.Snapshot.t
  -> budget:Authoring_query_budget.t
  -> sites:Authoring_query_site.Site_projection.t
  -> max_page_items:int
  -> t

val display
  :  t
  -> Chatml_typechecker.Type_snapshot.Id.t
  -> kind:Chatml_type_display.Kind.t
  -> (Wire.Query.Display.t, Authoring_query_error.t) Result.t

val symbol
  :  t
  -> Semantics.Snapshot.Symbol.t
  -> (Wire.Query.Item.symbol, Authoring_query_error.t) Result.t

val reference
  :  t
  -> Semantics.Snapshot.Reference.t
  -> (Wire.Query.Item.reference, Authoring_query_error.t) Result.t

val binding
  :  t
  -> Semantics.Snapshot.Binding.t
  -> depth:int
  -> (Wire.Query.Item.binding, Authoring_query_error.t) Result.t

val configured_module
  :  t
  -> Semantics.Snapshot.Configured_module.t
  -> (Wire.Query.Item.configured_module, Authoring_query_error.t) Result.t
