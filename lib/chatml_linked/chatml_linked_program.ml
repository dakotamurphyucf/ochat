open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Graph = Chatml_dependency_graph
module Checked_graph = Chatml_checked_graph
module Check = Chatml_configured_check
module Parse = Chatml.Chatml_parse
module Lang = Chatml.Chatml_lang
module Visibility = Chatml.Chatml_module_visibility
module Contract = Chatml.Chatml_compiler_contract

module Stage = struct
  type t =
    | Parse
    | Typecheck
    | Link
  [@@deriving equal, sexp_of]
end

module Failure = struct
  type t =
    { stage : Stage.t
    ; diagnostic : Chatml_diagnostic.t
    }

  let stage t = t.stage
  let diagnostic t = t.diagnostic
end

module Unit = struct
  type t =
    { id : Prepared.Module_id.t
    ; implementation : Captured.t
    ; interface : Captured.t option
    ; program : Lang.resolved_program
    ; visibility : Visibility.t
    }

  let id t = t.id
  let implementation t = t.implementation
  let interface t = t.interface
  let program t = t.program
  let visibility t = t.visibility
end

module Entrypoint = Check.Entrypoint

type t =
  { prepared : Prepared.t
  ; contract : Contract.t
  ; graph : Graph.t
  ; units : Unit.t Map.M(Prepared.Module_id).t
  ; runtime_order : Prepared.Module_id.t list
  ; entrypoints : Entrypoint.t list
  }

let prepared t = t.prepared
let static_contract t = t.contract
let graph t = t.graph
let find_unit t id = Map.find t.units id
let entry t = Map.find_exn t.units (Graph.entry t.graph)
let units t = List.map (Graph.dependency_order t.graph) ~f:(Map.find_exn t.units)
let runtime_order t = t.runtime_order
let entrypoints t = t.entrypoints

let compile prepared ~contract ~max_dependency_depth ~checkpoint =
  let failure stage diagnostic = Error { Failure.stage; diagnostic } in
  let open Result.Let_syntax in
  let%bind checked =
    Checked_graph.check prepared ~contract ~max_dependency_depth ~checkpoint
    |> Result.map_error ~f:(fun error ->
      let stage =
        match Checked_graph.Failure.kind error with
        | Compiler Parse -> Stage.Parse
        | Compiler Typecheck -> Typecheck
        | Sites _ | Recording _ -> Typecheck
      in
      { Failure.stage; diagnostic = Checked_graph.Failure.diagnostic error })
  in
  let graph = Checked_graph.graph checked in
  let%bind units =
    List.fold_result
      (Graph.dependency_order graph)
      ~init:(Map.empty (module Prepared.Module_id))
      ~f:(fun units id ->
        checkpoint ();
        match Checked_graph.checked checked id with
        | None ->
          failure
            Link
            (Chatml_diagnostic.create
               "Completed dependency graph is missing its checked unit")
        | Some unit ->
          let%map program =
            Chatml_resolver.resolve_configured_program
              (Check.Checked_unit.annotations (Checked_graph.Unit.checked unit))
              (Checked_graph.Unit.parsed unit)
              ~checkpoint
            |> Result.map_error ~f:(fun diagnostic ->
              { Failure.stage = Link; diagnostic })
          in
          let unit =
            { Unit.id
            ; implementation = Parse.Unit.source (Checked_graph.Unit.parsed unit)
            ; interface =
                Option.map (Checked_graph.Unit.interface unit) ~f:Parse.Interface.source
            ; program
            ; visibility = Check.Checked_unit.visibility (Checked_graph.Unit.checked unit)
            }
          in
          Map.set units ~key:id ~data:unit)
  in
  let runtime_order = Graph.runtime_dependency_order graph ~checkpoint in
  let entrypoints = Checked_graph.Unit.entrypoints (Checked_graph.entry checked) in
  checkpoint ();
  Ok { prepared; contract; graph; units; runtime_order; entrypoints }
;;
