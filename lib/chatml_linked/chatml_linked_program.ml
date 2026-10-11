open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Graph = Chatml_dependency_graph
module Session = Chatml_dependency_session
module Lookup = Chatml_configured_lookup
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

(* Private temporary compilation data; discarded after settled lowering. No
   engine, raw exports, parser/checker callback survives the t constructor. *)
module Checked = struct
  type t =
    { parsed : Parse.Unit.t
    ; interface : Parse.Interface.t option
    ; checked : Check.Checked_unit.t
    ; entrypoints : Entrypoint.t list
    }
end

let compile prepared ~contract ~max_dependency_depth ~checkpoint =
  let failure stage diagnostic = Error { Failure.stage; diagnostic } in
  let actual = Contract.fingerprint contract ~checkpoint in
  if not (Captured.Digest.equal actual (Prepared.compiler_contract prepared))
  then
    failure
      Typecheck
      (Chatml_diagnostic.create
         "Prepared compiler contract does not match the actual selected static host \
          contract")
  else (
    let engine = Check.create_static (Contract.static_surface contract) ~checkpoint in
    (* Session's expected diagnostic family remains shared with the checker.
       Remember only the first stage-bearing failure, never a mutable source owner. *)
    let cause = ref None in
    let stage_result stage result =
      match result with
      | Ok value -> Ok value
      | Error diagnostic ->
        if Option.is_none !cause then cause := Some { Failure.stage; diagnostic };
        Error diagnostic
    in
    let entry_id = Prepared.Module_id.of_implementation (Prepared.entry prepared) in
    let checked =
      Session.run prepared ~max_dependency_depth ~checkpoint ~check:(fun node ~require ->
        let open Result.Let_syntax in
        let source = Session.Unit.implementation node in
        let%bind parsed =
          Parse.parse_captured (Lookup.source source) ~checkpoint |> stage_result Parse
        in
        let%bind interface =
          match Session.Unit.interface node with
          | None -> Ok None
          | Some source ->
            Parse.parse_captured_interface (Lookup.source source) ~checkpoint
            |> stage_result Parse
            |> Result.map ~f:Option.some
        in
        let require ~source reference =
          require ~source reference
          |> Result.map ~f:(fun (unit : Checked.t) ->
            Check.Checked_unit.exports unit.checked)
        in
        let%bind checked =
          Check.check_node engine node ~require ~implementation:parsed ~interface
          |> stage_result Typecheck
        in
        let%bind entrypoints =
          if Prepared.Module_id.equal entry_id (Session.Unit.id node)
          then
            Check.check_entrypoints
              engine
              checked
              ~source
              ~require
              ~required_bindings:(Contract.required_bindings contract)
            |> stage_result Typecheck
          else Ok []
        in
        checkpoint ();
        Ok { Checked.parsed; interface; checked; entrypoints })
    in
    match checked with
    | Error diagnostic ->
      (match !cause with
       | Some failure -> Error failure
       | None -> failure Typecheck diagnostic)
    | Ok checked ->
      let open Result.Let_syntax in
      let graph = Session.graph checked in
      let%bind units =
        List.fold_result
          (Graph.dependency_order graph)
          ~init:(Map.empty (module Prepared.Module_id))
          ~f:(fun units id ->
            checkpoint ();
            match Session.checked checked id with
            | None ->
              failure
                Link
                (Chatml_diagnostic.create
                   "Completed dependency graph is missing its checked unit")
            | Some unit ->
              let%map program =
                Chatml_resolver.resolve_configured_program
                  (Check.Checked_unit.annotations unit.checked)
                  unit.parsed
                  ~checkpoint
                |> Result.map_error ~f:(fun diagnostic ->
                  { Failure.stage = Link; diagnostic })
              in
              let unit =
                { Unit.id
                ; implementation = Parse.Unit.source unit.parsed
                ; interface = Option.map unit.interface ~f:Parse.Interface.source
                ; program
                ; visibility = Check.Checked_unit.visibility unit.checked
                }
              in
              Map.set units ~key:id ~data:unit)
      in
      let runtime_order = Graph.runtime_dependency_order graph ~checkpoint in
      let entrypoints = (Session.entry checked).entrypoints in
      checkpoint ();
      Ok { prepared; contract; graph; units; runtime_order; entrypoints })
;;
