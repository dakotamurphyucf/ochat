open Core
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Graph = Chatml_dependency_graph
module Session = Chatml_dependency_session
module Check = Chatml_configured_check
module Parse = Chatml.Chatml_parse
module Recorder = Chatml_typechecker.Semantic
module Captured = Chatml_source
module Lookup = Chatml_configured_lookup

module Stage = struct
  type t =
    | Parse
    | Typecheck
  [@@deriving equal, sexp_of]
end

module Failure = struct
  type kind =
    | Compiler of Stage.t
    | Sites of Chatml.Chatml_syntax_sites.Error.t
    | Recording of Recorder.Error.t

  type t =
    { kind : kind
    ; diagnostic : Chatml_diagnostic.t
    }

  let kind t = t.kind
  let diagnostic t = t.diagnostic
end

module Unit = struct
  type t =
    { parsed : Parse.Unit.t
    ; interface : Parse.Interface.t option
    ; checked : Check.Checked_unit.t
    ; entrypoints : Check.Entrypoint.t list
    }

  let parsed t = t.parsed
  let interface t = t.interface
  let checked t = t.checked
  let entrypoints t = t.entrypoints
end

type t =
  { units : Unit.t Session.t
  ; semantic_facts : Recorder.Settled.t option
  }

let graph t = Session.graph t.units
let entry t = Session.entry t.units
let checked t id = Session.checked t.units id
let semantic_facts t = t.semantic_facts

let check_invocation ?recorder prepared ~contract ~max_dependency_depth ~checkpoint =
  let failure stage diagnostic = Error { Failure.kind = Compiler stage; diagnostic } in
  let actual = Contract.fingerprint contract ~checkpoint in
  if not (Captured.Digest.equal actual (Prepared.compiler_contract prepared))
  then (
    Option.iter recorder ~f:Recorder.discard;
    failure
      Typecheck
      (Chatml_diagnostic.create
         "Prepared compiler contract does not match the actual selected static host \
          contract"))
  else (
    let engine =
      Check.create_static ?recorder (Contract.static_surface contract) ~checkpoint
    in
    let cause = ref None in
    let remember kind diagnostic =
      if Option.is_none !cause then cause := Some { Failure.kind; diagnostic };
      Error diagnostic
    in
    let stage_result stage = function
      | Ok value -> Ok value
      | Error diagnostic -> remember (Compiler stage) diagnostic
    in
    let site_result = function
      | Ok value -> Ok value
      | Error (Parse.Syntax diagnostic) -> remember (Compiler Parse) diagnostic
      | Error (Sites error) ->
        remember
          (Sites error)
          (Chatml_diagnostic.create "Parser identifier metadata admission rejected")
    in
    let parse source =
      match recorder with
      | None -> Parse.parse_captured source ~checkpoint |> stage_result Parse
      | Some recorder ->
        let limits = Recorder.site_limits recorder ~checkpoint |> Recorder.take_exn in
        Parse.parse_captured_with_sites source ~limits ~checkpoint |> site_result
    in
    let parse_interface source =
      match recorder with
      | None -> Parse.parse_captured_interface source ~checkpoint |> stage_result Parse
      | Some recorder ->
        let limits = Recorder.site_limits recorder ~checkpoint |> Recorder.take_exn in
        Parse.parse_captured_interface_with_sites source ~limits ~checkpoint
        |> site_result
    in
    let entry_id = Prepared.Module_id.of_implementation (Prepared.entry prepared) in
    let units =
      Session.run prepared ~max_dependency_depth ~checkpoint ~check:(fun node ~require ->
        let open Result.Let_syntax in
        let source = Session.Unit.implementation node in
        let%bind parsed = parse (Lookup.source source) in
        let%bind interface =
          match Session.Unit.interface node with
          | None -> Ok None
          | Some source ->
            parse_interface (Lookup.source source) |> Result.map ~f:Option.some
        in
        let require ~source reference =
          require ~source reference
          |> Result.map ~f:(fun (unit : Unit.t) ->
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
        Ok { Unit.parsed; interface; checked; entrypoints })
    in
    match units with
    | Error diagnostic ->
      Option.iter recorder ~f:Recorder.discard;
      (match !cause with
       | Some failure -> Error failure
       | None -> failure Typecheck diagnostic)
    | Ok units ->
      let semantic_facts =
        Option.map recorder ~f:(fun recorder ->
          Recorder.settle recorder ~checkpoint |> Recorder.take_exn)
      in
      checkpoint ();
      Ok { units; semantic_facts })
;;

let check ?recording prepared ~contract ~max_dependency_depth ~checkpoint =
  match recording with
  | None -> check_invocation prepared ~contract ~max_dependency_depth ~checkpoint
  | Some limits ->
    let recorder = Recorder.create prepared ~limits in
    (match
       Recorder.with_recording recorder ~checkpoint ~f:(fun ~checkpoint ->
         check_invocation ~recorder prepared ~contract ~max_dependency_depth ~checkpoint)
     with
     | Ok result -> result
     | Error error ->
       Error
         { Failure.kind = Recording error
         ; diagnostic =
             Chatml_diagnostic.create "Semantic fact metadata admission rejected"
         })
;;
