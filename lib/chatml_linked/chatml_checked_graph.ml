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

module Invalidity = struct
  type t =
    | Parse of
        { source : Captured.Id.t
        ; phase : Lookup.Phase.t
        }
    | Implementation of Captured.Id.t
    | Companion of Captured.Id.t
    | Host_requirements
    | Dependency
  [@@deriving sexp_of]
end

module Parsed = struct
  type t =
    { source : Captured.t
    ; owner : Prepared.Module_id.t
    ; phase : Lookup.Phase.t
    ; sites : Chatml.Chatml_syntax_sites.t
    }

  let source t = t.source
  let owner t = t.owner
  let phase t = t.phase
  let sites t = t.sites
end

module Failure = struct
  type kind =
    | Compiler of Stage.t
    | Sites of Chatml.Chatml_syntax_sites.Error.t
    | Recording of Recorder.Error.t
    | Admission

  type t =
    { kind : kind
    ; diagnostic : Chatml_diagnostic.t
    ; invalidity : Invalidity.t option
    ; parsed : Parsed.t list
    }

  let kind t = t.kind
  let diagnostic t = t.diagnostic
  let invalidity t = t.invalidity
  let parsed t = t.parsed
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
  ; parsed : Parsed.t list
  }

let graph t = Session.graph t.units
let entry t = Session.entry t.units
let checked t id = Session.checked t.units id
let semantic_facts t = t.semantic_facts
let parsed t = t.parsed

type mode =
  | Executable
  | Provisional

let check_invocation ?recorder mode prepared ~contract ~max_dependency_depth ~checkpoint =
  let failure ?invalidity kind diagnostic =
    Error { Failure.kind; diagnostic; invalidity; parsed = [] }
  in
  let actual = Contract.fingerprint contract ~checkpoint in
  if not (Captured.Digest.equal actual (Prepared.compiler_contract prepared))
  then (
    Option.iter recorder ~f:Recorder.discard;
    failure
      Admission
      (Chatml_diagnostic.create
         "Prepared compiler contract does not match the actual selected static host \
          contract"))
  else (
    let engine =
      Check.create_static ?recorder (Contract.static_surface contract) ~checkpoint
    in
    let cause = ref None in
    let journal =
      Option.map recorder ~f:(fun _ ->
        let remaining =
          List.fold (Prepared.definitions prepared) ~init:2 ~f:(fun count _ ->
            checkpoint ();
            if count > Int.max_value - 2
            then Recorder.take_exn (Error Recorder.Error.Accounting_overflow);
            count + 2)
        in
        ref [], ref remaining)
    in
    let parsed () =
      Option.value_map journal ~default:[] ~f:(fun (entries, _) ->
        List.fold !entries ~init:[] ~f:(fun rows entry ->
          checkpoint ();
          entry :: rows))
    in
    let record_parsed context source sites =
      Option.iter journal ~f:(fun (entries, remaining) ->
        checkpoint ();
        if !remaining <= 0 then Recorder.take_exn (Error Recorder.Error.Fact_limit);
        decr remaining;
        let sites = Option.value_exn sites in
        entries
        := { Parsed.source
           ; owner = Lookup.owner context
           ; phase = Lookup.phase context
           ; sites
           }
           :: !entries)
    in
    let remember ?invalidity kind diagnostic =
      if Option.is_none !cause
      then cause := Some { Failure.kind; diagnostic; invalidity; parsed = [] };
      Error diagnostic
    in
    let stage_result invalidity stage = function
      | Ok value -> Ok value
      | Error diagnostic -> remember ~invalidity (Compiler stage) diagnostic
    in
    let site_result invalidity = function
      | Ok value -> Ok value
      | Error (Parse.Syntax diagnostic) ->
        remember ~invalidity (Compiler Parse) diagnostic
      | Error (Sites error) ->
        remember
          (Sites error)
          (Chatml_diagnostic.create "Parser identifier metadata admission rejected")
    in
    let parse context =
      let source = Lookup.source context in
      let invalidity =
        Invalidity.Parse { source = Captured.id source; phase = Lookup.phase context }
      in
      let result =
        match recorder with
        | None -> Parse.parse_captured source ~checkpoint |> stage_result invalidity Parse
        | Some recorder ->
          let limits = Recorder.site_limits recorder ~checkpoint |> Recorder.take_exn in
          Parse.parse_captured_with_sites source ~limits ~checkpoint
          |> site_result invalidity
      in
      Result.map result ~f:(fun parsed ->
        record_parsed context (Parse.Unit.source parsed) (Parse.Unit.sites parsed);
        parsed)
    in
    let parse_interface context =
      let source = Lookup.source context in
      let invalidity =
        Invalidity.Parse { source = Captured.id source; phase = Lookup.phase context }
      in
      let result =
        match recorder with
        | None ->
          Parse.parse_captured_interface source ~checkpoint
          |> stage_result invalidity Parse
        | Some recorder ->
          let limits = Recorder.site_limits recorder ~checkpoint |> Recorder.take_exn in
          Parse.parse_captured_interface_with_sites source ~limits ~checkpoint
          |> site_result invalidity
      in
      Result.map result ~f:(fun parsed ->
        record_parsed
          context
          (Parse.Interface.source parsed)
          (Parse.Interface.sites parsed);
        parsed)
    in
    let entry_id = Prepared.Module_id.of_implementation (Prepared.entry prepared) in
    let units =
      Session.run prepared ~max_dependency_depth ~checkpoint ~check:(fun node ~require ->
        let open Result.Let_syntax in
        let source = Session.Unit.implementation node in
        let%bind parsed = parse source in
        let%bind interface =
          match Session.Unit.interface node with
          | None -> Ok None
          | Some source -> parse_interface source |> Result.map ~f:Option.some
        in
        let require ~source reference =
          match require ~source reference with
          | Ok (unit : Unit.t) -> Ok (Check.Checked_unit.exports unit.checked)
          | Error diagnostic ->
            remember ~invalidity:Dependency (Compiler Typecheck) diagnostic
        in
        let%bind checked =
          match
            Check.check_node_detailed
              engine
              node
              ~require
              ~implementation:parsed
              ~interface
          with
          | Ok checked -> Ok checked
          | Error error ->
            let diagnostic = Check.Failure.diagnostic error in
            (match Check.Failure.phase error with
             | Implementation ->
               remember
                 ~invalidity:(Implementation (Captured.id (Lookup.source source)))
                 (Compiler Typecheck)
                 diagnostic
             | Companion ->
               let interface = Option.value_exn interface in
               remember
                 ~invalidity:(Companion (Captured.id (Parse.Interface.source interface)))
                 (Compiler Typecheck)
                 diagnostic
             | Admission -> remember Admission diagnostic)
        in
        let%bind entrypoints =
          if
            match mode with
            | Executable -> Prepared.Module_id.equal entry_id (Session.Unit.id node)
            | Provisional -> false
          then
            Check.check_entrypoints
              engine
              checked
              ~source
              ~require
              ~required_bindings:(Contract.required_bindings contract)
            |> stage_result Host_requirements Typecheck
          else Ok []
        in
        checkpoint ();
        Ok { Unit.parsed; interface; checked; entrypoints })
    in
    match units with
    | Error diagnostic ->
      Option.iter recorder ~f:Recorder.discard;
      (match !cause with
       | Some failure -> Error { failure with parsed = parsed () }
       | None ->
         Error
           { Failure.kind = Compiler Typecheck
           ; diagnostic
           ; invalidity = Some Dependency
           ; parsed = parsed ()
           })
    | Ok units ->
      let semantic_facts =
        Option.map recorder ~f:(fun recorder ->
          Recorder.settle recorder ~checkpoint |> Recorder.take_exn)
      in
      checkpoint ();
      Ok { units; semantic_facts; parsed = parsed () })
;;

let check_mode mode ?recording prepared ~contract ~max_dependency_depth ~checkpoint =
  match recording with
  | None -> check_invocation mode prepared ~contract ~max_dependency_depth ~checkpoint
  | Some limits ->
    let recorder = Recorder.create prepared ~limits in
    (match
       Recorder.with_recording recorder ~checkpoint ~f:(fun ~checkpoint ->
         check_invocation
           ~recorder
           mode
           prepared
           ~contract
           ~max_dependency_depth
           ~checkpoint)
     with
     | Ok result -> result
     | Error error ->
       Error
         { Failure.kind = Recording error
         ; diagnostic =
             Chatml_diagnostic.create "Semantic fact metadata admission rejected"
         ; invalidity = None
         ; parsed = []
         })
;;

let check ?recording prepared ~contract ~max_dependency_depth ~checkpoint =
  check_mode Executable ?recording prepared ~contract ~max_dependency_depth ~checkpoint
;;

module Provisional = struct
  type nonrec t = { checked : t }

  let check ?recording prepared ~contract ~max_dependency_depth ~checkpoint =
    check_mode Provisional ?recording prepared ~contract ~max_dependency_depth ~checkpoint
    |> Result.map ~f:(fun checked -> { checked })
  ;;

  let graph t = graph t.checked
  let semantic_facts t = semantic_facts t.checked
  let parsed t = parsed t.checked
end
