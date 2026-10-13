open Core
module Lang = Chatml.Chatml_lang
module Eval = Chatml.Chatml_eval
module Linked = Chatml_linked_program
module Surface = Chatml.Chatml_builtin_surface
module Contract = Chatml.Chatml_compiler_contract
module Captured = Chatml_source
module Graph = Chatml_dependency_graph
module Id = Chatml_prepared_program.Module_id
module Reference = Chatml.Chatml_module_reference
module Diagnostic = Chatml_diagnostic

module Error = struct
  module Kind = struct
    type t =
      | Entrypoint_selection
      | Runtime_surface
      | Invalid_linkage
      | Runtime
    [@@deriving equal, sexp_of]
  end

  type t =
    { kind : Kind.t
    ; diagnostic : Diagnostic.t
    }

  let kind t = t.kind
  let diagnostic t = t.diagnostic
end

type t =
  { env : Lang.env
  ; source : Captured.t
  ; bindings : Lang.value String.Map.t
  }

let entry_env t = t.env
let entry_source t = t.source
let binding t ~name = Map.find t.bindings name

let instantiate program ~surface ~entrypoints ?control () =
  let open Result.Let_syntax in
  let checkpoint () =
    Option.iter control ~f:(fun control -> control.Lang.diagnostic_checkpoint ())
  in
  let allocate bytes =
    Option.iter control ~f:(fun control -> control.Lang.allocate bytes)
  in
  let source = Linked.Unit.implementation (Linked.entry program) in
  let failure ?(source = source) kind message =
    let site =
      Captured.locate source ~start_byte:0 ~end_byte:0 ~checkpoint
      |> Or_error.bind ~f:(fun location ->
        Diagnostic.Site.create source ~span:(Captured.Location.span location) ~checkpoint)
      |> Result.ok
    in
    Error { Error.kind; diagnostic = Diagnostic.create ?site message }
  in
  let runtime result =
    Result.map_error result ~f:(fun (diagnostic : Diagnostic.t) ->
      Chatml_trace.Scope.observe_site
        ~kind:
          (Chatml_trace.Event.Kind.Unrecovered_failure Chatml_trace.Event.Failure.Runtime)
        ~site:diagnostic.site
        ~correlation:None;
      { Error.kind = Runtime; diagnostic })
  in
  checkpoint ();
  let%bind () =
    if List.is_empty entrypoints
    then failure Entrypoint_selection "Prepared execution requires a compiled entrypoint"
    else Ok ()
  in
  let available =
    List.fold
      (Linked.entrypoints program)
      ~init:String.Map.empty
      ~f:(fun map descriptor ->
        checkpoint ();
        allocate 128;
        Map.set map ~key:(Linked.Entrypoint.name descriptor) ~data:descriptor)
  in
  let%bind selected =
    List.fold_result entrypoints ~init:String.Map.empty ~f:(fun selected name ->
      checkpoint ();
      match Map.find available name with
      | None ->
        failure Entrypoint_selection "Prepared entrypoint was not selected at compilation"
      | Some descriptor ->
        allocate 128;
        Ok (Map.set selected ~key:name ~data:descriptor))
  in
  let%bind () =
    if
      Contract.matches_runtime_surface
        (Linked.static_contract program)
        surface
        ~checkpoint
        ~allocate
        ()
    then Ok ()
    else
      failure
        Runtime_surface
        "Runtime surface metadata does not match the linked compiler contract"
  in
  let dependencies =
    List.fold
      (Graph.edges (Linked.graph program))
      ~init:(Map.empty (module Id))
      ~f:(fun map edge ->
        checkpoint ();
        if Graph.Edge.is_runtime_dependency edge
        then (
          allocate 256;
          Map.update map (Graph.Edge.owner edge) ~f:(fun previous ->
            Set.add
              (Option.value previous ~default:(Set.empty (module Id)))
              (Graph.Edge.dependency edge)))
        else map)
  in
  let restricted completed id =
    let required =
      Map.find dependencies id |> Option.value ~default:(Set.empty (module Id))
    in
    let required =
      Set.fold required ~init:[] ~f:(fun deps dependency ->
        checkpoint ();
        allocate 24;
        dependency :: deps)
    in
    List.fold_result
      required
      ~init:Lang.Configured_modules.empty
      ~f:(fun modules dependency ->
        checkpoint ();
        let implementation = Id.implementation dependency in
        let%bind reference =
          match Reference.create implementation ~member_path:[] with
          | Ok reference -> Ok reference
          | Error _ -> failure Invalid_linkage "Invalid canonical dependency identity"
        in
        let%bind value =
          match Lang.Configured_modules.read completed reference with
          | Ok value -> Ok value
          | Error _ ->
            failure
              Invalid_linkage
              "Configured dependency has not completed initialization"
        in
        allocate 256;
        match
          Lang.Configured_modules.add modules ~implementation ~public_module:value
        with
        | Ok modules -> Ok modules
        | Error _ -> failure Invalid_linkage "Invalid completed dependency module")
  in
  let%bind completed, entry_env =
    List.fold_result
      (Linked.runtime_order program)
      ~init:(Lang.Configured_modules.empty, None)
      ~f:(fun (completed, entry_env) id ->
        checkpoint ();
        let%bind unit =
          match Linked.find_unit program id with
          | Some unit -> Ok unit
          | None -> failure Invalid_linkage "Linked runtime unit is unavailable"
        in
        let%bind configured_modules = restricted completed id in
        let base =
          Chatml_builtin_modules.create_controlled_env_with_surface ?control surface
        in
        let base = { base with Lang.configured_modules } in
        let unit_source = Linked.Unit.implementation unit in
        let unit_program = Linked.Unit.program unit in
        let initialization_phase =
          if Id.equal id (Graph.entry (Linked.graph program))
          then Chatml_trace.Event.Phase.Entry_initialization
          else Chatml_trace.Event.Phase.Dependency_initialization
        in
        let phase_span =
          match unit_program.stmts with
          | stmt :: _ -> stmt.span
          | [] ->
            let start = Source.{ line = 1; column = 0; offset = 0 } in
            Source.{ left = start; right = start }
        in
        let phase kind =
          Chatml_trace.Scope.observe_span
            ~kind
            ~source:(Some unit_source)
            ~span:phase_span
            ~correlation:None
        in
        phase (Chatml_trace.Event.Kind.Phase_started initialization_phase);
        let%bind env =
          Eval.eval_captured_program base ~source:unit_source unit_program |> runtime
        in
        let%bind location =
          match
            Captured.locate
              unit_source
              ~start_byte:0
              ~end_byte:(Captured.byte_length unit_source)
              ~checkpoint
          with
          | Ok location -> Ok location
          | Error _ ->
            failure ~source:unit_source Invalid_linkage "Invalid linked unit range"
        in
        let%bind public_env =
          Eval.select_exports
            env
            ~visibility:(Linked.Unit.visibility unit)
            ~span:(Captured.Location.span location)
          |> runtime
        in
        checkpoint ();
        allocate 256;
        let%bind completed =
          match
            Lang.Configured_modules.add
              completed
              ~implementation:(Id.implementation id)
              ~public_module:(Lang.VModule public_env)
          with
          | Ok completed -> Ok completed
          | Error _ ->
            failure
              ~source:unit_source
              Invalid_linkage
              "Linked module initialized more than once"
        in
        phase (Chatml_trace.Event.Kind.Phase_finished initialization_phase);
        Ok
          ( completed
          , if Id.equal id (Graph.entry (Linked.graph program))
            then Some env
            else entry_env ))
  in
  let%bind env =
    match entry_env with
    | Some env -> Ok env
    | None -> failure Invalid_linkage "Linked entry was not initialized"
  in
  let%bind bindings =
    Map.fold
      selected
      ~init:(Ok String.Map.empty)
      ~f:(fun ~key:name ~data:descriptor result ->
        checkpoint ();
        let%bind bindings = result in
        let value =
          match Linked.Entrypoint.target descriptor with
          | Local_binding name -> Lang.find_var env name
          | Configured_member reference ->
            Lang.Configured_modules.read completed reference |> Result.ok
        in
        match value with
        | None -> failure Invalid_linkage "Compiled entrypoint binding is unavailable"
        | Some value ->
          allocate 128;
          Ok (Map.set bindings ~key:name ~data:value))
  in
  checkpoint ();
  Ok { env; source; bindings }
;;
