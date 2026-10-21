open Core
module Lang = Chatml.Chatml_lang
module Eval = Chatml.Chatml_eval
module Id = Chatml_prepared_program.Module_id
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Reference = Chatml.Chatml_module_reference

module Unit = struct
  type t =
    { id : Id.t
    ; source : Captured.t
    ; program : Lang.resolved_program
    ; visibility : Chatml.Chatml_module_visibility.t
    }

  let create ~id ~source ~program ~visibility = { id; source; program; visibility }
  let id t = t.id
  let source t = t.source
end

module Failure = struct
  type t =
    | Invalid_linkage of Diagnostic.t
    | Runtime of Diagnostic.t

  let diagnostic = function
    | Invalid_linkage diagnostic | Runtime diagnostic -> diagnostic
  ;;
end

let evaluate
      unit
      ~surface
      ~completed
      ~direct_dependencies
      ~phase
      ?(before_evaluation = fun () -> ())
      ?control
      ()
  =
  let open Result.Let_syntax in
  let checkpoint () =
    Option.iter control ~f:(fun control -> control.Lang.diagnostic_checkpoint ())
  in
  let allocate bytes =
    Option.iter control ~f:(fun control -> control.Lang.allocate bytes)
  in
  let invalid message =
    let site =
      Captured.locate unit.Unit.source ~start_byte:0 ~end_byte:0 ~checkpoint
      |> Or_error.bind ~f:(fun location ->
        Diagnostic.Site.create
          unit.source
          ~span:(Captured.Location.span location)
          ~checkpoint)
      |> Result.ok
    in
    Error (Failure.Invalid_linkage (Diagnostic.create ?site message))
  in
  let runtime result =
    Result.map_error result ~f:(fun diagnostic ->
      Chatml_trace.Scope.observe_site
        ~kind:
          (Chatml_trace.Event.Kind.Unrecovered_failure Chatml_trace.Event.Failure.Runtime)
        ~site:diagnostic.Diagnostic.site
        ~correlation:None;
      Failure.Runtime diagnostic)
  in
  let%bind configured_modules =
    List.fold_result
      direct_dependencies
      ~init:Lang.Configured_modules.empty
      ~f:(fun modules dependency ->
        checkpoint ();
        let implementation = Id.implementation dependency in
        let%bind reference =
          match Reference.create implementation ~member_path:[] with
          | Ok reference -> Ok reference
          | Error _ -> invalid "Invalid canonical dependency identity"
        in
        let%bind value =
          match Lang.Configured_modules.read completed reference with
          | Ok value -> Ok value
          | Error _ -> invalid "Configured dependency has not completed initialization"
        in
        allocate 256;
        match
          Lang.Configured_modules.add modules ~implementation ~public_module:value
        with
        | Ok modules -> Ok modules
        | Error _ -> invalid "Invalid completed dependency module")
  in
  let base = Chatml_builtin_modules.create_controlled_env_with_surface ?control surface in
  let base = { base with Lang.configured_modules } in
  let phase_span =
    match unit.Unit.program.stmts with
    | stmt :: _ -> stmt.span
    | [] ->
      let start = Source.{ line = 1; column = 0; offset = 0 } in
      Source.{ left = start; right = start }
  in
  let trace kind =
    Chatml_trace.Scope.observe_span
      ~kind
      ~source:(Some unit.source)
      ~span:phase_span
      ~correlation:None
  in
  before_evaluation ();
  trace (Chatml_trace.Event.Kind.Phase_started phase);
  let%bind env =
    Eval.eval_captured_program base ~source:unit.source unit.program |> runtime
  in
  let%bind location =
    match
      Captured.locate
        unit.source
        ~start_byte:0
        ~end_byte:(Captured.byte_length unit.source)
        ~checkpoint
    with
    | Ok location -> Ok location
    | Error _ -> invalid "Invalid linked unit range"
  in
  let%bind public_env =
    Eval.select_exports
      env
      ~visibility:unit.visibility
      ~span:(Captured.Location.span location)
    |> runtime
  in
  checkpoint ();
  allocate 256;
  trace (Chatml_trace.Event.Kind.Phase_finished phase);
  Ok (env, Lang.VModule public_env)
;;
