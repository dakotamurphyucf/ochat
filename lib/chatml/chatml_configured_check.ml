open Core
module Lookup = Chatml_configured_lookup
module Reference = Lookup.Reference
module Parse = Chatml.Chatml_parse
module Checker = Chatml_typechecker
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Scope = Checker.Value_scope
module Public_module = Checker.Configured_resolution.Public_module
module Visibility = Chatml.Chatml_module_visibility
module Static = Chatml.Chatml_static_surface
module Signature = Checker.Signature_check
module Semantic = Checker.Semantic

module Checked_unit = struct
  type t =
    { annotations : Checker.checked_program
    ; exports : Public_module.t
    ; visibility : Visibility.t
    ; scope : Scope.t
    ; context : Lookup.t
    ; invocation : unit ref
    ; semantic_unit : Semantic.Unit.t option
    }

  let annotations t = t.annotations
  let exports t = t.exports
  let visibility t = t.visibility
end

type t =
  { machine : Checker.infer_state
  ; surface : Static.t
  ; invocation : unit ref
  ; recorder : Semantic.t option
  }

let create_static ?recorder surface ~checkpoint =
  { machine = Checker.create_state ~checkpoint ()
  ; surface
  ; invocation = ref ()
  ; recorder
  }
;;

let create surface ~checkpoint =
  create_static (Static.of_runtime_surface surface ~checkpoint) ~checkpoint
;;

let paired source captured phase =
  Captured.Id.equal (Captured.id captured) (Captured.id (Lookup.source source))
  && Captured.Digest.equal
       (Captured.digest captured)
       (Captured.digest (Lookup.source source))
  && Lookup.Phase.equal (Lookup.phase source) phase
;;

(* Both borrowed callbacks have origin protection outside compiler-domain handlers.
   No state/callback is retained by Checked_unit. *)
let with_state ?semantic_unit t context ~require ~f =
  let exception Callback_raised of exn * Stdlib.Printexc.raw_backtrace in
  let guarded_require ~source reference =
    try require ~source reference with
    | exn -> raise (Callback_raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  try
    Chatml.Chatml_checkpoint.protect t.machine.checkpoint ~f:(fun checkpoint ->
      let state = Checker.for_source t.machine (Lookup.source context) in
      let configured =
        Checker.Configured_resolution.create context ~require:guarded_require ~checkpoint
      in
      let semantic =
        match t.recorder, semantic_unit with
        | Some recorder, Some unit ->
          Some (Semantic.frame recorder unit ~checkpoint |> Semantic.take_exn)
        | None, None -> None
        | Some _, None | None, Some _ ->
          invalid_arg "Configured_check.with_state: inconsistent recorder unit"
      in
      let semantic_members =
        Option.map semantic ~f:(fun _ -> Checker.SpanTbl.create ())
      in
      let state =
        { state with
          checkpoint
        ; configured = Some configured
        ; semantic
        ; semantic_members
        }
      in
      Checker.with_unit_level state ~f:(fun () ->
        try f state with
        | Checker.Type_diagnostic diagnostic -> Error diagnostic
        | Checker.Type_error message -> Error (Diagnostic.create message)
        | Checker.Type_error_with_loc (message, span) ->
          Error
            (Chatml.Chatml_owned_source.attach
               (Lookup.source context)
               (Diagnostic.create ~span message)
               ~checkpoint)))
  with
  | Callback_raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let unrestricted_visibility exports ~checkpoint =
  let fields, _ =
    Checker.merge_fields
      ~checkpoint
      (Public_module.value_scheme exports
       |> function
       | Checker.Module row -> row
       | _ -> assert false)
  in
  Checker.Env.bindings fields
  |> List.map ~f:(fun (name, _) ->
    checkpoint ();
    Visibility.Value name)
;;

let check_implementation t ~source ~require implementation =
  if not (paired source (Parse.Unit.source implementation) Lookup.Phase.Implementation)
  then
    Error
      (Diagnostic.create
         "Configured parser unit does not match its admitted implementation context")
  else (
    let semantic_unit =
      Option.map t.recorder ~f:(fun recorder ->
        Semantic.implementation
          recorder
          source
          implementation
          ~checkpoint:t.machine.checkpoint
        |> Semantic.take_exn)
    in
    with_state ?semantic_unit t source ~require ~f:(fun state ->
      let annotations, exports, scope =
        Checker.infer_configured_unit_static
          state
          t.surface
          (Parse.Unit.program implementation)
      in
      let visibility = unrestricted_visibility exports ~checkpoint:state.checkpoint in
      state.checkpoint ();
      Ok
        { Checked_unit.annotations
        ; exports
        ; visibility
        ; scope
        ; context = source
        ; invocation = t.invocation
        ; semantic_unit
        }))
;;

let record_exports t checked =
  match
    t.recorder, checked.Checked_unit.semantic_unit, Public_module.members checked.exports
  with
  | Some recorder, Some unit, Some members ->
    Semantic.exports recorder unit members ~checkpoint:t.machine.checkpoint
    |> Semantic.take_exn
  | None, _, _ | Some _, None, _ | Some _, Some _, None -> ()
;;

let check_unit t ~source ~require implementation =
  Result.map (check_implementation t ~source ~require implementation) ~f:(fun checked ->
    record_exports t checked;
    checked)
;;

let check_node t node ~require ~implementation ~interface =
  let source = Chatml_dependency_session.Unit.implementation node in
  let companion = Chatml_dependency_session.Unit.interface node in
  let interface_paired =
    match companion, interface with
    | None, None -> true
    | Some context, Some parsed ->
      paired context (Parse.Interface.source parsed) Lookup.Phase.Interface
    | None, Some _ | Some _, None -> false
  in
  if
    not
      (paired source (Parse.Unit.source implementation) Lookup.Phase.Implementation
       && interface_paired)
  then
    Error
      (Diagnostic.create
         "Configured implementation/interface does not match its admitted node")
  else
    let open Result.Let_syntax in
    let%bind checked = check_implementation t ~source ~require implementation in
    match companion, interface with
    | None, None ->
      record_exports t checked;
      Ok checked
    | Some context, Some interface ->
      let semantic_unit =
        Option.map t.recorder ~f:(fun recorder ->
          Semantic.interface recorder context interface ~checkpoint:t.machine.checkpoint
          |> Semantic.take_exn)
      in
      with_state ?semantic_unit t context ~require ~f:(fun state ->
        let env =
          Checker.init_env_with_static
            ?semantic:state.semantic
            ~checkpoint:state.checkpoint
            t.surface
        in
        let outer_types =
          Checker.init_types_with_static
            ?semantic:state.semantic
            ~checkpoint:state.checkpoint
            t.surface
        in
        let values =
          match Public_module.value_scheme checked.exports with
          | Checker.Module row ->
            fst (Checker.merge_fields ~checkpoint:state.checkpoint row)
          | _ -> assert false
        in
        let%bind signature =
          Checker.check_captured_module_signature
            ~semantic_values:env
            ?semantic_members:(Public_module.members checked.exports)
            ?lookup:(Checker.type_lookup state env)
            state
            interface
            ~values
            ~types:(Public_module.types checked.exports)
            ~outer_types
        in
        let exports =
          Public_module.create
            ~values:(Signature.values signature)
            ~types:(Signature.types signature)
          |> fun exports ->
          Option.value_map
            (Signature.members signature)
            ~default:exports
            ~f:(Public_module.with_members exports)
        in
        state.checkpoint ();
        let checked =
          { checked with exports; visibility = Signature.visibility signature }
        in
        record_exports t checked;
        Ok checked)
    | None, Some _ | Some _, None -> assert false
;;

module Entrypoint = struct
  type t =
    { name : string
    ; target : Chatml.Chatml_module_reference.Binding_target.t
    }

  let name t = t.name
  let target t = t.target
end

let same_context a b =
  Chatml_prepared_program.Module_id.equal (Lookup.owner a) (Lookup.owner b)
  && Captured.Digest.equal (Lookup.input_fingerprint a) (Lookup.input_fingerprint b)
  && paired a (Lookup.source b) (Lookup.phase b)
;;

let check_entrypoints t (checked : Checked_unit.t) ~source ~require ~required_bindings =
  if
    not
      (phys_equal t.invocation checked.invocation
       && same_context source checked.context
       && Lookup.Phase.equal (Lookup.phase source) Implementation)
  then
    Error
      (Diagnostic.create
         "Host entrypoint bindings do not belong to this compiler invocation and \
          admitted implementation")
  else
    with_state ?semantic_unit:checked.semantic_unit t source ~require ~f:(fun state ->
      let targets = ref [] in
      let lookup_binding name =
        state.checkpoint ();
        let binding =
          match Scope.find checked.scope ~name with
          | Some binding -> binding
          | None -> raise (Checker.Type_error ("Unknown variable '" ^ name ^ "'"))
        in
        let scheme, target =
          match Scope.Binding.origin binding with
          | None ->
            ( Scope.Binding.scheme binding
            , Chatml.Chatml_module_reference.Binding_target.Local_binding name )
          | Some origin ->
            let span =
              match Scope.Binding.span binding with
              | Some span -> span
              | None ->
                raise
                  (Checker.Type_error
                     "Imported host binding has no original introduction site")
            in
            let scheme, _ =
              Checker.Configured_resolution.value
                (Option.value_exn state.configured)
                ~env:checked.scope
                ~name
                ~span
              |> Checker.alias_result
            in
            let reference =
              Chatml.Chatml_module_reference.create
                (Chatml_prepared_program.Module_id.implementation
                   (Lookup.Origin.dependency origin))
                ~member_path:(Lookup.Origin.member_path origin)
              |> Or_error.ok_exn
            in
            ( scheme
            , Chatml.Chatml_module_reference.Binding_target.Configured_member reference )
        in
        targets := { Entrypoint.name; target } :: !targets;
        if Checker.contains_generic ~checkpoint:state.checkpoint scheme
        then Checker.instantiate state scheme
        else scheme
      in
      Checker.check_required_bindings
        ~semantic_scope:checked.scope
        ~binding_span:(fun name ->
          Option.bind (Scope.find checked.scope ~name) ~f:Scope.Binding.span)
        state
        required_bindings
        ~lookup_binding;
      state.checkpoint ();
      Ok (List.rev !targets))
;;
