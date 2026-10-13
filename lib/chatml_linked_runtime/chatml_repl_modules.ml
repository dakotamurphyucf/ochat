open Core
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Surface = Chatml.Chatml_builtin_surface
module Lang = Chatml.Chatml_lang
module Typing = Chatml_repl_typing
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Graph = Chatml_dependency_graph
module Lookup = Chatml_configured_lookup
module Id = Prepared.Module_id
module Initializer = Chatml_module_initializer
module Check = Chatml_configured_check

module Error = struct
  type t =
    | Closed
    | Busy
    | Prepared_mismatch
    | Runtime_surface of Diagnostic.t
    | Invalid_linkage of Diagnostic.t
    | Lowering of Diagnostic.t
    | Typing of Typing.Error.t
    | Faulted of
        { module_id : Id.t
        ; diagnostic : Diagnostic.t
        }
    | Runtime of
        { module_id : Id.t
        ; diagnostic : Diagnostic.t
        }
    | Foreign_admission
    | Consumed_admission
    | Finalized_admission
  [@@deriving sexp_of]
end

type unit_state =
  | Pending
  | Initializing
  | Initialized of Lang.value
  | Faulted of Diagnostic.t

type artifact =
  { unit : Initializer.Unit.t
  ; direct_dependencies : Id.t list
  ; interrupted : Diagnostic.t
  ; mutable state : unit_state
  }

type t =
  { typing : Typing.t
  ; input_fingerprint : Captured.Digest.t
  ; surface : Surface.surface
  ; control : Lang.execution_control
  ; mutable artifacts : artifact Map.M(Id).t
  ; mutable completed : Lang.Configured_modules.t
  ; mutable current : admission option
  ; mutable busy : bool
  ; mutable closed : bool
  }

and admission =
  { owner : t
  ; reservation : Typing.Reservation.t
  ; selected_order : Id.t list
  ; direct_roots : Id.t list
  ; mutable consumed : bool
  }

module Admission = struct
  type t = admission

  let typing_reservation t = t.reservation
  let selected_order t = t.selected_order
end

module Status = struct
  type t =
    | Pending
    | Initializing
    | Initialized
    | Faulted of Diagnostic.t
  [@@deriving sexp_of]
end

let error message = Error (Error.Invalid_linkage (Diagnostic.create message))

let create ~typing ~surface ~control ~checkpoint =
  let prepared = Typing.prepared typing in
  let contract = Typing.contract typing in
  checkpoint ();
  if
    not
      (Captured.Digest.equal
         (Prepared.compiler_contract prepared)
         (Contract.fingerprint contract ~checkpoint))
  then Error Error.Prepared_mismatch
  else if not (Contract.matches_runtime_surface contract surface ~checkpoint ())
  then
    Error
      (Error.Runtime_surface
         (Diagnostic.create
            "Runtime surface metadata does not match the selected compiler contract"))
  else (
    checkpoint ();
    Ok
      { typing
      ; input_fingerprint = Prepared.fingerprint prepared
      ; surface
      ; control
      ; artifacts = Map.empty (module Id)
      ; completed = Lang.Configured_modules.empty
      ; current = None
      ; busy = false
      ; closed = false
      })
;;

let with_activity t ~f =
  if t.busy
  then Error Error.Busy
  else if t.closed
  then Error Error.Closed
  else (
    t.busy <- true;
    Exn.protect ~f ~finally:(fun () -> t.busy <- false))
;;

let interrupt_diagnostic source ~checkpoint =
  let site =
    Captured.locate source ~start_byte:0 ~end_byte:0 ~checkpoint
    |> Or_error.bind ~f:(fun location ->
      Diagnostic.Site.create source ~span:(Captured.Location.span location) ~checkpoint)
    |> Result.ok
  in
  Diagnostic.create ?site "Module initialization was interrupted after it began"
;;

let direct_dependency_map edges ~checkpoint =
  List.fold
    edges
    ~init:(Map.empty (module Id))
    ~f:(fun map edge ->
      checkpoint ();
      if Graph.Edge.is_runtime_dependency edge
      then
        Map.update map (Graph.Edge.owner edge) ~f:(fun previous ->
          Set.add
            (Option.value previous ~default:(Set.empty (module Id)))
            (Graph.Edge.dependency edge))
      else map)
;;

let dependency_list map id ~checkpoint =
  Map.find map id
  |> Option.value ~default:(Set.empty (module Id))
  |> Set.fold ~init:[] ~f:(fun result dependency ->
    checkpoint ();
    dependency :: result)
  |> List.rev
;;

let selected_direct_roots t candidate ~checkpoint =
  let source = Typing.Candidate.source candidate in
  let expected_owner = Id.of_implementation (Captured.id source) in
  List.fold_result
    (Typing.Candidate.direct_references candidate)
    ~init:(Set.empty (module Id))
    ~f:(fun roots reference ->
      checkpoint ();
      let use_id, use_digest = Lookup.Reference.use_source reference in
      if
        (not
           (Captured.Digest.equal
              t.input_fingerprint
              (Lookup.Reference.input_fingerprint reference)))
        || (not (Id.equal expected_owner (Lookup.Reference.owner reference)))
        || (not (Captured.Id.equal (Captured.id source) use_id))
        || not (Captured.Digest.equal (Captured.digest source) use_digest)
      then error "Direct dependency reference belongs to another REPL source"
      else (
        match Lookup.Reference.reason reference with
        | Value_reference -> Ok (Set.add roots (Lookup.Reference.dependency reference))
        | Open | Qualified_type -> Ok roots))
  |> Result.map ~f:(fun roots ->
    Set.fold roots ~init:[] ~f:(fun ids id ->
      checkpoint ();
      id :: ids)
    |> List.rev)
;;

let prepare_artifacts t candidate ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if
      Captured.Digest.equal
        t.input_fingerprint
        (Typing.Candidate.input_fingerprint candidate)
    then Ok ()
    else Error Error.Prepared_mismatch
  in
  let checked_order = Typing.Candidate.checked_dependency_order candidate ~checkpoint in
  let edges = Typing.Candidate.dependency_edges candidate in
  let%bind roots = selected_direct_roots t candidate ~checkpoint in
  let%bind selected_order =
    Graph.runtime_order_for_roots
      ~input_fingerprint:t.input_fingerprint
      ~checked_order
      ~edges
      ~roots
      ~checkpoint
    |> Result.map_error ~f:(fun diagnostic -> Error.Invalid_linkage diagnostic)
  in
  let dependencies = direct_dependency_map edges ~checkpoint in
  let%bind artifacts =
    List.fold_result
      (Typing.Candidate.new_checked_dependencies candidate)
      ~init:t.artifacts
      ~f:(fun artifacts dependency ->
        checkpoint ();
        let id = Typing.Candidate.Checked_dependency.id dependency in
        let parsed = Typing.Candidate.Checked_dependency.implementation dependency in
        let source = Chatml.Chatml_parse.Unit.source parsed in
        if Map.mem artifacts id
        then error "Candidate repeats an already lowered canonical unit"
        else if not (Captured.Id.equal (Id.implementation id) (Captured.id source))
        then error "Checked dependency has a foreign implementation source"
        else (
          let checked = Typing.Candidate.Checked_dependency.checked dependency in
          let%map program =
            Chatml_resolver.resolve_configured_program
              (Check.Checked_unit.annotations checked)
              parsed
              ~checkpoint
            |> Result.map_error ~f:(fun diagnostic -> Error.Lowering diagnostic)
          in
          let unit =
            Initializer.Unit.create
              ~id
              ~source
              ~program
              ~visibility:(Check.Checked_unit.visibility checked)
          in
          let direct_dependencies = dependency_list dependencies id ~checkpoint in
          let interrupted = interrupt_diagnostic source ~checkpoint in
          Map.set
            artifacts
            ~key:id
            ~data:{ unit; direct_dependencies; interrupted; state = Pending }))
  in
  let%bind () =
    List.fold_result checked_order ~init:() ~f:(fun () id ->
      checkpoint ();
      match Map.find artifacts id with
      | None -> error "Checked canonical unit has no lowered runtime artifact"
      | Some artifact ->
        let expected = dependency_list dependencies id ~checkpoint in
        if List.equal Id.equal artifact.direct_dependencies expected
        then Ok ()
        else error "Canonical module dependency edges changed within the epoch")
  in
  let%bind () =
    if Map.length artifacts = List.length checked_order
    then Ok ()
    else error "Runtime artifact registry does not match checked canonical roots"
  in
  checkpoint ();
  Ok (artifacts, selected_order, roots)
;;

let reserve t candidate ~checkpoint =
  with_activity t ~f:(fun () ->
    let discard_if_current () =
      match Typing.validate_candidate t.typing candidate with
      | Ok () ->
        ignore
          (Typing.discard_candidate t.typing candidate : (unit, Typing.Error.t) result)
      | Error _ -> ()
    in
    let work () =
      let open Result.Let_syntax in
      let%bind () =
        match t.current with
        | Some admission
          when Typing.Reservation.pending_for admission.reservation ~owner:t.typing ->
          Error Error.Busy
        | Some _ | None -> Ok ()
      in
      let%bind () =
        Typing.validate_candidate t.typing candidate
        |> Result.map_error ~f:(fun error -> Error.Typing error)
      in
      let%bind artifacts, selected_order, direct_roots =
        prepare_artifacts t candidate ~checkpoint
      in
      let%bind reservation =
        Typing.reserve_execution t.typing candidate ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Typing error)
      in
      let admission =
        { owner = t; reservation; selected_order; direct_roots; consumed = false }
      in
      t.artifacts <- artifacts;
      t.current <- Some admission;
      Ok admission
    in
    match work () with
    | Ok admission -> Ok admission
    | Error ((Error.Lowering _ | Invalid_linkage _ | Prepared_mismatch) as failure) ->
      discard_if_current ();
      Error failure
    | Error error -> Error error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      discard_if_current ();
      Exn.raise_with_original_backtrace exn backtrace)
;;

let current_admission t admission =
  phys_equal admission.owner t && Option.exists t.current ~f:(phys_equal admission)
;;

let initialized_value t id =
  match Map.find t.artifacts id with
  | Some { state = Initialized value; _ } -> Ok value
  | Some { state = Faulted diagnostic; _ } ->
    Error (Error.Faulted { module_id = id; diagnostic })
  | Some { state = Pending | Initializing; _ } | None ->
    error "Selected configured module has not completed initialization"
;;

let selected_modules t roots =
  List.fold_result roots ~init:Lang.Configured_modules.empty ~f:(fun modules id ->
    t.control.diagnostic_checkpoint ();
    let open Result.Let_syntax in
    let%bind value = initialized_value t id in
    t.control.allocate 256;
    match
      Lang.Configured_modules.add
        modules
        ~implementation:(Id.implementation id)
        ~public_module:value
    with
    | Ok modules -> Ok modules
    | Error _ -> error "Duplicate direct configured module identity")
;;

let initialize t admission =
  with_activity t ~f:(fun () ->
    if not (current_admission t admission)
    then Error Error.Foreign_admission
    else if admission.consumed
    then Error Error.Consumed_admission
    else if not (Typing.Reservation.pending_for admission.reservation ~owner:t.typing)
    then Error Error.Finalized_admission
    else (
      admission.consumed <- true;
      let open Result.Let_syntax in
      let%bind () =
        List.fold_result admission.selected_order ~init:() ~f:(fun () id ->
          t.control.diagnostic_checkpoint ();
          match Map.find t.artifacts id with
          | Some { state = Faulted diagnostic; _ } ->
            Error (Error.Faulted { module_id = id; diagnostic })
          | Some { state = Pending | Initialized _; _ } -> Ok ()
          | Some { state = Initializing; _ } | None ->
            error "Selected configured module is unavailable")
      in
      let%bind () =
        List.fold_result admission.selected_order ~init:() ~f:(fun () id ->
          t.control.diagnostic_checkpoint ();
          match Map.find t.artifacts id with
          | None -> error "Selected configured module is unavailable"
          | Some artifact ->
            (match artifact.state with
             | Initialized _ -> Ok ()
             | Faulted diagnostic -> Error (Error.Faulted { module_id = id; diagnostic })
             | Initializing -> error "Selected configured module is already initializing"
             | Pending ->
               let evaluate () =
                 Initializer.evaluate
                   artifact.unit
                   ~surface:t.surface
                   ~completed:t.completed
                   ~direct_dependencies:artifact.direct_dependencies
                   ~phase:Chatml_trace.Event.Phase.Dependency_initialization
                   ~before_evaluation:(fun () -> artifact.state <- Initializing)
                   ~control:t.control
                   ()
               in
               (match evaluate () with
                | Error failure ->
                  let diagnostic = Initializer.Failure.diagnostic failure in
                  (match artifact.state with
                   | Initializing -> artifact.state <- Faulted diagnostic
                   | Pending -> ()
                   | Initialized _ | Faulted _ -> assert false);
                  (match failure with
                   | Initializer.Failure.Invalid_linkage _ ->
                     Error (Error.Invalid_linkage diagnostic)
                   | Runtime _ -> Error (Error.Runtime { module_id = id; diagnostic }))
                | Ok (_private_env, public_module) ->
                  (match
                     Lang.Configured_modules.add
                       t.completed
                       ~implementation:(Id.implementation id)
                       ~public_module
                   with
                   | Error _ ->
                     artifact.state <- Faulted artifact.interrupted;
                     error "Configured module initialized more than once"
                   | Ok completed ->
                     t.completed <- completed;
                     artifact.state <- Initialized public_module;
                     Ok ())
                | exception exn ->
                  let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                  (match artifact.state with
                   | Initializing -> artifact.state <- Faulted artifact.interrupted
                   | Pending -> ()
                   | Initialized _ | Faulted _ -> assert false);
                  Exn.raise_with_original_backtrace exn backtrace)))
      in
      selected_modules t admission.direct_roots))
;;

let status t id =
  if t.closed
  then None
  else
    Option.map (Map.find t.artifacts id) ~f:(fun artifact ->
      match artifact.state with
      | Pending -> Status.Pending
      | Initializing -> Status.Initializing
      | Initialized _ -> Status.Initialized
      | Faulted diagnostic -> Status.Faulted diagnostic)
;;

let close t =
  if t.busy
  then Error Error.Busy
  else if t.closed
  then Ok ()
  else (
    match t.current with
    | Some admission
      when Typing.Reservation.pending_for admission.reservation ~owner:t.typing ->
      Error Error.Busy
    | Some _ | None ->
      t.closed <- true;
      t.artifacts <- Map.empty (module Id);
      t.completed <- Lang.Configured_modules.empty;
      t.current <- None;
      Ok ())
;;
