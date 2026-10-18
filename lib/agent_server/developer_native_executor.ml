open Core
module P = Agent_protocol
module O = Developer_root_owner
module D = Agent_session.Developer_invocation_owner.Selected
module N = Agent_session.Native_tool_invocation
module C = Chat_response.Tool_capability
module H = P.Host_invocation
module Job_scope = Agent_session.Developer_job_scope
module Jobs = Developer_script_job_service
module Managed_tool = Developer_managed_tool_executor
module Child_selection = Developer_child_selection
module Claimed_runtime = Agent_session.Claimed_child_runtime

type t =
  { root : O.t
  ; issuer : D.t
  ; jobs : Jobs.t option
  ; managed : Managed_tool.t option
  ; managed_env : Eio_unix.Stdenv.base option
  }

(* One private execution selection is carried through native dispatch, lexical
   Jobs and managed dependencies. The ordinary Root constructor below retains
   exactly the preexisting selection; a source-bound child constructor may be
   installed only after its physical selected-target admission. *)
type execution =
  { base : t
  ; issuer : D.t
  ; managed : Managed_tool.t option
  ; source : execution_source
  ; selected_current : unit -> (C.t, P.Error.t) result
  ; selected_for_witness : O.Current.t -> (C.t, P.Error.t) result
  }

and execution_source =
  | Root_source
  | Foreground_child of Child_selection.t
  | Claimed_issuer of Claimed_runtime.t

let issued_by_root t root = phys_equal t.root root
let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f

let create ~root =
  let owner = O.owner root in
  let capture_fingerprint = Chatml_developer_capture.fingerprint (O.capture root) in
  let current ~owner:claimed_owner ~capture_fingerprint:claimed_fingerprint =
    if
      (not (P.Execution_owner.equal_developer owner claimed_owner))
      || not (Chatml_source.Digest.equal capture_fingerprint claimed_fingerprint)
    then invalid "selected native issuer is not the retained developer root"
    else Result.map (O.current_after_mutations root) ~f:ignore
  in
  let* issuer =
    D.create
      ~owner
      ~capture_fingerprint
      ~ceiling:(O.native_ceiling root)
      ~selected:(fun () -> O.native_selected root)
      ~current
      ~now:(fun () -> O.native_now root)
  in
  Ok { root; issuer; jobs = None; managed = None; managed_env = None }
;;

let with_job_service t jobs =
  if not (Jobs.issued_by_root jobs t.root)
  then invalid "developer Job service belongs to another physical root"
  else (
    match t.jobs with
    | None -> Ok { t with jobs = Some jobs }
    | Some existing when phys_equal existing jobs -> Ok t
    | Some _ -> invalid "developer native executor already owns another Job service")
;;

let with_managed_definition (t : t) ~definition ~env =
  match t.managed, t.jobs with
  | Some _, _ -> invalid "managed developer executor is already configured"
  | None, None -> invalid "managed developer executor requires its physical Job service"
  | None, Some jobs ->
    let* managed = Managed_tool.create ~root:t.root ~jobs ~definition ~env in
    Ok { t with managed = Some managed; managed_env = Some env }
;;

let managed_definition (t : t) = Option.map t.managed ~f:Managed_tool.definition

let root_execution (t : t) =
  { base = t
  ; issuer = t.issuer
  ; managed = t.managed
  ; source = Root_source
  ; selected_current = (fun () -> Ok (O.native_selected t.root))
  ; selected_for_witness = (fun witness -> Ok (O.Current.selected witness))
  }
;;

let reselect current ceiling =
  let* names =
    List.fold_result (C.references ceiling) ~init:[] ~f:(fun names reference ->
      C.resolve current ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
      |> Result.map ~f:(fun _ -> reference.name :: names))
  in
  C.select current ~names:(List.rev names)
  |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
;;

let reselect_current (t : t) ceiling =
  let* current = D.selected t.issuer in
  reselect current ceiling
;;

let narrowed_registry execution parent =
  match execution.source with
  | Foreground_child _ -> execution.selected_current ()
  | Root_source | Claimed_issuer _ ->
    let* ceiling = N.selected_borrowed_capabilities parent in
    let* current = execution.selected_current () in
    reselect current ceiling
;;

let finish_completion completion settlement = D.Completion.finish completion ~settlement

let recovery_uncertain message =
  P.Error.create Persistence_error ~message ~retryable:false ()
;;

let finish_after_recovery_exception completion exn backtrace =
  (* The completion carries the original handler/dispatch exception. Finishing it
     raises that original exception first; only an otherwise normal completion
     exposes the later recovery exception. *)
  let _ =
    finish_completion
      completion
      (D.Completion.Uncertain (recovery_uncertain "selected native recovery is uncertain"))
  in
  Exn.raise_with_original_backtrace exn backtrace
;;

let submit_completion scope completion =
  let terminal = D.Completion.terminal completion in
  let selected = D.Completion.selected completion in
  let submitted =
    match
      Eio.Cancel.protect (fun () -> O.Native_terminal.submit scope ~terminal ~selected)
    with
    | Ok revision -> `Saved revision
    | Error error -> `Failed error
    | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let settlement =
    match submitted with
    | `Saved revision -> D.Completion.Terminal_saved revision
    | `Failed error -> Uncertain error
    | `Raised _ ->
      Uncertain
        (P.Error.create
           Persistence_error
           ~message:"selected native terminal save is uncertain"
           ~retryable:false
           ())
  in
  let finished = finish_completion completion settlement in
  match submitted with
  | `Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | `Saved _ | `Failed _ -> finished
;;

let recover_completion scope completion =
  let open O.Native_terminal in
  let reconciled =
    match Eio.Cancel.protect (fun () -> reconcile_dispatch scope) with
    | Ok dispatch -> `Resolved dispatch
    | Error error -> `Failed error
    | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  match reconciled with
  | `Failed error -> finish_completion completion (Uncertain error)
  | `Raised (exn, backtrace) -> finish_after_recovery_exception completion exn backtrace
  | `Resolved dispatch ->
    (match dispatch with
     | Installed (_, dispatch_revision) ->
       let settlement =
         match Eio.Cancel.protect (fun () -> resolve_remembered scope) with
         | Ok terminal_revision ->
           D.Completion.Recovered_terminal_saved { dispatch_revision; terminal_revision }
         | Error error -> Uncertain error
         | exception exn ->
           finish_after_recovery_exception
             completion
             exn
             (Stdlib.Printexc.get_raw_backtrace ())
       in
       finish_completion completion settlement
     | Absent ->
       (match Eio.Cancel.protect (fun () -> abandon_retained_without_dispatch scope) with
        | Ok () -> finish_completion completion Dispatch_absent
        | Error error -> finish_completion completion (Uncertain error)
        | exception exn ->
          finish_after_recovery_exception
            completion
            exn
            (Stdlib.Printexc.get_raw_backtrace ()))
     | Uncertain ->
       finish_completion
         completion
         (Uncertain (recovery_uncertain "selected native dispatch remains uncertain")))
;;

let recover_early scope =
  let open O.Native_terminal in
  let* reconciled = reconcile_dispatch scope in
  match reconciled with
  | Absent ->
    let* retained = retained scope in
    if retained then abandon_retained_without_dispatch scope else Ok ()
  | Installed _ | Uncertain ->
    invalid "pre-dispatch failure left a durable or uncertain invocation"
;;

let rec run
          ?ceiling
          ?native_invocation
          execution
          ~parent
          ~job_parent
          ~borrowed
          ~registry
          ~reference
          ~invocation
          ~authorize
          ~prepare_output
  =
  let t = execution.base in
  let* admitted_child =
    match execution.source with
    | Root_source | Claimed_issuer _ -> Ok None
    | Foreground_child _ -> Result.map (registry ()) ~f:Option.some
  in
  let registry_now () =
    match execution.source, admitted_child with
    | (Root_source | Claimed_issuer _), None -> registry ()
    | Foreground_child child, Some admitted ->
      let* () =
        match borrowed with
        | None -> invalid "child native invocation has no borrowed parent"
        | Some borrowed ->
          Result.map (N.selected_borrowed_capabilities borrowed) ~f:ignore
      in
      let* source = Child_selection.source_current child in
      let* current = reselect source admitted in
      if String.equal (C.fingerprint current) (C.fingerprint admitted)
      then Ok current
      else invalid "child native admission changed after its parent borrow"
    | (Root_source | Claimed_issuer _), Some _ | Foreground_child _, None ->
      invalid "child native admission is inconsistent"
  in
  let* capabilities =
    match execution.source, ceiling with
    | Foreground_child child, None -> Ok (Jobs.Capability_source.Admitted_child child)
    | Claimed_issuer issuer, None -> Ok (Claimed_child issuer)
    | Root_source, Some selected -> Ok (Worker selected)
    | Root_source, None -> Ok Root
    | (Foreground_child _ | Claimed_issuer _), Some _ ->
      invalid "selected native selection cannot borrow a worker ceiling"
  in
  let* entered =
    O.with_native_registration t.root ?job_parent ~invocation (fun ~scope ->
      let execute job_scope =
        let dispatch : D.dispatch_storage =
          { read = O.Native_terminal.read_dispatch scope
          ; save_dispatch = O.Native_terminal.save_dispatch scope
          }
        in
        let effect_entry binding =
          let* witness = O.current_after_mutations t.root in
          let* selected = execution.selected_for_witness witness in
          let reference = C.reference binding in
          let* narrowed = registry_now () in
          let* narrowed_binding =
            C.resolve narrowed ~id:reference.id ~fingerprint:reference.fingerprint
            |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
          in
          let* actual =
            C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
            |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
          in
          let* () =
            match
              ( C.native_implementation binding
              , C.native_implementation narrowed_binding
              , C.native_implementation actual )
            with
            | Some expected, Some narrowed, Some actual
              when phys_equal expected narrowed && phys_equal expected actual -> Ok ()
            | None, None, None -> Ok ()
            | Some _, _, _ | None, _, _ ->
              invalid "selected native implementation changed before effect entry"
          in
          let* () =
            match job_parent with
            | None -> Ok ()
            | Some ancestor ->
              Job_scope.Parent.check
                ancestor
                ~owner:(O.owner t.root)
                ~capture_fingerprint:
                  (Chatml_developer_capture.fingerprint (O.capture t.root))
          in
          (* The parent read above may yield. Resolve the SAME execution
             selection again before the final nonyielding Root/parent gate. *)
          let* current_selected = registry_now () in
          let* current_binding =
            C.resolve current_selected ~id:reference.id ~fingerprint:reference.fingerprint
            |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
          in
          let* () =
            match
              C.native_implementation binding, C.native_implementation current_binding
            with
            | Some expected, Some current when phys_equal expected current -> Ok ()
            | None, None -> Ok ()
            | Some _, _ | None, Some _ ->
              invalid
                "selected native implementation changed before final effect admission"
          in
          let* final_witness, final_selected =
            match execution.source with
            | Root_source ->
              let* fresh = O.current_after_mutations t.root in
              let* () =
                if O.Current.same_selection witness fresh
                then Ok ()
                else invalid "selected native Root selection changed before effect"
              in
              let* selected = execution.selected_for_witness fresh in
              Ok (fresh, selected)
            | Foreground_child child ->
              let* admission = Child_selection.refresh child in
              let* fresh = Child_selection.Admission.checked_root_now admission in
              let* () =
                if O.Current.same_selection witness fresh
                then Ok ()
                else invalid "selected native child Root selection changed before effect"
              in
              (* The invoked binding was checked against [registry_now]'s
                 borrowed ceiling above. An entered managed program borrows its
                 dependency ceiling, which excludes its own handler binding;
                 use this SAME admission's source registry for the final
                 handler comparison, not for selecting nested Tool.call. *)
              Ok (fresh, Child_selection.Admission.source_capabilities admission)
            | Claimed_issuer issuer ->
              let* () = Claimed_runtime.check issuer in
              let* fresh = O.current_after_mutations t.root in
              let* () =
                if O.Current.same_selection witness fresh
                then Ok ()
                else invalid "claimed native Root selection changed before effect"
              in
              let* () = Claimed_runtime.check_now issuer in
              Ok (fresh, Claimed_runtime.selected issuer)
          in
          let* final_binding =
            C.resolve final_selected ~id:reference.id ~fingerprint:reference.fingerprint
            |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
          in
          let* () =
            match
              C.native_implementation binding, C.native_implementation final_binding
            with
            | Some expected, Some final when phys_equal expected final -> Ok ()
            | None, None -> Ok ()
            | Some _, _ | None, Some _ ->
              invalid "selected native implementation changed at final effect admission"
          in
          let* () = O.check_effect_now t.root final_witness in
          let* () =
            match job_parent with
            | None -> Ok ()
            | Some ancestor ->
              Job_scope.Parent.check_now
                ancestor
                ~owner:(O.owner t.root)
                ~capture_fingerprint:
                  (Chatml_developer_capture.fingerprint (O.capture t.root))
          in
          D.check_parent_now execution.issuer ~parent invocation
        in
        let seal outcome ~selected_jobs =
          let physical selection =
            match job_scope with
            | None -> Ok selection
            | Some lexical ->
              (match Jobs.frozen_native lexical ~terminal:scope with
               | Some (frozen, sealed)
                 when P.Invocation.equal_outcome frozen outcome
                      && phys_equal (Jobs.Native_selection.physical sealed) selection ->
                 Ok selection
               | Some _ | None -> invalid "native Job selection lost its lexical seal")
          in
          match O.Native_terminal.sealed scope with
          | Some (frozen, selection) ->
            if
              P.Invocation.equal_outcome frozen outcome
              && List.equal
                   P.Id.Job.equal
                   (List.map
                      (O.Native_terminal.Selection.jobs selection)
                      ~f:P.Developer_job.id)
                   selected_jobs
            then Result.map (physical selection) ~f:Option.some
            else invalid "selected native outcome changed after sealing"
          | None ->
            (match job_scope with
             | None -> O.Native_terminal.seal scope ~outcome ~selected:selected_jobs
             | Some lexical ->
               let* sealed =
                 Jobs.seal_native lexical ~terminal:scope ~outcome ~selected_jobs
               in
               Ok (Jobs.Native_selection.physical sealed))
            |> Result.map ~f:Option.some
        in
        let stage_background binding ~request ~checkpoint =
          let* () = effect_entry binding in
          match job_scope with
          | None ->
            O.Native_terminal.stage_background
              scope
              ~request
              ~checkpoint
              ~check_parent_now:(fun () ->
                D.check_parent_now execution.issuer ~parent invocation)
          | Some lexical -> Jobs.Scope.stage_background lexical ~request ~checkpoint
        in
        let* managed =
          match execution.managed, job_scope with
          | None, _ -> Ok None
          | Some _, None -> invalid "managed callback has no physical Job scope"
          | Some managed, Some lexical ->
            Managed_tool.bind
              managed
              ~terminal:scope
              ~scope:lexical
              ~call_child:(fun ~reference ~invocation ->
                run_nested execution ~reference ~invocation ~authorize ~prepare_output)
            |> Result.map ~f:Option.some
        in
        match
          N.run_owned_selected
            ~admit_managed:(fun execution ->
              match job_scope with
              | None -> Ok ()
              | Some lexical -> Jobs.Scope.admit_managed lexical execution)
            ~install_jobs:(fun prepared config ->
              match job_scope with
              | None -> N.{ config; scoped_async_operation = None }
              | Some lexical -> Jobs.Scope.install_selected lexical ~prepared config)
            ~owner:execution.issuer
            ~parent
            ~dispatch
            ~parent_live_now:(fun () -> O.Native_terminal.effect_parent_live_now scope)
            ~frozen:(fun () ->
              match job_scope with
              | None -> O.Native_terminal.sealed scope
              | Some lexical ->
                Option.map
                  (Jobs.frozen_native lexical ~terminal:scope)
                  ~f:(fun (outcome, selection) ->
                    outcome, Jobs.Native_selection.physical selection))
            ~enter_callback:(fun ~dispatched f ->
              O.with_native_callback scope ~dispatched (fun ~sw:_ -> f ()))
            ~managed
            ~registry:registry_now
            ~reference
            ~invocation
            ~native_invocation
            ~authorize
            ~prepare_output
            ~effect_entry
            ~stage_background
            ~seal
        with
        | Error error -> Ok (`Early_error (scope, error))
        | exception exn ->
          Ok (`Early_raised (scope, exn, Stdlib.Printexc.get_raw_backtrace ()))
        | Ok completion ->
          if O.Native_terminal.callback_complete scope
          then
            Result.map (submit_completion scope completion) ~f:(fun terminal ->
              (match job_scope with
               | None -> ()
               | Some lexical ->
                 (match Jobs.frozen_native lexical ~terminal:scope with
                  | None -> ()
                  | Some (_, selection) ->
                    (match Jobs.acknowledge_native lexical selection with
                     | Ok () -> ()
                     | Error error -> failwith error.message)));
              `Saved terminal)
          else
            let* () =
              O.Native_terminal.remember_failure
                scope
                ~terminal:(D.Completion.terminal completion)
            in
            Ok (`Retained (scope, completion))
      in
      match t.jobs with
      | None -> execute None
      | Some jobs ->
        Jobs.with_native_scope
          jobs
          ~terminal:scope
          ~borrowed
          ~capabilities
          ~check_parent_now:(fun () ->
            D.check_parent_now execution.issuer ~parent invocation)
          (fun ~scope:lexical -> execute (Some lexical)))
  in
  match entered with
  | `Saved terminal -> Ok terminal
  | `Retained (scope, completion) -> recover_completion scope completion
  | `Early_error (scope, error) ->
    (match Eio.Cancel.protect (fun () -> recover_early scope) with
     | Ok () -> Error error
     | Error _ -> invalid "pre-dispatch cleanup remains uncertain")
  | `Early_raised (scope, exn, backtrace) ->
    (try
       ignore (Eio.Cancel.protect (fun () -> recover_early scope) : (unit, _) result)
     with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace

and run_nested execution ~reference ~invocation ~authorize ~prepare_output =
  let* borrowed = N.borrow_selected () in
  if not (D.same_issuer execution.issuer (N.selected_borrowed_issuer borrowed))
  then invalid "selected child belongs to another native executor"
  else (
    let parent = N.selected_borrowed_parent borrowed in
    let* job_parent = D.inherited_job_parent execution.issuer parent in
    run
      execution
      ~parent:(D.Child parent)
      ~job_parent
      ~borrowed:(Some borrowed)
      ~registry:(fun () -> narrowed_registry execution borrowed)
      ~reference
      ~invocation
      ~authorize
      ~prepare_output)
;;

let run_child t ~reference ~invocation ~authorize ~prepare_output =
  run_nested (root_execution t) ~reference ~invocation ~authorize ~prepare_output
;;

let run_root t ~reference ~invocation ~authorize ~prepare_output =
  run
    (root_execution t)
    ~parent:D.Root
    ~job_parent:None
    ~borrowed:None
    ~registry:(fun () -> D.selected t.issuer)
    ~reference
    ~invocation
    ~authorize
    ~prepare_output
;;

let run_job_child ?selected t ~active ~reference ~invocation ~authorize ~prepare_output =
  let* job_parent = O.job_parent t.root active in
  run
    ?ceiling:selected
    (root_execution t)
    ~parent:(D.Job_child job_parent)
    ~job_parent:(Some job_parent)
    ~borrowed:None
    ~registry:(fun () ->
      match selected with
      | None -> D.selected t.issuer
      | Some selected -> reselect_current t selected)
    ~reference
    ~invocation
    ~authorize
    ~prepare_output
;;

module Claimed = struct
  type parent = t

  type t =
    { execution : execution
    ; issuer : Claimed_runtime.t
    ; job_parent : Job_scope.Parent.t
    ; live : bool ref
    }

  let with_issuer ~parent ~active ~issuer f =
    let* job_parent = O.job_parent parent.root active in
    let* () =
      if Claimed_runtime.same_parent issuer job_parent
      then Ok ()
      else invalid "claimed native issuer belongs to another Worker Claim"
    in
    let* () = Claimed_runtime.check issuer in
    let* () = Result.map (O.current_after_mutations parent.root) ~f:ignore in
    let owner = O.owner parent.root in
    let capture_fingerprint =
      Chatml_developer_capture.fingerprint (O.capture parent.root)
    in
    let live = ref true in
    let check_live () =
      if !live
      then Claimed_runtime.check issuer
      else invalid "claimed native execution expired"
    in
    let* empty_after_expiry =
      C.select (Claimed_runtime.selected issuer) ~names:[]
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
    in
    let current ~owner:claimed_owner ~capture_fingerprint:claimed_fingerprint =
      if
        (not (P.Execution_owner.equal_developer owner claimed_owner))
        || not (Chatml_source.Digest.equal capture_fingerprint claimed_fingerprint)
      then invalid "claimed native issuer differs from its physical Root"
      else
        let* () = check_live () in
        Result.map (O.current_after_mutations parent.root) ~f:ignore
    in
    let* invocation_issuer =
      D.create
        ~owner
        ~capture_fingerprint
        ~ceiling:(Claimed_runtime.selected issuer)
        ~selected:(fun () ->
          if !live then Claimed_runtime.selected issuer else empty_after_expiry)
        ~current
        ~now:(fun () -> O.native_now parent.root)
    in
    let* managed =
      match parent.jobs, parent.managed_env with
      | Some jobs, Some env ->
        Managed_tool.create_claimed ~root:parent.root ~jobs ~active ~issuer ~env
        |> Result.map ~f:Option.some
      | None, None -> Ok None
      | Some _, None | None, Some _ ->
        invalid "claimed native execution has incomplete physical Job resources"
    in
    let execution =
      { base = parent
      ; issuer = invocation_issuer
      ; managed
      ; source = Claimed_issuer issuer
      ; selected_current =
          (fun () ->
            let* () = check_live () in
            Ok (Claimed_runtime.selected issuer))
      ; selected_for_witness =
          (fun _ ->
            let* () = check_live () in
            Ok (Claimed_runtime.selected issuer))
      }
    in
    let* () = Claimed_runtime.check_now issuer in
    let result =
      Exn.protect
        ~f:(fun () -> f { execution; issuer; job_parent; live })
        ~finally:(fun () -> live := false)
    in
    let* value = result in
    let* () = Claimed_runtime.check issuer in
    Ok value
  ;;

  let run t ~(reference : C.reference) ~invocation ~authorize ~prepare_output =
    let* () = if !(t.live) then Ok () else invalid "claimed native execution expired" in
    let* () = Claimed_runtime.check t.issuer in
    let pinned = Claimed_runtime.pinned_capabilities t.issuer in
    let* _binding =
      C.resolve pinned ~id:reference.id ~fingerprint:reference.fingerprint
      |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
    in
    let* () =
      if String.equal (H.context invocation).capability_fingerprint (C.fingerprint pinned)
      then Ok ()
      else invalid "claimed native invocation differs from its pinned request"
    in
    run
      t.execution
      ~parent:(D.Job_child t.job_parent)
      ~job_parent:(Some t.job_parent)
      ~borrowed:None
      ~registry:(fun () ->
        let* () = t.execution.selected_current () |> Result.map ~f:ignore in
        Ok (Claimed_runtime.pinned_capabilities t.issuer))
      ~reference
      ~invocation
      ~authorize
      ~prepare_output
  ;;
end

module Child = struct
  type parent = t

  type t =
    { execution : execution
    ; selection : Child_selection.t
    }

  let with_selection ~parent ~selection f =
    let* managed =
      match parent.jobs, parent.managed_env with
      | None, None -> Ok None
      | Some jobs, Some env ->
        Managed_tool.create_child ~root:parent.root ~jobs ~selection ~env
        |> Result.map ~f:Option.some
      | None, Some _ | Some _, None ->
        invalid "child managed execution has incomplete physical Job resources"
    in
    let selected_current () =
      let* admission = Child_selection.refresh selection in
      let* () = Child_selection.Admission.check_now admission in
      Ok (Child_selection.Admission.capabilities admission)
    in
    let execution =
      { base = parent
      ; issuer = Child_selection.issuer selection
      ; managed
      ; source = Foreground_child selection
      ; selected_current
      ; selected_for_witness = (fun _ -> Child_selection.source_current selection)
      }
    in
    f { execution; selection }
  ;;

  let with_target ~parent ~target ~prepared f =
    let* borrowed = N.borrow_selected () in
    let* selection =
      Child_selection.create ~root:parent.root ~target ~prepared ~parent:borrowed
    in
    with_selection ~parent ~selection f
  ;;

  let with_content_target ~parent ~target ~prepared f =
    let* borrowed = N.borrow_selected () in
    let* selection =
      Child_selection.create_content
        ~root:parent.execution.base.root
        ~parent:parent.selection
        ~target
        ~prepared
        ~borrowed
    in
    with_selection ~parent:parent.execution.base ~selection f
  ;;

  let with_model_target ~parent ~target ~prepared f =
    let* borrowed = N.borrow_selected () in
    let* selection =
      Child_selection.create_model
        ~root:parent.execution.base.root
        ~parent:parent.selection
        ~target
        ~prepared
        ~borrowed
    in
    with_selection ~parent:parent.execution.base ~selection f
  ;;

  let current_capabilities t = t.execution.selected_current ()

  let run t ~reference ~invocation ~native_invocation ~authorize ~prepare_output =
    let* borrowed = Child_selection.first_borrowed t.selection in
    let* job_parent =
      D.inherited_job_parent
        (N.selected_borrowed_issuer borrowed)
        (N.selected_borrowed_parent borrowed)
    in
    run
      ~native_invocation
      t.execution
      ~parent:(Child_selection.first_parent t.selection)
      ~job_parent
      ~borrowed:(Some borrowed)
      ~registry:t.execution.selected_current
      ~reference
      ~invocation
      ~authorize
      ~prepare_output
  ;;
end
