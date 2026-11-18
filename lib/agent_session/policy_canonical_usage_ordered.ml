open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Policy_canonical_usage_data
module Cause = Policy_cause_data
module Member = Cause.Member
module Work = Cause.Allocation_work
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Context = Cause.Context
module L = Inference_ledger
module F = Canonical_usage_fold
module Canonical = Goal_descendants.Canonical
module Occurrence = Goal_descendant_occurrence
module U = Goal_usage
open Policy_canonical_usage_carrier
open Policy_canonical_usage_selection
open Policy_canonical_usage_update

type t = Policy_canonical_usage_carrier.t

module Update = Policy_canonical_usage_update.Update

let context_native (context : Context.t) ~accounts ~catalog ~descendants =
  Context.create
    ~session_id:context.session_id
    ~generation:context.generation
    ~current_source:context.current_source
    ~events:context.events
    ~invocations:context.invocations
    ~jobs:context.jobs
    ~runs:context.runs
    ~accounts
    ~catalog
    ~descendants
;;

let context_json (context : Context.t) =
  let%map.Result basis = Basis.capture context in
  `Object
    [ "session", P.Id.Session.to_json context.session_id
    ; "generation", X.host_counter_to_json context.generation
    ; "source", X.option_json P.Run_source.to_json context.current_source
    ; "events", `Array (List.map context.events ~f:P.Moderator_execution.to_json)
    ; "invocations", `Array (List.map context.invocations ~f:P.Invocation.to_json)
    ; "jobs", `Array (List.map context.jobs ~f:P.Job.to_json)
    ; "runs", X.option_json Run_state.to_jsonaf context.runs
    ; "native", Basis.to_json basis
    ]
;;

let members_in_context (context : Context.t) =
  let open Result.Let_syntax in
  let%bind events =
    List.map context.events ~f:(fun event ->
      Member.event
        event.P.Moderator_execution.context.id
        ~generation:event.context.generation)
    |> Result.all
  in
  let%bind invocations =
    List.map context.invocations ~f:(fun invocation ->
      Member.invocation
        invocation.P.Invocation.context.id
        ~generation:invocation.context.generation)
    |> Result.all
  in
  let%map jobs =
    List.filter_map context.jobs ~f:(fun job ->
      match job.P.Job.kind with
      | Model_call | Async_tool -> Some (Policy_native_ancestry.member_of_job job)
      | Nested_agent | Scheduled_event | Shell_process | Compaction -> None)
    |> Result.all
  in
  events @ invocations @ jobs
;;

module Ordered = struct
  type accounting = t

  type birth =
    { member : Member.t
    ; before : Context.t
    ; after : Context.t
    }

  type pending =
    { selected : Selection.t
    ; accounting : accounting
    ; live_prefix : Context.t
    ; before_ledger : L.t
    ; after_ledger : L.t
    }

  type t =
    { accounting : accounting option
    ; context : Context.t
    ; ledger : L.t
    ; births : birth list option
    ; pending : pending option
    ; new_facts : Canonical.Fact.t list
    }

  let begin_ ~previous ~context ~ledger =
    let%map.Result () =
      Option.value_map previous ~default:(Ok ()) ~f:(fun accounting ->
        validate accounting ~context ~ledger)
    in
    { accounting = previous
    ; context
    ; ledger
    ; births = Some []
    ; pending = None
    ; new_facts = []
    }
  ;;

  let begin_absent ~context ~ledger =
    let%map.Result t = begin_ ~previous:None ~context ~ledger in
    { t with births = None }
  ;;

  let seal t =
    match t.pending with
    | None -> Ok t
    | Some pending ->
      let open Result.Let_syntax in
      let%bind basis = Basis.capture t.context in
      if not (Basis.equal pending.selected.basis basis)
      then invalid "policy canonical coverage basis has not appeared at this exact prefix"
      else (
        let%map () =
          Selection.verify
            pending.selected
            pending.accounting
            ~live_prefix:pending.live_prefix
            ~coverage_after:t.context
            ~before_ledger:pending.before_ledger
            ~after_ledger:pending.after_ledger
        in
        { t with pending = None })
  ;;

  let seal_if_ready t =
    match t.pending with
    | None -> Ok t
    | Some pending ->
      let%bind.Result basis = Basis.capture t.context in
      if Basis.equal pending.selected.basis basis then seal t else Ok t
  ;;

  let same_accounting left right =
    match left, right with
    | None, None -> Ok true
    | Some left, Some right -> equal_encoded left right
    | None, Some _ | Some _, None -> Ok false
  ;;

  let adopt_update expected update =
    let incoming = Update.candidate update in
    if not (Data.equal (value expected) (value incoming))
    then invalid "policy Update candidate differs from its actual closed reason"
    else (
      let%bind.Result candidate =
        D.Domain_codec.adopt codec ~previous:expected ~incoming |> domain
      in
      admit candidate)
  ;;

  let birth t member =
    Result.of_option
      (Option.bind t.births ~f:(fun births ->
         List.find births ~f:(fun birth -> Member.equal birth.member member)))
      ~error:
        (P.Error.invalid_request
           "policy registration has no actual missing-to-present ordered birth")
  ;;

  let metadata t update =
    let open Result.Let_syntax in
    let%bind t = seal t in
    let%bind () =
      match t.births with
      | None -> invalid "absence-only policy cursor cannot admit metadata"
      | Some _ -> Ok ()
    in
    let%bind expected =
      match update.Update.reason with
      | Register_event cause ->
        let%bind member =
          Member.event
            (Cause.parent_event cause)
            ~generation:(Cause.callee_source cause).generation
        in
        let%bind birth = birth t member in
        register_event
          t.accounting
          ~before:birth.before
          ~after:birth.after
          ~ledger:t.ledger
          ~cause
      | Register_work { cause; member } ->
        let%bind accounting =
          Result.of_option
            t.accounting
            ~error:(P.Error.invalid_request "policy work cannot precede its seed")
        in
        let%bind birth = birth t member in
        register_work accounting ~before:birth.before ~after:birth.after ~cause ~member
      | Prune_work ->
        let%bind accounting =
          Result.of_option
            t.accounting
            ~error:(P.Error.invalid_request "absent policy component cannot prune work")
        in
        prune_work accounting ~context:t.context
      | Reconcile _ -> invalid "policy canonical Update requires actual ledger edge"
    in
    let%bind accounting = adopt_update (Update.candidate expected) update in
    let%bind () = validate accounting ~context:t.context ~ledger:t.ledger in
    let consumed =
      match update.reason with
      | Register_event cause ->
        Member.event
          (Cause.parent_event cause)
          ~generation:(Cause.callee_source cause).generation
        |> Result.map ~f:Option.some
      | Register_work { member; _ } -> Ok (Some member)
      | Prune_work -> Ok None
      | Reconcile _ -> assert false
    in
    let%map consumed = consumed in
    { t with
      accounting = Some accounting
    ; births =
        Option.map t.births ~f:(fun births ->
          List.filter births ~f:(fun birth ->
            not (Option.exists consumed ~f:(Member.equal birth.member))))
    }
  ;;

  let canonical t ~before ~after ~update =
    let open Result.Let_syntax in
    let%bind t = seal t in
    let%bind same = same_ledger t.ledger before in
    let%bind () =
      if same then Ok () else invalid "policy ordered canonical before-ledger differs"
    in
    let%bind accounting =
      Result.of_option
        t.accounting
        ~error:(P.Error.invalid_request "policy canonical Update has no component")
    in
    let%bind selection =
      match update.Update.reason with
      | Reconcile selected -> Ok selected
      | Register_event _ | Register_work _ | Prune_work ->
        invalid "policy canonical edge has metadata-only reason"
    in
    let%bind () =
      match selection with
      | None -> Ok ()
      | Some selected ->
        let%bind cause =
          Result.of_option
            (Data.find_cause (value accounting) selected.cause)
            ~error:(P.Error.invalid_request "policy canonical cause absent")
        in
        let%bind () = Canonical.validate_fact ~before ~after ~fact:selected.fact in
        Selection.check_work
          (value accounting)
          ~cause
          ~work:selected.work
          ~fact:selected.fact
          t.context
    in
    let%bind expected = reconcile accounting ~before ~after ~selection in
    let%bind candidate = adopt_update (Update.candidate expected) update in
    let pending =
      Option.map selection ~f:(fun selected ->
        { selected
        ; accounting
        ; live_prefix = t.context
        ; before_ledger = before
        ; after_ledger = after
        })
    in
    let new_facts =
      t.new_facts
      @ Option.to_list (Option.map selection ~f:(fun selected -> selected.Selection.fact))
    in
    seal_if_ready
      { t with accounting = Some candidate; ledger = after; pending; new_facts }
  ;;

  let finish_work t context =
    let open Result.Let_syntax in
    let%bind births =
      match t.births with
      | None -> Ok None
      | Some births ->
        let%bind before_members = members_in_context t.context in
        let%bind after_members = members_in_context context in
        let new_members =
          List.filter after_members ~f:(fun member ->
            not (List.mem before_members member ~equal:Member.equal))
        in
        let%bind () =
          match t.accounting with
          | None -> Ok ()
          | Some accounting ->
            List.fold_result new_members ~init:() ~f:(fun () member ->
              if Option.is_none (Data.find_member (value accounting) member)
              then Ok ()
              else
                invalid
                  "historical policy source-work witness cannot become fresh work again")
        in
        let births =
          births
          @ List.map new_members ~f:(fun member ->
            { member; before = t.context; after = context })
        in
        let%bind () =
          if List.length births <= max_memberships
          then Ok ()
          else invalid "policy ordered birth window exceeds membership bound"
        in
        Ok (Some births)
    in
    let%map () =
      Option.value_map t.accounting ~default:(Ok ()) ~f:(fun accounting ->
        validate accounting ~context ~ledger:t.ledger)
    in
    { t with context; births }
  ;;

  type step =
    | Source_work of Context.t
    | Lifecycle_work of
        { context : Context.t
        ; receipt : P.Moderator_execution.t
        ; capture : Moderator_lifecycle.t option
        }
    | Source_changed of
        { current_source : P.Run_source.t option
        ; runs : Run_state.t option
        }
    | Accounts of Goal_accounts.t option
    | Descendants of Goal_descendants.t option
    | Catalog of Goal_catalog.t option
    | Metadata of Update.t
    | Canonical of
        { before : L.t
        ; after : L.t
        ; update : Update.t
        }
    | Ordinary_canonical of
        { before : L.t
        ; after : L.t
        }
    | Replacement of
        { context : Context.t
        ; ledger : L.t
        ; accounting : accounting option
        }

  let advance t = function
    | Metadata update -> metadata t update
    | Canonical { before; after; update } -> canonical t ~before ~after ~update
    | Source_changed { current_source; runs } ->
      let open Result.Let_syntax in
      let%bind t =
        if
          Option.equal P.Run_source.equal t.context.current_source current_source
          && Option.equal Run_state.equal t.context.runs runs
        then Ok t
        else seal t
      in
      let old = t.context in
      let%bind context =
        Context.create
          ~session_id:old.session_id
          ~generation:old.generation
          ~current_source
          ~events:old.events
          ~invocations:old.invocations
          ~jobs:old.jobs
          ~runs
          ~accounts:old.accounts
          ~catalog:old.catalog
          ~descendants:old.descendants
      in
      let%map () =
        Option.value_map t.accounting ~default:(Ok ()) ~f:(fun accounting ->
          validate accounting ~context ~ledger:t.ledger)
      in
      let births =
        if Option.equal P.Run_source.equal old.current_source current_source
        then t.births
        else Option.map t.births ~f:(fun _ -> [])
      in
      { t with context; births }
    | Lifecycle_work { context; receipt; capture } ->
      let open Result.Let_syntax in
      let module E = P.Moderator_execution in
      let%bind t = seal t in
      let%bind basis = Basis.capture context in
      let%bind old_basis = Basis.capture t.context in
      let%bind () =
        if
          Basis.equal basis old_basis
          && P.Id.Session.equal t.context.session_id context.session_id
          && Int.equal t.context.generation context.generation
          && Option.equal
               P.Run_source.equal
               t.context.current_source
               context.current_source
        then Ok ()
        else invalid "policy lifecycle-work step changes native/source basis"
      in
      let%bind () =
        if
          Moderator_lifecycle.lifecycle_phase receipt.context.phase
          && P.Id.Session.equal receipt.context.session_id context.session_id
          && Int.equal receipt.context.generation context.generation
          && (not
                (List.exists t.context.events ~f:(fun previous ->
                   P.Id.Moderator_execution.equal previous.E.context.id receipt.context.id)))
          && List.equal E.equal context.events (receipt :: t.context.events)
          && List.equal P.Invocation.equal context.invocations t.context.invocations
          && List.equal
               (fun left right ->
                  Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right))
               context.jobs
               t.context.jobs
        then E.validate_transition ~previous:None receipt
        else invalid "policy lifecycle-work requires one exact new lifecycle receipt"
      in
      let%bind runs =
        match capture with
        | None -> Ok (Option.map t.context.runs ~f:Run_state.clear_moderator_lifecycle)
        | Some capture ->
          let%bind () =
            if
              P.Id.Moderator_execution.equal
                receipt.context.id
                (Moderator_lifecycle.execution_id capture)
            then Ok ()
            else invalid "policy lifecycle-work capture selects another receipt"
          in
          let%bind current_source =
            Result.of_option
              context.current_source
              ~error:
                (P.Error.invalid_request "policy lifecycle-work has no current source")
          in
          let%bind () =
            Moderator_lifecycle.validate
              capture
              ~session_id:context.session_id
              ~generation:context.generation
              ~current_source
              ~executions:context.events
          in
          let%bind index =
            Result.of_option
              t.context.runs
              ~error:
                (P.Error.invalid_request "policy lifecycle-work has no installed index")
          in
          let%map index = Run_state.capture_moderator_lifecycle index capture in
          Some index
      in
      let%bind () =
        if Option.equal Run_state.equal runs context.runs
        then Ok ()
        else invalid "policy lifecycle-work changes Run state outside its exact pointer"
      in
      finish_work t context
    | Source_work context ->
      let open Result.Let_syntax in
      let%bind t = seal t in
      let%bind basis = Basis.capture context in
      let%bind old_basis = Basis.capture t.context in
      let%bind () =
        if
          Basis.equal basis old_basis
          && P.Id.Session.equal t.context.session_id context.session_id
          && Int.equal t.context.generation context.generation
          && Option.equal
               P.Run_source.equal
               t.context.current_source
               context.current_source
          && Option.equal Run_state.equal t.context.runs context.runs
        then Ok ()
        else invalid "policy source-work step changes native/source basis"
      in
      finish_work t context
    | Accounts accounts ->
      let open Result.Let_syntax in
      let%bind context =
        context_native
          t.context
          ~accounts
          ~catalog:t.context.catalog
          ~descendants:t.context.descendants
      in
      let%bind () =
        match t.pending with
        | None -> Ok ()
        | Some pending ->
          let%bind basis = Basis.capture context in
          if Option.equal String.equal basis.accounts pending.selected.basis.accounts
          then Ok ()
          else invalid "policy pending accounts basis differs at actual ordered leaf"
      in
      seal_if_ready { t with context }
    | Descendants descendants ->
      let open Result.Let_syntax in
      let%bind context =
        context_native
          t.context
          ~accounts:t.context.accounts
          ~catalog:t.context.catalog
          ~descendants
      in
      let%bind () =
        match t.pending with
        | None -> Ok ()
        | Some pending ->
          let%bind basis = Basis.capture context in
          if
            Option.equal String.equal basis.descendants pending.selected.basis.descendants
          then Ok ()
          else invalid "policy pending descendant basis differs at actual ordered leaf"
      in
      seal_if_ready { t with context }
    | Catalog catalog ->
      let open Result.Let_syntax in
      let%bind context =
        context_native
          t.context
          ~accounts:t.context.accounts
          ~catalog
          ~descendants:t.context.descendants
      in
      let%bind () =
        match t.pending with
        | None -> Ok ()
        | Some pending ->
          let%bind basis = Basis.capture context in
          if Option.equal String.equal basis.catalog pending.selected.basis.catalog
          then Ok ()
          else invalid "policy pending catalog basis differs at actual ordered leaf"
      in
      seal_if_ready { t with context }
    | Ordinary_canonical { before; after } ->
      let open Result.Let_syntax in
      let%bind t = seal t in
      let%bind same = same_ledger t.ledger before in
      let%bind () =
        if same then Ok () else invalid "ordinary canonical before-ledger differs"
      in
      (match t.accounting with
       | Some _ -> invalid "positive policy component requires a paired canonical Update"
       | None ->
         let%map () = L.validate_update before ~incoming:after |> Data.ledger in
         { t with ledger = after })
    | Replacement { context; ledger; accounting } ->
      let open Result.Let_syntax in
      let%bind t = seal t in
      let%bind accounting =
        match t.accounting, accounting with
        | None, None ->
          let%map () = L.validate_update t.ledger ~incoming:ledger |> Data.ledger in
          None
        | Some previous, Some incoming ->
          let%bind expected = reconcile_factual previous ~before:t.ledger ~after:ledger in
          let%bind () =
            if Data.equal (value expected) (value incoming)
            then Ok ()
            else invalid "policy replacement differs from factual reconciliation"
          in
          let%map candidate = adopt expected ~incoming in
          Some candidate
        | None, Some _ -> invalid "replacement cannot initialize policy causes"
        | Some _, None -> invalid "replacement discarded policy accounting"
      in
      let%map () =
        Option.value_map accounting ~default:(Ok ()) ~f:(fun accounting ->
          validate accounting ~context ~ledger)
      in
      { t with
        context
      ; ledger
      ; accounting
      ; births = Option.map t.births ~f:(fun _ -> [])
      }
  ;;

  let validate_publication t ~revision =
    List.fold_result t.new_facts ~init:() ~f:(fun () fact ->
      let frame = Occurrence.frame (Canonical.Fact.occurrence fact) in
      if Int64.equal revision (Occurrence.Frame.candidate_session_revision frame)
      then Ok ()
      else invalid "policy canonical fact differs from actual transaction publication")
  ;;

  let check t ~incoming ~context ~ledger =
    let open Result.Let_syntax in
    let%bind t = seal t in
    let%bind same = same_ledger t.ledger ledger in
    let%bind same_accounting = same_accounting t.accounting incoming in
    let%bind expected_context = context_json t.context in
    let%bind actual_context = context_json context in
    if same && same_accounting && D.Json.equal expected_context actual_context
    then
      Option.value_map incoming ~default:(Ok ()) ~f:(fun accounting ->
        validate accounting ~context ~ledger)
    else invalid "policy ordered final candidate differs from actual last prefix"
  ;;
end
