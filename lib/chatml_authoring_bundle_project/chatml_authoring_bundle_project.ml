open Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Project = Chatml_project
module Prepared = Chatml_prepared_program
module Wire_source = Agent_protocol.Authoring_common.Source
module Result_check = Authoring_bundle_project_result

module Limits = struct
  type t =
    { max_work_steps : int
    ; max_retained_input_bytes : int
    ; document : Document_schema.Limits.t
    }

  let create ~max_work_steps ~max_retained_input_bytes ~document =
    if max_work_steps <= 0 || max_retained_input_bytes <= 0
    then Or_error.error_string "bundle project limits must be positive"
    else Ok { max_work_steps; max_retained_input_bytes; document }
  ;;
end

module Error = struct
  type problem =
    | Snapshot_mismatch
    | Missing_or_surplus_member
    | Wrong_project_basis
    | Missing_selected_script
    | Derived_entry_mismatch
    | Previous_lock_mismatch
    | Unexpected_previous_build
    | Result_mismatch
    | Work_limit
    | Input_limit
    | Accounting_overflow
    | Attempt_closed
    | Attempt_busy
    | Builder of Build.Error.t
    | Project_declaration of Project.Error.t

  type t = { problem : problem }

  let problem t = t.problem
  let create problem = { problem }
end

module Validated = struct
  type t =
    { manifest_fingerprint : Captured.Digest.t
    ; root_source : Captured.Id.t
    ; build : Build.t
    ; input_fingerprint : Captured.Digest.t
    ; selected_input_sources : Captured.t list
    }

  let manifest_fingerprint t = t.manifest_fingerprint
  let root_source t = t.root_source
  let build t = t.build
  let input_fingerprint t = t.input_fingerprint
  let selected_input_sources t = t.selected_input_sources
end

exception Work_exhausted
exception Input_exhausted
exception Invalid_stage_input
exception Expired_stage
exception Concurrent_stage

let error problem = Error (Error.create problem)

let source_matches_metadata (source : Captured.t) (metadata : Wire_source.t) =
  Captured.Id.equal (Captured.id source) (Wire_source.id metadata)
  && Captured.Digest.equal (Captured.digest source) (Wire_source.digest metadata)
  && Int.equal (Captured.byte_length source) (Wire_source.byte_length metadata)
;;

let source_matches_member source member =
  source_matches_metadata source (Bundle.Member.source member)
;;

module Attempt = struct
  type t =
    { manifest : Bundle.Manifest.t
    ; sources : Captured.t Map.M(Captured.Id).t
    ; limits : Limits.t
    ; mutable work_left : int
    ; mutable input_left : int
    ; closed : bool Atomic.t
    ; active : bool Atomic.t
    ; creator_domain : int
    }

  let manifest_fingerprint t = Bundle.Manifest.fingerprint t.manifest

  let tick t ~checkpoint =
    if Atomic.get t.closed then raise Expired_stage;
    if t.work_left <= 0 then raise Work_exhausted;
    t.work_left <- t.work_left - 1;
    checkpoint ()
  ;;

  let debit_input t bytes =
    if bytes < 0
    then error Accounting_overflow
    else if bytes > t.input_left
    then error Input_limit
    else (
      t.input_left <- t.input_left - bytes;
      Ok ())
  ;;

  let bounded_source_id id =
    String.length (Captured.Id.package_namespace id) <= 1024
    && String.length (Captured.Id.relative_path id) <= 1024
  ;;

  let map_snapshot manifest sources ~limits ~checkpoint =
    let budget =
      { manifest
      ; sources = Map.empty (module Captured.Id)
      ; limits
      ; work_left = limits.Limits.max_work_steps
      ; input_left = limits.max_retained_input_bytes
      ; closed = Atomic.make false
      ; active = Atomic.make false
      ; creator_domain = (Stdlib.Domain.self () :> int)
      }
    in
    let open Result.Let_syntax in
    let%bind members =
      List.fold_result
        (Bundle.Manifest.members manifest)
        ~init:(Map.empty (module Captured.Id))
        ~f:(fun members member ->
          tick budget ~checkpoint;
          let id = Wire_source.id (Bundle.Member.source member) in
          if Map.mem members id
          then error Snapshot_mismatch
          else Ok (Map.set members ~key:id ~data:member))
    in
    let%bind actual, count =
      List.fold_result
        sources
        ~init:(Map.empty (module Captured.Id), 0)
        ~f:(fun (map, count) source ->
          tick budget ~checkpoint;
          if count >= Bundle.Wire_limits.max_members
          then error Missing_or_surplus_member
          else (
            let id = Captured.id source in
            if not (bounded_source_id id)
            then error Snapshot_mismatch
            else if Map.mem map id
            then error Snapshot_mismatch
            else (
              match Map.find members id with
              | None -> error Missing_or_surplus_member
              | Some member ->
                if not (source_matches_member source member)
                then error Snapshot_mismatch
                else (
                  let%map () = debit_input budget (Captured.byte_length source) in
                  Map.set map ~key:id ~data:source, count + 1))))
    in
    if not (Int.equal count (Map.length members))
    then error Missing_or_surplus_member
    else (
      match Bundle.Manifest.basis manifest with
      | Chatmd_document _ -> Ok { budget with sources = actual }
      | Project project ->
        let subset = Bundle.Manifest.Project.source_members project in
        if not (Int.equal (List.length subset) count)
        then error Missing_or_surplus_member
        else Ok { budget with sources = actual })
  ;;

  let admit manifest ~sources ~limits ~checkpoint =
    try map_snapshot manifest sources ~limits ~checkpoint with
    | Work_exhausted -> error Work_limit
  ;;

  let with_open t ~(error_of_problem : Error.problem -> 'error) ~f =
    if not (Int.equal t.creator_domain (Stdlib.Domain.self () :> int))
    then (
      Atomic.set t.closed true;
      Error (error_of_problem Attempt_busy))
    else if Atomic.get t.closed
    then Error (error_of_problem Attempt_closed)
    else if not (Atomic.compare_and_set t.active false true)
    then (
      Atomic.set t.closed true;
      Error (error_of_problem Attempt_busy))
    else if Atomic.get t.closed
    then (
      Atomic.set t.active false;
      Error (error_of_problem Attempt_closed))
    else (
      let result =
        try f () with
        | Work_exhausted -> Error (error_of_problem Work_limit)
        | Input_exhausted -> Error (error_of_problem Input_limit)
        | Invalid_stage_input -> Error (error_of_problem Accounting_overflow)
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          Atomic.set t.closed true;
          Atomic.set t.active false;
          Stdlib.Printexc.raise_with_backtrace exn backtrace
      in
      let invalidated_during_call = Atomic.get t.closed in
      (match result with
       | Ok _ -> ()
       | Error _ -> Atomic.set t.closed true);
      let result =
        if invalidated_during_call && Result.is_ok result
        then Error (error_of_problem Attempt_closed)
        else result
      in
      Atomic.set t.active false;
      result)
  ;;

  module Stage = struct
    type attempt = t

    type t =
      { owner : attempt
      ; checkpoint_callback : unit -> unit
      ; live : bool Atomic.t
      ; in_use : bool Atomic.t
      }

    let with_live stage ~f =
      let owner = stage.owner in
      if not (Int.equal owner.creator_domain (Stdlib.Domain.self () :> int))
      then (
        Atomic.set owner.closed true;
        raise Concurrent_stage);
      if
        (not (Atomic.get stage.live))
        || (not (Atomic.get owner.active))
        || Atomic.get owner.closed
      then (
        Atomic.set owner.closed true;
        raise Expired_stage);
      if not (Atomic.compare_and_set stage.in_use false true)
      then (
        Atomic.set owner.closed true;
        raise Concurrent_stage);
      Exn.protect
        ~f:(fun () ->
          try
            if
              (not (Atomic.get stage.live))
              || (not (Atomic.get owner.active))
              || Atomic.get owner.closed
            then raise Expired_stage;
            let value = f () in
            if
              (not (Atomic.get stage.live))
              || (not (Atomic.get owner.active))
              || Atomic.get owner.closed
            then raise Expired_stage;
            value
          with
          | exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            Atomic.set owner.closed true;
            Stdlib.Printexc.raise_with_backtrace exn backtrace)
        ~finally:(fun () -> Atomic.set stage.in_use false)
    ;;

    let checkpoint stage =
      with_live stage ~f:(fun () ->
        tick stage.owner ~checkpoint:stage.checkpoint_callback)
    ;;

    let debit_input stage ~bytes =
      with_live stage ~f:(fun () ->
        if bytes < 0 then raise Invalid_stage_input;
        if bytes > stage.owner.input_left then raise Input_exhausted;
        stage.owner.input_left <- stage.owner.input_left - bytes)
    ;;

    let source stage id =
      with_live stage ~f:(fun () ->
        tick stage.owner ~checkpoint:stage.checkpoint_callback;
        Map.find stage.owner.sources id)
    ;;
  end

  type 'stage_error stage_failure =
    | Stage_error of 'stage_error
    | Budget_error of Error.t

  let run_stage t ~checkpoint ~f =
    with_open
      t
      ~error_of_problem:(fun problem -> Budget_error (Error.create problem))
      ~f:(fun () ->
        let stage : Stage.t =
          { owner = t
          ; checkpoint_callback = checkpoint
          ; live = Atomic.make true
          ; in_use = Atomic.make false
          }
        in
        Exn.protect
          ~f:(fun () -> Result.map_error (f stage) ~f:(fun error -> Stage_error error))
          ~finally:(fun () -> Atomic.set stage.live false))
  ;;

  let find_source t id ~checkpoint =
    tick t ~checkpoint;
    match Map.find t.sources id with
    | Some source -> Ok source
    | None -> error Missing_or_surplus_member
  ;;

  let selected_inputs t project ~derived_entry ~checkpoint =
    let open Result.Let_syntax in
    let%bind sources =
      List.fold_result
        (Bundle.Manifest.Project.source_members project)
        ~init:[]
        ~f:(fun sources id ->
          let%map source = find_source t id ~checkpoint in
          source :: sources)
      |> Result.map ~f:List.rev
    in
    let%bind entry, extra =
      match Bundle.Manifest.Project.entry project, derived_entry with
      | Declared, None -> Ok (Build.Entry.Declared, None)
      | Declared, Some _ -> error Derived_entry_mismatch
      | Captured captured, supplied ->
        let metadata = Bundle.Manifest.Project.Captured_entry.source captured in
        (match Bundle.Manifest.Project.Captured_entry.delivery captured, supplied with
         | Uploaded_member, None ->
           let%map source = find_source t (Wire_source.id metadata) ~checkpoint in
           Build.Entry.Captured source, None
         | Derived_script, Some source when source_matches_metadata source metadata ->
           tick t ~checkpoint;
           Ok (Build.Entry.Captured source, Some source)
         | Uploaded_member, Some _ | Derived_script, None | Derived_script, Some _ ->
           error Derived_entry_mismatch)
    in
    Ok (sources, entry, extra)
  ;;

  let selection_request = Result_check.selection_request

  let remaining_build_limits t =
    if t.work_left <= 0
    then error Work_limit
    else if t.input_left <= 0
    then error Input_limit
    else (
      match
        Build.Limits.create
          ~max_work_steps:t.work_left
          ~max_input_bytes:(Some t.input_left)
          ~document:t.limits.document
      with
      | Ok limits -> Ok limits
      | Error _ -> error Accounting_overflow)
  ;;

  let debit_build t build =
    let open Result.Let_syntax in
    let work = Build.Accounting.work_steps (Build.accounting build) in
    if work < 0 || work > t.work_left
    then error Work_limit
    else (
      t.work_left <- t.work_left - work;
      let input =
        Prepared.Accounting.input_bytes (Prepared.accounting (Build.prepared build))
      in
      let%map () = debit_input t input in
      ())
  ;;

  let exact_previous t selected previous ~checkpoint =
    match selected, previous with
    | None, None -> Ok None
    | None, Some _ -> error Unexpected_previous_build
    | Some _, None -> error Previous_lock_mismatch
    | Some metadata, Some build ->
      tick t ~checkpoint;
      if source_matches_metadata (Build.lock_source build) metadata
      then Ok (Some build)
      else error Previous_lock_mismatch
  ;;

  let validate_project t project ~derived_entry ~previous ~contract ~checkpoint =
    let open Result.Let_syntax in
    tick t ~checkpoint;
    let%bind sources, entry, extra =
      selected_inputs t project ~derived_entry ~checkpoint
    in
    let captured_inputs =
      Option.value_map extra ~default:sources ~f:(fun source -> source :: sources)
    in
    let mode = Bundle.Manifest.Project.mode project in
    let%bind build, expected_sources =
      match mode with
      | Exact_lock lock_id ->
        let%bind () =
          match previous with
          | None -> Ok ()
          | Some _ -> error Unexpected_previous_build
        in
        let%bind lock_source = find_source t lock_id ~checkpoint in
        let other_sources =
          List.filter sources ~f:(fun source ->
            tick t ~checkpoint;
            not (Captured.Id.equal lock_id (Captured.id source)))
        in
        let other_sources =
          Option.value_map extra ~default:other_sources ~f:(fun source ->
            source :: other_sources)
        in
        let%bind limits = remaining_build_limits t in
        let%map build =
          Build.restore lock_source ~sources:other_sources ~contract ~limits ~checkpoint
          |> Result.map_error ~f:(fun problem -> Error.create (Builder problem))
        in
        build, captured_inputs
      | Compose { selected_previous_lock; generated_lock } ->
        let%bind previous =
          exact_previous t selected_previous_lock previous ~checkpoint
        in
        let routes =
          List.map (Bundle.Manifest.Project.routes project) ~f:(fun route ->
            tick t ~checkpoint;
            Build.Effective.Route.create
              ~declaration:(Bundle.Manifest.Route.declaration route)
              ~origin:(Bundle.Manifest.Route.origin route)
              ~dependencies:(Bundle.Manifest.Route.dependencies route))
        in
        let%bind limits = remaining_build_limits t in
        let%map build =
          Build.Effective.compose
            ~root_project:(Bundle.Manifest.Project.root_source project)
            ~selection:(selection_request (Bundle.Manifest.Project.selection project))
            ~entry
            ~sources
            ~routes
            ~previous
            ~lock_id:generated_lock
            ~contract
            ~limits
            ~checkpoint
          |> Result.map_error ~f:(fun problem -> Error.create (Builder problem))
        in
        build, Build.lock_source build :: captured_inputs
    in
    let%bind () = debit_build t build in
    let%bind root_source =
      find_source t (Bundle.Manifest.Project.root_source project) ~checkpoint
    in
    let%bind () =
      Result_check.reconcile
        project
        build
        ~root_source
        ~expected_sources
        ~document:t.limits.document
        ~checkpoint:(fun () -> tick t ~checkpoint)
      |> Result.map_error ~f:(function
        | Result_check.Declaration problem -> Error.create (Project_declaration problem)
        | Mismatch -> Error.create Result_mismatch)
    in
    let expected_lock =
      match mode with
      | Exact_lock id -> id
      | Compose { generated_lock; _ } -> generated_lock
    in
    if not (Captured.Id.equal expected_lock (Captured.id (Build.lock_source build)))
    then error Result_mismatch
    else
      Ok
        { Validated.manifest_fingerprint = Bundle.Manifest.fingerprint t.manifest
        ; root_source = Bundle.Manifest.Project.root_source project
        ; build
        ; input_fingerprint = Prepared.fingerprint (Build.prepared build)
        ; selected_input_sources = captured_inputs
        }
  ;;

  let validate_top_project t ~previous ~contract ~checkpoint =
    with_open
      t
      ~error_of_problem:(fun problem -> Error.create problem)
      ~f:(fun () ->
        match Bundle.Manifest.basis t.manifest with
        | Project project ->
          validate_project t project ~derived_entry:None ~previous ~contract ~checkpoint
        | Chatmd_document _ -> error Wrong_project_basis)
  ;;

  let validate_selected_script t ~selector ~derived_entry ~previous ~contract ~checkpoint =
    with_open
      t
      ~error_of_problem:(fun problem -> Error.create problem)
      ~f:(fun () ->
        match Bundle.Manifest.basis t.manifest with
        | Project _ -> error Wrong_project_basis
        | Chatmd_document document ->
          let rec find = function
            | [] -> error Missing_selected_script
            | script :: rest ->
              tick t ~checkpoint;
              if
                Bundle.Manifest.Script.Selector.equal
                  selector
                  (Bundle.Manifest.Script.selector script)
              then (
                match Bundle.Manifest.Script.choice script with
                | Source_only -> error Wrong_project_basis
                | Selected_project project ->
                  validate_project
                    t
                    project
                    ~derived_entry
                    ~previous
                    ~contract
                    ~checkpoint)
              else find rest
          in
          find (Bundle.Manifest.Chatmd_document.scripts document))
  ;;
end
