open! Core
module Owner = Prompt_source_owner
module Receipt = Agent_store.Prompt_source_receipt
module Id = Agent_protocol.Id.Prompt_definition

module Owner_domain = struct
  type t = int

  let current () = (Stdlib.Domain.self () :> int)
  let is_current t = Int.equal t (current ())

  let check_exn t =
    if not (is_current t)
    then invalid_arg "prompt catalog used outside its creating Eio domain"
  ;;
end

module Source_selection = struct
  type t =
    { domain : Owner_domain.t
    ; receipt : Receipt.t
    ; policy : Owner.Policy.t
    ; owner : (Owner.t, Owner.Error.t) Result.t
    }

  let receipt t = t.receipt

  type availability =
    | Live
    | Unavailable of Owner.Error.t
    | Revoked
    | Closed

  let availability t =
    Owner_domain.check_exn t.domain;
    match Owner.Policy.check t.policy with
    | Error _ -> Revoked
    | Ok () ->
      (match t.owner with
       | Error error -> Unavailable error
       | Ok owner ->
         (match Owner.check owner with
          | Ok () -> Live
          | Error error ->
            (match Owner.Error.code error with
             | Closed -> Closed
             | Revoked -> Revoked
             | Invalid_binding
             | Foreign_receipt
             | Missing_live_root
             | Busy
             | Lease_limit
             | Filesystem
             | Capture
             | Foreign_capture
             | Project_preparation
             | Document_capture -> Unavailable error)))
  ;;

  let check_intent t expected =
    Owner_domain.check_exn t.domain;
    if not (Receipt.equal t.receipt expected)
    then Error (Owner.Error.foreign_receipt ())
    else Owner.Policy.check t.policy
  ;;

  let lease t ~expected ~sw ~checkpoint =
    Result.bind (check_intent t expected) ~f:(fun () ->
      Result.bind t.owner ~f:(fun owner -> Owner.lease owner ~sw ~checkpoint))
  ;;

  let close t =
    Owner_domain.check_exn t.domain;
    Result.iter t.owner ~f:Owner.close
  ;;
end

type availability =
  | Ready of Prompt_revision.t
  | Unavailable of Prompt_revision_builder.Diagnostic.t list
  | Disabled

type entry =
  { definition : Prompt_definition.t
  ; availability : availability
  }

module Admission = struct
  type t =
    { catalog : unit ref
    ; generation : int
    ; token : unit ref
    ; release : unit -> unit
    }
end

module Publication_state = struct
  type t =
    | Available
    | Sealed
    | Uncertain
  [@@deriving equal, sexp_of]
end

module Seal_error = struct
  type t =
    | Busy
    | Foreign_domain
    | Publication_uncertain
    | Unowned_candidate
    | Foreign_candidate
    | Stale_candidate
    | Discarded_candidate
    | Already_installed
    | Generation_exhausted
    | Conflicting_revision of Agent_protocol.Id.Prompt_revision.t
    | Aborted
    | Committed
    | Pending
  [@@deriving equal, sexp_of]
end

type prepared =
  { domain : Owner_domain.t
  ; entries : entry list
  ; revisions : Prompt_revision.t list
  ; sources : Source_selection.t String.Map.t
  ; mutable installed : bool
  ; mutable discarded : bool
  ; mutable sealed : bool
  ; admission : Admission.t option
  }

module Install_plan = struct
  type t =
    { entries_by_id : entry String.Map.t
    ; entries_by_name : entry String.Map.t
    ; revisions : Prompt_revision.t String.Map.t
    ; revocations : Owner.Policy.t list
    ; retired : Source_selection.t list
    ; candidate_owners : Source_selection.t list
    ; next_generation : int
    }
end

module Seal_phase = struct
  type t =
    | Pending
    | Committed
    | Aborted
    | Uncertain
end

type t =
  { domain : Owner_domain.t
  ; env : Eio_unix.Stdenv.base
  ; artifact_store : Agent_store.Prompt_artifact_store.t
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; mutex : Eio.Mutex.t
  ; identity : unit ref
  ; mutable generation : int
  ; mutable candidate : unit ref option
  ; mutable entries_by_id : entry String.Map.t
  ; mutable entries_by_name : entry String.Map.t
  ; mutable source_selections : Source_selection.t String.Map.t
  ; mutable installed_revisions : Prompt_revision.t String.Map.t
  ; mutable restored_revisions : Prompt_revision.t String.Map.t
  ; mutable publication : publication
  }

and publication =
  | Open
  | Sealed of sealed
  | Indeterminate of sealed

and sealed =
  { catalog : t
  ; prepared : prepared
  ; plan : Install_plan.t
  ; mutable phase : Seal_phase.t
  ; mutable retired : bool
  ; mutable retiring : bool
  ; uncertain_publication : publication
  ; pending_publication : publication
  }

let create ~env ~artifact_store ~content_limits =
  { domain = Owner_domain.current ()
  ; env
  ; artifact_store
  ; content_limits
  ; mutex = Eio.Mutex.create ()
  ; identity = ref ()
  ; generation = 0
  ; candidate = None
  ; entries_by_id = String.Map.empty
  ; entries_by_name = String.Map.empty
  ; installed_revisions = String.Map.empty
  ; restored_revisions = String.Map.empty
  ; source_selections = String.Map.empty
  ; publication = Open
  }
;;

let prepare_entry t ~transaction_id ~created_at definition =
  if not definition.Prompt_definition.enabled
  then { definition; availability = Disabled }, None
  else (
    match
      Prompt_revision_builder.build
        ~env:t.env
        ~artifact_store:t.artifact_store
        ~transaction_id:(transaction_id definition)
        ~created_at
        definition
    with
    | Ok revision -> { definition; availability = Ready revision }, Some revision
    | Error diagnostics -> { definition; availability = Unavailable diagnostics }, None)
;;

let prepare t ~transaction_id ~created_at definitions =
  Owner_domain.check_exn t.domain;
  let entries, revisions =
    List.map definitions ~f:(prepare_entry t ~transaction_id ~created_at) |> List.unzip
  in
  { domain = t.domain
  ; entries
  ; revisions = List.filter_opt revisions
  ; sources = String.Map.empty
  ; installed = false
  ; discarded = false
  ; sealed = false
  ; admission = None
  }
;;

let discard_prepared (prepared : prepared) =
  Owner_domain.check_exn prepared.domain;
  if
    Option.is_some prepared.admission
    && (not prepared.installed)
    && (not prepared.discarded)
    && not prepared.sealed
  then (
    prepared.discarded <- true;
    Eio.Cancel.protect (fun () ->
      Map.iter prepared.sources ~f:Source_selection.close;
      Option.iter prepared.admission ~f:(fun admission -> admission.release ())))
;;

let source_selection t id =
  Owner_domain.check_exn t.domain;
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.source_selections (Id.to_string id))
;;

let source_error definition error =
  ( { definition
    ; availability =
        Unavailable
          [ Prompt_revision_builder.Diagnostic.
              { code = "prompt.original_source_unavailable"
              ; message = Sexp.to_string_hum (Owner.Error.sexp_of_t error)
              ; source = Some definition.Prompt_definition.root_file
              }
          ]
    }
  , None )
;;

let capture_needs_content captured ~checkpoint =
  let captured = Prompt_source_owner.Capture.value captured in
  let needs_content parsed =
    checkpoint ();
    Prompt_content_custody.requires_captured_documents parsed ~checkpoint
  in
  needs_content (Prompt_revision_builder.Capture.root captured)
  || List.exists (Prompt_revision_builder.Capture.agents captured) ~f:(fun (_, parsed) ->
    needs_content parsed)
;;

let prepare_owned
      t
      ?prepare_programs
      ?prepare_content
      ~sw
      ~source_limits
      ~max_roots
      ~checkpoint
      ~transaction_id
      ~created_at
      definitions
  =
  if not (Owner_domain.is_current t.domain)
  then Or_error.error_string "prompt catalog used outside its creating Eio domain"
  else
    let module Abort = struct
      exception Admission of Core.Error.t
    end
    in
    let count =
      if max_roots < 0
      then Or_error.error_string "invalid original source root allowance"
      else
        List.fold_result definitions ~init:0 ~f:(fun count _ ->
          checkpoint ();
          if max_roots < 0 || count >= max_roots
          then Or_error.error_string "original source catalog root limit exceeded"
          else Ok (count + 1))
    in
    match count with
    | Error error -> Error error
    | Ok _ ->
      (try
         let token = ref () in
         let reservation =
           Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
             match t.publication, t.candidate with
             | (Sealed _ | Indeterminate _), _ ->
               Or_error.error_string "prompt catalog publication is reserved or uncertain"
             | Open, Some _ ->
               Or_error.error_string "original source catalog candidate already pending"
             | Open, None ->
               t.candidate <- Some token;
               Ok (t.source_selections, t.generation))
         in
         let previous, generation =
           match reservation with
           | Ok value -> value
           | Error error -> raise (Abort.Admission error)
         in
         let release () =
           Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
             match t.candidate with
             | Some current when phys_equal current token -> t.candidate <- None
             | None | Some _ -> ())
         in
         let admission = Admission.{ catalog = t.identity; generation; token; release } in
         let sources = ref String.Map.empty in
         let transferred = ref false in
         Fun.protect
           ~finally:(fun () ->
             if not !transferred
             then
               Eio.Cancel.protect (fun () ->
                 Map.iter !sources ~f:Source_selection.close;
                 release ()))
           (fun () ->
              let entries, revisions =
                List.map definitions ~f:(fun definition ->
                  Eio.Fiber.check ();
                  checkpoint ();
                  let receipt =
                    Receipt.create
                      ~definition_id:definition.Prompt_definition.id
                      ~root_file:definition.root_file
                      ~max_path_bytes:4096
                      ~checkpoint
                    |> function
                    | Ok receipt -> receipt
                    | Error error -> raise (Abort.Admission error)
                  in
                  let key = Id.to_string definition.id in
                  if Map.mem !sources key
                  then
                    raise
                      (Abort.Admission
                         (Core.Error.of_string "duplicate original source definition"));
                  let policy =
                    match Map.find previous key with
                    | Some old
                      when definition.enabled
                           && Receipt.equal old.receipt receipt
                           && Result.is_ok (Owner.Policy.check old.policy) ->
                      if
                        not (Owner.Policy.accepts_limits old.policy ~limits:source_limits)
                      then
                        raise
                          (Abort.Admission
                             (Core.Error.of_string
                                "original source limits differ for unchanged operator \
                                 selection"));
                      old.policy
                    | None | Some _ -> Owner.Policy.create receipt ~limits:source_limits
                  in
                  let owner =
                    if not definition.enabled
                    then (
                      Owner.Policy.revoke policy;
                      Error (Owner.Error.missing_live_root ()))
                    else
                      Eio.Switch.run (fun root_sw ->
                        (* Authority comes from CURRENT operator definition and actual
                 daemon fs capability, never from retained artifact receipt. *)
                        let opened =
                          try
                            Ok
                              (Eio.Path.open_dir
                                 ~sw:root_sw
                                 Eio.Path.(
                                   Eio.Stdenv.fs t.env
                                   / Filename.dirname definition.root_file))
                          with
                          | Eio.Io _ -> Error (Owner.Error.missing_live_root ())
                        in
                        Result.bind opened ~f:(fun root ->
                          Owner.open_owned root ~sw ~policy ~checkpoint))
                  in
                  let selection : Source_selection.t =
                    { domain = t.domain; receipt; policy; owner }
                  in
                  sources := Map.set !sources ~key ~data:selection;
                  if not definition.enabled
                  then { definition; availability = Disabled }, None
                  else (
                    match owner with
                    | Error error -> source_error definition error
                    | Ok owner ->
                      (match
                         Owner.with_scope owner ~f:(fun scope ->
                           Owner.Scope.capture
                             scope
                             ~env:t.env
                             ~definition
                             ~preprocess:Meta_prompting.Preprocessor.preprocess
                             ~limits:(Prompt_revision_builder.capture_limits ())
                             ~checkpoint
                           |> Result.map ~f:(fun capture ->
                             let prepare_content =
                               Option.filter prepare_content ~f:(fun _ ->
                                 capture_needs_content capture ~checkpoint)
                             in
                             Prompt_revision_builder.build_captured
                               ?prepare_programs:
                                 (Option.map prepare_programs ~f:(fun prepare _ ->
                                    prepare scope capture))
                               ?prepare_content:
                                 (Option.map prepare_content ~f:(fun prepare _ ->
                                    prepare scope capture))
                               ~capture:(Owner.Capture.value capture)
                               ~source_receipt:receipt
                               ~artifact_store:t.artifact_store
                               ~transaction_id:(transaction_id definition)
                               ~created_at
                               definition))
                       with
                       | Error error -> source_error definition error
                       | Ok prepared ->
                         (match prepared with
                          | Error diagnostics ->
                            { definition; availability = Unavailable diagnostics }, None
                          | Ok revision ->
                            { definition; availability = Ready revision }, Some revision))))
                |> List.unzip
              in
              Eio.Fiber.check ();
              checkpoint ();
              Eio.Fiber.check ();
              let prepared =
                { domain = t.domain
                ; entries
                ; revisions = List.filter_opt revisions
                ; sources = !sources
                ; installed = false
                ; discarded = false
                ; sealed = false
                ; admission = Some admission
                }
              in
              transferred := true;
              Ok prepared)
       with
       | Abort.Admission error -> Error error)
;;

module Install_snapshot = struct
  type t =
    { generation : int
    ; installed : Prompt_revision.t String.Map.t
    ; restored : Prompt_revision.t String.Map.t
    ; sources : Source_selection.t String.Map.t
    }
end

let snapshot_install (t : t) =
  Install_snapshot.
    { generation = t.generation
    ; installed = t.installed_revisions
    ; restored = t.restored_revisions
    ; sources = t.source_selections
    }
;;

let admit_install t (prepared : prepared) ~owned =
  let open Result.Let_syntax in
  let%bind () =
    if Owner_domain.is_current prepared.domain
    then Ok ()
    else Error Seal_error.Foreign_domain
  in
  let%bind () =
    match t.publication with
    | Open -> Ok ()
    | Sealed _ -> Error Seal_error.Busy
    | Indeterminate _ -> Error Seal_error.Publication_uncertain
  in
  let%bind () =
    match prepared.admission with
    | None -> if owned then Error Seal_error.Unowned_candidate else Ok ()
    | Some admission ->
      if not (phys_equal admission.catalog t.identity)
      then Error Seal_error.Foreign_candidate
      else if prepared.discarded
      then Error Seal_error.Discarded_candidate
      else if prepared.installed
      then Ok ()
      else if
        (not (Int.equal admission.generation t.generation))
        || not
             (Option.exists t.candidate ~f:(fun token -> phys_equal token admission.token))
      then Error Seal_error.Stale_candidate
      else Ok ()
  in
  if prepared.discarded
  then Error Seal_error.Discarded_candidate
  else if owned && prepared.installed
  then Error Seal_error.Already_installed
  else if
    t.generation = Int.max_value
    && not (prepared.installed && Option.is_some prepared.admission)
  then Error Seal_error.Generation_exhausted
  else Ok ()
;;

let same_revision left right =
  Id.equal (Prompt_revision.definition left).id (Prompt_revision.definition right).id
  && String.equal
       (Prompt_revision.artifact left).manifest_sha256
       (Prompt_revision.artifact right).manifest_sha256
;;

let compute_install (snapshot : Install_snapshot.t) (prepared : prepared) ~checkpoint =
  let open Result.Let_syntax in
  let entries_by_id, entries_by_name =
    List.fold
      prepared.entries
      ~init:(String.Map.empty, String.Map.empty)
      ~f:(fun (by_id, by_name) entry ->
        checkpoint ();
        ( Map.set by_id ~key:(Id.to_string entry.definition.id) ~data:entry
        , Map.set by_name ~key:entry.definition.config_name ~data:entry ))
  in
  let%bind revisions =
    List.fold_result
      prepared.revisions
      ~init:snapshot.Install_snapshot.installed
      ~f:(fun map revision ->
        checkpoint ();
        let id = Prompt_revision.id revision in
        let key = Agent_protocol.Id.Prompt_revision.to_string id in
        let previous =
          match Map.find map key with
          | Some _ as previous -> previous
          | None -> Map.find snapshot.restored key
        in
        match previous with
        | Some previous when not (same_revision previous revision) ->
          Error (Seal_error.Conflicting_revision id)
        | None | Some _ -> Ok (Map.set map ~key ~data:revision))
  in
  let revocations, retired =
    Map.fold snapshot.sources ~init:([], []) ~f:(fun ~key ~data:old (revoke, retired) ->
      checkpoint ();
      let revoke =
        match Map.find prepared.sources key with
        | Some next when phys_equal old.policy next.policy -> revoke
        | None | Some _ -> old.policy :: revoke
      in
      revoke, old :: retired)
  in
  let candidate_owners =
    Map.fold prepared.sources ~init:[] ~f:(fun ~key:_ ~data owners ->
      checkpoint ();
      data :: owners)
  in
  Ok
    Install_plan.
      { entries_by_id
      ; entries_by_name
      ; revisions
      ; revocations
      ; retired
      ; candidate_owners
      ; next_generation = snapshot.generation + 1
      }
;;

(* Called only after exact admission. All values, including retirement lists and
   result carriers, already exist. Policy.revoke is a nonyielding boolean store. *)
let apply_install t (prepared : prepared) plan =
  List.iter plan.Install_plan.revocations ~f:Owner.Policy.revoke;
  t.entries_by_id <- plan.entries_by_id;
  t.entries_by_name <- plan.entries_by_name;
  t.installed_revisions <- plan.revisions;
  t.source_selections <- prepared.sources;
  t.generation <- plan.next_generation;
  t.candidate <- None;
  prepared.installed <- Option.is_some prepared.admission
;;

let close_selections selections =
  Eio.Cancel.protect (fun () -> List.iter selections ~f:Source_selection.close)
;;

let install_owned (t : t) (prepared : prepared) =
  if not (Owner_domain.is_current t.domain)
  then Or_error.error_string "prompt catalog used outside its creating Eio domain"
  else (
    let publication =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let open Result.Let_syntax in
        let%bind () = admit_install t prepared ~owned:false in
        if prepared.installed && Option.is_some prepared.admission
        then Ok []
        else (
          (* This compatibility path has no borrowed callbacks or IO. It uses the
           same admitted computation and pointer transition as an owned Seal. *)
          let%map plan =
            compute_install (snapshot_install t) prepared ~checkpoint:ignore
          in
          apply_install t prepared plan;
          plan.retired))
    in
    publication
    |> Result.map_error ~f:(fun error -> Core.Error.create_s (Seal_error.sexp_of_t error))
    |> Result.map ~f:close_selections)
;;

let publication_state t =
  Owner_domain.check_exn t.domain;
  match t.publication with
  | Open -> Publication_state.Available
  | Sealed _ -> Sealed
  | Indeterminate _ -> Uncertain
;;

let seal (t : t) (prepared : prepared) ~checkpoint =
  if not (Owner_domain.is_current t.domain)
  then Error Seal_error.Foreign_domain
  else
    let open Result.Let_syntax in
    let%bind snapshot =
      Eio.Mutex.use_ro t.mutex (fun () ->
        let%map () = admit_install t prepared ~owned:true in
        snapshot_install t)
    in
    let poll () =
      Eio.Fiber.check ();
      checkpoint ();
      Eio.Fiber.check ()
    in
    let%bind plan = compute_install snapshot prepared ~checkpoint:poll in
    let rec sealed =
      { catalog = t
      ; prepared
      ; plan
      ; phase = Seal_phase.Pending
      ; retired = false
      ; retiring = false
      ; uncertain_publication = Indeterminate sealed
      ; pending_publication = Sealed sealed
      }
    in
    let outcome = Ok sealed in
    poll ();
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let%bind () = admit_install t prepared ~owned:true in
      (* Persistent map identity is the actual cache snapshot witness. A restore
       during the borrowed callback must be accounted before external writing.
       Once sealed, insertion checks the reserved candidate IDs below. *)
      if
        snapshot.generation <> t.generation
        || not (phys_equal snapshot.restored t.restored_revisions)
      then Error Seal_error.Stale_candidate
      else (
        prepared.sealed <- true;
        t.publication <- sealed.pending_publication;
        outcome))
;;

module Seal = struct
  module Error = Seal_error

  type t = sealed

  let success = Ok ()
  let aborted = Error Error.Aborted
  let committed = Error Error.Committed
  let uncertain = Error Error.Publication_uncertain
  let pending = Error Error.Pending
  let busy = Error Error.Busy
  let foreign_domain = Error Error.Foreign_domain

  let retire_selections t selections =
    if t.retiring
    then busy
    else if t.retired
    then success
    else (
      t.retiring <- true;
      Fun.protect
        ~finally:(fun () -> t.retiring <- false)
        (fun () ->
           close_selections selections;
           t.retired <- true;
           success))
  ;;

  let abort t =
    if not (Owner_domain.is_current t.catalog.domain)
    then foreign_domain
    else (
      match t.phase with
      | Committed -> committed
      | Uncertain -> uncertain
      | Aborted -> retire_selections t t.plan.candidate_owners
      | Pending ->
        t.phase <- Aborted;
        t.prepared.sealed <- false;
        t.prepared.discarded <- true;
        t.catalog.publication <- Open;
        t.catalog.candidate <- None;
        retire_selections t t.plan.candidate_owners)
  ;;

  let mark_uncertain t =
    if not (Owner_domain.is_current t.catalog.domain)
    then foreign_domain
    else (
      match t.phase with
      | Aborted -> aborted
      | Committed -> committed
      | Uncertain -> success
      | Pending ->
        t.phase <- Uncertain;
        t.catalog.publication <- t.uncertain_publication;
        success)
  ;;

  let commit t =
    if not (Owner_domain.is_current t.catalog.domain)
    then foreign_domain
    else (
      match t.phase with
      | Aborted -> aborted
      | Uncertain -> uncertain
      | Committed -> success
      | Pending ->
        apply_install t.catalog t.prepared t.plan;
        t.prepared.sealed <- false;
        t.phase <- Committed;
        t.catalog.publication <- Open;
        success)
  ;;

  let retire t =
    if not (Owner_domain.is_current t.catalog.domain)
    then foreign_domain
    else (
      match t.phase with
      | Pending -> pending
      | Aborted | Committed | Uncertain ->
        let selections =
          match t.phase with
          | Committed -> t.plan.retired
          | Aborted | Uncertain -> t.plan.candidate_owners
          | Pending -> assert false
        in
        retire_selections t selections)
  ;;
end

let install t prepared = install_owned t prepared |> Or_error.ok_exn

let entries t =
  Owner_domain.check_exn t.domain;
  Eio.Mutex.use_ro t.mutex (fun () -> Map.data t.entries_by_id)
;;

let prepared_entries (prepared : prepared) = prepared.entries

let find t id =
  Owner_domain.check_exn t.domain;
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries_by_id (Id.to_string id))
;;

let find_by_name t name =
  Owner_domain.check_exn t.domain;
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries_by_name name)
;;

let cached_revision t revision_id =
  let key = Agent_protocol.Id.Prompt_revision.to_string revision_id in
  match Map.find t.installed_revisions key with
  | Some _ as revision -> revision
  | None -> Map.find t.restored_revisions key
;;

let find_revision t revision_id =
  Owner_domain.check_exn t.domain;
  Eio.Mutex.use_ro t.mutex (fun () -> cached_revision t revision_id)
;;

let prune_unreferenced_artifacts t ~additional =
  Owner_domain.check_exn t.domain;
  let protected =
    Eio.Mutex.use_ro t.mutex (fun () ->
      List.map (Map.data t.installed_revisions) ~f:Prompt_revision.id
      @ List.map (Map.data t.restored_revisions) ~f:Prompt_revision.id
      @ (match t.publication with
         | Open -> []
         | Sealed sealed | Indeterminate sealed ->
           List.map sealed.prepared.revisions ~f:Prompt_revision.id)
      @ additional)
  in
  Agent_store.Prompt_artifact_store.prune_unreferenced t.artifact_store ~protected
;;

let restore_error code message =
  Error [ Prompt_revision_builder.Diagnostic.{ code; message; source = None } ]
;;

let validate_definition revision definition_id =
  let artifact = Prompt_revision.artifact revision in
  if
    Id.equal (Prompt_revision.definition revision).id definition_id
    && Option.for_all artifact.prompt_definition_id ~f:(Id.equal definition_id)
    && Option.for_all artifact.source_receipt ~f:(fun receipt ->
      Id.equal (Receipt.definition_id receipt) definition_id)
  then Ok revision
  else
    restore_error
      "prompt.revision_definition_mismatch"
      "pinned prompt revision belongs to another definition"
;;

type restore_admission =
  | Cached of Prompt_revision.t
  | Restore of Prompt_definition.t

let restore_revision t ~definition_id ~revision_id =
  Owner_domain.check_exn t.domain;
  let open Result.Let_syntax in
  let%bind admitted =
    Eio.Mutex.use_ro t.mutex (fun () ->
      match cached_revision t revision_id with
      | Some revision ->
        Result.map (validate_definition revision definition_id) ~f:(fun revision ->
          Cached revision)
      | None ->
        (match Map.find t.entries_by_id (Id.to_string definition_id) with
         | Some entry -> Ok (Restore entry.definition)
         | None ->
           restore_error
             "prompt.definition_missing"
             "pinned prompt definition is absent from the catalog"))
  in
  match admitted with
  | Cached revision -> Ok revision
  | Restore definition ->
    let%bind revision =
      Prompt_revision_builder.restore
        ?content_limits:t.content_limits
        ~artifact_store:t.artifact_store
        definition
        revision_id
    in
    let%bind revision = validate_definition revision definition_id in
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let%bind () =
        match t.publication with
        | Open -> Ok ()
        | Sealed sealed | Indeterminate sealed ->
          (match
             Map.find
               sealed.plan.revisions
               (Agent_protocol.Id.Prompt_revision.to_string revision_id)
           with
           | None -> Ok ()
           | Some reserved ->
             if same_revision reserved revision
             then Ok ()
             else
               restore_error
                 "prompt.revision_conflict"
                 "pinned prompt revision conflicts with the sealed publication")
      in
      match cached_revision t revision_id with
      | Some cached ->
        let%bind cached = validate_definition cached definition_id in
        if
          String.equal
            (Prompt_revision.artifact cached).manifest_sha256
            (Prompt_revision.artifact revision).manifest_sha256
        then Ok cached
        else
          restore_error
            "prompt.revision_conflict"
            "pinned prompt revision conflicts with already retained content"
      | None ->
        t.restored_revisions
        <- Map.set
             t.restored_revisions
             ~key:(Agent_protocol.Id.Prompt_revision.to_string revision_id)
             ~data:revision;
        Ok revision)
;;
