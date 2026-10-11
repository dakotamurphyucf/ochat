open! Core
module Owner = Prompt_source_owner
module Receipt = Agent_store.Prompt_source_receipt
module Id = Agent_protocol.Id.Prompt_definition

module Source_selection = struct
  type t =
    { receipt : Receipt.t
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
    if not (Receipt.equal t.receipt expected)
    then Error (Owner.Error.foreign_receipt ())
    else Owner.Policy.check t.policy
  ;;

  let lease t ~expected ~sw ~checkpoint =
    Result.bind (check_intent t expected) ~f:(fun () ->
      Result.bind t.owner ~f:(fun owner -> Owner.lease owner ~sw ~checkpoint))
  ;;

  let close t = Result.iter t.owner ~f:Owner.close
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

type prepared =
  { entries : entry list
  ; revisions : Prompt_revision.t list
  ; sources : Source_selection.t String.Map.t
  ; mutable installed : bool
  ; mutable discarded : bool
  ; admission : Admission.t option
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; artifact_store : Agent_store.Prompt_artifact_store.t
  ; mutex : Eio.Mutex.t
  ; identity : unit ref
  ; mutable generation : int
  ; mutable candidate : unit ref option
  ; mutable entries_by_id : entry String.Map.t
  ; mutable entries_by_name : entry String.Map.t
  ; mutable source_selections : Source_selection.t String.Map.t
  ; mutable revisions : Prompt_revision.t String.Map.t
  }

let create ~env ~artifact_store =
  { env
  ; artifact_store
  ; mutex = Eio.Mutex.create ()
  ; identity = ref ()
  ; generation = 0
  ; candidate = None
  ; entries_by_id = String.Map.empty
  ; entries_by_name = String.Map.empty
  ; revisions = String.Map.empty
  ; source_selections = String.Map.empty
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
  let entries, revisions =
    List.map definitions ~f:(prepare_entry t ~transaction_id ~created_at) |> List.unzip
  in
  { entries
  ; revisions = List.filter_opt revisions
  ; sources = String.Map.empty
  ; installed = false
  ; discarded = false
  ; admission = None
  }
;;

let discard_prepared prepared =
  if
    Option.is_some prepared.admission
    && (not prepared.installed)
    && not prepared.discarded
  then (
    prepared.discarded <- true;
    Eio.Cancel.protect (fun () ->
      Map.iter prepared.sources ~f:Source_selection.close;
      Option.iter prepared.admission ~f:(fun admission -> admission.release ())))
;;

let source_selection t id =
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

let prepare_owned
      t
      ~sw
      ~source_limits
      ~max_roots
      ~checkpoint
      ~transaction_id
      ~created_at
      definitions
  =
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
           match t.candidate with
           | Some _ ->
             Or_error.error_string "original source catalog candidate already pending"
           | None ->
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
                    if not (Owner.Policy.accepts_limits old.policy ~limits:source_limits)
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
                let selection : Source_selection.t = { receipt; policy; owner } in
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
                           ~limits:
                             (Prompt.Chat_markdown.Limits.create
                                ~max_input_bytes:(8 * 1024 * 1024)
                                ~max_parse_calls:1024
                                ~max_tokens:100_000
                                ~max_depth:128
                              |> Or_error.ok_exn)
                           ~checkpoint)
                     with
                     | Error error -> source_error definition error
                     | Ok capture ->
                       (match
                          Prompt_revision_builder.build_captured
                            ~capture:(Owner.Capture.value capture)
                            ~source_receipt:receipt
                            ~artifact_store:t.artifact_store
                            ~transaction_id:(transaction_id definition)
                            ~created_at
                            definition
                        with
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
              { entries
              ; revisions = List.filter_opt revisions
              ; sources = !sources
              ; installed = false
              ; discarded = false
              ; admission = Some admission
              }
            in
            transferred := true;
            Ok prepared)
     with
     | Abort.Admission error -> Error error)
;;

let install_owned t prepared =
  let publication =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let valid =
        match prepared.admission with
        | None -> Ok ()
        | Some admission ->
          if not (phys_equal admission.catalog t.identity)
          then Or_error.error_string "foreign original source catalog candidate"
          else if prepared.installed
          then Ok ()
          else if
            (not (Int.equal admission.generation t.generation))
            || not
                 (Option.exists t.candidate ~f:(fun token ->
                    phys_equal token admission.token))
          then Or_error.error_string "stale original source catalog candidate"
          else Ok ()
      in
      if prepared.discarded
      then Or_error.error_string "discarded prompt catalog candidate"
      else
        Result.bind valid ~f:(fun () ->
          if prepared.installed && Option.is_some prepared.admission
          then Ok []
          else if t.generation = Int.max_value
          then Or_error.error_string "prompt catalog generation exhausted"
          else (
            (* Allocate complete maps BEFORE irreversible policy revocation. *)
            let entries_by_id =
              List.fold prepared.entries ~init:String.Map.empty ~f:(fun map entry ->
                Map.set map ~key:(Id.to_string entry.definition.id) ~data:entry)
            in
            let entries_by_name =
              List.fold prepared.entries ~init:String.Map.empty ~f:(fun map entry ->
                Map.set map ~key:entry.definition.config_name ~data:entry)
            in
            let revisions =
              List.fold prepared.revisions ~init:t.revisions ~f:(fun map revision ->
                Map.set
                  map
                  ~key:
                    (Agent_protocol.Id.Prompt_revision.to_string
                       (Prompt_revision.id revision))
                  ~data:revision)
            in
            let retired = Map.data t.source_selections in
            Map.iteri t.source_selections ~f:(fun ~key ~data:old ->
              match Map.find prepared.sources key with
              | Some next when phys_equal old.policy next.policy -> ()
              | None | Some _ -> Owner.Policy.revoke old.policy);
            t.entries_by_id <- entries_by_id;
            t.entries_by_name <- entries_by_name;
            t.revisions <- revisions;
            t.source_selections <- prepared.sources;
            t.generation <- t.generation + 1;
            t.candidate <- None;
            prepared.installed <- Option.is_some prepared.admission;
            Ok retired)))
  in
  Result.map publication ~f:(fun retired ->
    Eio.Cancel.protect (fun () -> List.iter retired ~f:Source_selection.close))
;;

let install t prepared = install_owned t prepared |> Or_error.ok_exn
let entries t = Eio.Mutex.use_ro t.mutex (fun () -> Map.data t.entries_by_id)

let find t id =
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries_by_id (Id.to_string id))
;;

let find_by_name t name =
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries_by_name name)
;;

let find_revision t revision_id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    Map.find t.revisions (Agent_protocol.Id.Prompt_revision.to_string revision_id))
;;

let prune_unreferenced_artifacts t ~additional =
  let protected =
    Eio.Mutex.use_ro t.mutex (fun () ->
      List.map (Map.data t.revisions) ~f:Prompt_revision.id @ additional)
  in
  Agent_store.Prompt_artifact_store.prune_unreferenced t.artifact_store ~protected
;;

let restore_revision t ~definition_id ~revision_id =
  match find_revision t revision_id with
  | Some revision -> Ok revision
  | None ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match
        Map.find t.revisions (Agent_protocol.Id.Prompt_revision.to_string revision_id)
      with
      | Some revision -> Ok revision
      | None ->
        (match Map.find t.entries_by_id (Id.to_string definition_id) with
         | None ->
           Error
             [ Prompt_revision_builder.Diagnostic.
                 { code = "prompt.definition_missing"
                 ; message = "pinned prompt definition is absent from the catalog"
                 ; source = None
                 }
             ]
         | Some entry ->
           (match
              Prompt_revision_builder.restore
                ~artifact_store:t.artifact_store
                entry.definition
                revision_id
            with
            | Error _ as failure -> failure
            | Ok revision ->
              t.revisions
              <- Map.set
                   t.revisions
                   ~key:(Agent_protocol.Id.Prompt_revision.to_string revision_id)
                   ~data:revision;
              Ok revision)))
;;
