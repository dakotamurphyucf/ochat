open! Core
module P = Agent_protocol
module Store = Developer_run_store
module Doc = Developer_result_intent_document

module Key = struct
  type t =
    { run : P.Id.Developer_run.t
    ; epoch : int
    ; job : P.Id.Job.t
    }
  [@@deriving compare, hash, sexp]
end

type intent =
  { key : Key.t
  ; root : Store.Result_root.t
  ; job : P.Developer_job.t
  ; document : Doc.t
  ; bytes : string
  }

type blob_copy =
  { temporary : Blob_developer_namespace.Entry.t option
  ; durable : Blob_developer_namespace.Entry.t option
  }

type verified =
  { intent : intent
  ; completion : P.Completion.t
  ; content : string option
  ; reference : Developer_result_reference.t option
  }

let corrupt message = Error (Store_error.Corrupt message)

let inspect_directory reader ~checkpoint path =
  checkpoint ();
  let open Result.Let_syntax in
  let%bind relative =
    match String.chop_prefix path ~prefix:(Retention_reader.root reader ^ "/") with
    | Some relative when not (String.is_empty relative) -> Ok relative
    | Some _ | None -> corrupt "developer result namespace is outside its owned root"
  in
  let%bind found = Retention_reader.kind_or_missing reader ~path:relative in
  match found with
  | None -> Ok false
  | Some `Directory -> Ok true
  | Some `File -> corrupt "developer result namespace entry is not a directory"
;;

let prefixed ~prefix name =
  let open Result.Let_syntax in
  let%bind rest =
    match String.chop_prefix name ~prefix with
    | Some rest when not (String.is_empty rest) -> Ok rest
    | Some _ | None -> corrupt "developer result namespace name is not canonical"
  in
  Ok rest
;;

let typed_run name =
  let open Result.Let_syntax in
  let%bind value = prefixed ~prefix:"run_" name in
  let%bind run =
    P.Id.Developer_run.of_string value
    |> Result.map_error ~f:(fun _ ->
      Store_error.Corrupt "developer result intent run name is invalid")
  in
  if String.equal value (P.Id.Developer_run.to_string run)
  then Ok run
  else corrupt "developer result intent run name is not canonical"
;;

let typed_epoch name =
  let open Result.Let_syntax in
  let%bind value = prefixed ~prefix:"epoch_" name in
  match Int.of_string_opt value with
  | Some epoch when epoch >= 0 && String.equal value (Int.to_string epoch) -> Ok epoch
  | Some _ | None -> corrupt "developer result intent epoch name is invalid"
;;

let typed_job name =
  let open Result.Let_syntax in
  let%bind stem = prefixed ~prefix:"job_" name in
  let%bind stem =
    match String.chop_suffix stem ~suffix:".frame" with
    | Some stem -> Ok stem
    | None -> corrupt "developer result intent file extension is invalid"
  in
  let%bind job_name, attempt_name =
    match String.rsplit2 stem ~on:'-' with
    | Some pair -> Ok pair
    | None -> corrupt "developer result intent job key is invalid"
  in
  let%bind job =
    P.Id.Job.of_string job_name
    |> Result.map_error ~f:(fun _ ->
      Store_error.Corrupt "developer result intent job ID is invalid")
  in
  let%bind attempt =
    match Int.of_string_opt attempt_name with
    | Some attempt when attempt > 0 && String.equal attempt_name (Int.to_string attempt)
      -> Ok attempt
    | Some _ | None -> corrupt "developer result intent attempt is invalid"
  in
  if String.equal job_name (P.Id.Job.to_string job)
  then Ok (job, attempt)
  else corrupt "developer result intent job name is not canonical"
;;

module Snapshot = struct
  type root_rows =
    { root : Store.Result_root.t
    ; epochs : (int, Chatml_source.Digest.t) Hashtbl.t
    }

  type expected =
    { reference : Developer_result_reference.t
    ; stage : Blob_stage_documents.t option
    ; terminal : bool
    }

  type t =
    { roots : (P.Id.Developer_run.t, root_rows) Hashtbl.t
    ; jobs : (Key.t, P.Developer_job.t) Hashtbl.t
    ; intents : (Key.t, intent) Hashtbl.t
    ; validated : (Key.t, [ `Complete of verified | `Incomplete ]) Hashtbl.t
    ; expected : (P.Id.Blob.t, expected) Hashtbl.t
    ; copies : (P.Id.Blob.t, blob_copy) Hashtbl.t
    }

  let create () =
    { roots = Hashtbl.create (module P.Id.Developer_run)
    ; jobs = Hashtbl.create (module Key)
    ; intents = Hashtbl.create (module Key)
    ; validated = Hashtbl.create (module Key)
    ; expected = Hashtbl.create (module P.Id.Blob)
    ; copies = Hashtbl.create (module P.Id.Blob)
    }
  ;;

  let key_of_job job =
    let owner = P.Developer_job.owner job in
    { Key.run = owner.run_id; epoch = owner.epoch; job = P.Developer_job.id job }
  ;;

  let add_unique table ~key ~data ~message =
    if Hashtbl.mem table key
    then corrupt message
    else (
      Hashtbl.set table ~key ~data;
      Ok ())
  ;;

  let roots snapshot ~runs ~reader ~checkpoint =
    let open Result.Let_syntax in
    let%bind roots =
      Store.Result_root.existing_for_result_inventory runs ~reader ~checkpoint
      |> Result.map_error ~f:(fun error ->
        Store_error.Corrupt (Sexp.to_string_hum (Store.Error.sexp_of_t error)))
    in
    List.fold_result roots ~init:() ~f:(fun () root ->
      checkpoint ();
      let run = Store.Result_root.run_id root in
      let%bind rows = Store.Result_root.rows_for_inventory root ~reader ~checkpoint in
      let epochs = Hashtbl.create (module Int) in
      let%bind () =
        List.fold_result
          (Store.Result_root.Inventory_rows.epochs rows)
          ~init:()
          ~f:(fun () (number, capture) ->
            checkpoint ();
            add_unique
              epochs
              ~key:number
              ~data:capture
              ~message:"developer result inventory has duplicate epoch")
      in
      let%bind () =
        add_unique
          snapshot.roots
          ~key:run
          ~data:{ root; epochs }
          ~message:"developer result inventory has duplicate run"
      in
      List.fold_result
        (Store.Result_root.Inventory_rows.jobs rows)
        ~init:()
        ~f:(fun () job ->
          checkpoint ();
          add_unique
            snapshot.jobs
            ~key:(key_of_job job)
            ~data:job
            ~message:"developer result inventory has duplicate historical job"))
  ;;

  let expected_ref snapshot ~reference ~stage ~terminal =
    let key = (Developer_result_reference.blob reference).id in
    match Hashtbl.find snapshot.expected key with
    | None ->
      Hashtbl.set snapshot.expected ~key ~data:{ reference; stage; terminal };
      Ok ()
    | Some existing ->
      if
        Jsonaf.exactly_equal
          (Developer_result_reference.to_json existing.reference)
          (Developer_result_reference.to_json reference)
        &&
        match existing.stage, stage with
        | None, _ | _, None -> true
        | Some left, Some right -> Blob_stage_documents.equal left right
      then (
        Hashtbl.set
          snapshot.expected
          ~key
          ~data:
            { reference
            ; stage = Option.first_some existing.stage stage
            ; terminal = existing.terminal || terminal
            };
        Ok ())
      else corrupt "developer result Blob is selected by conflicting jobs"
  ;;

  let terminal_references snapshot ~checkpoint =
    Hashtbl.fold snapshot.jobs ~init:(Ok ()) ~f:(fun ~key:_ ~data:job prior ->
      let open Result.Let_syntax in
      let%bind () = prior in
      checkpoint ();
      match (P.Developer_job.state job).result with
      | Some (Artifact _) ->
        let%bind reference = Developer_result_reference.of_terminal_job job in
        expected_ref snapshot ~reference ~stage:None ~terminal:true
      | Some (Inline _) | None -> Ok ())
  ;;

  let intent_file snapshot ~reader ~checkpoint ~directory ~run ~epoch name =
    let open Result.Let_syntax in
    checkpoint ();
    let%bind job_id, attempt = typed_job name in
    let key = { Key.run; epoch; job = job_id } in
    let%bind root_rows =
      match Hashtbl.find snapshot.roots run with
      | Some rows -> Ok rows
      | None -> corrupt "developer result intent names an absent run"
    in
    let%bind capture =
      match Hashtbl.find root_rows.epochs epoch with
      | Some capture -> Ok capture
      | None -> corrupt "developer result intent names an absent epoch"
    in
    let%bind job =
      match Hashtbl.find snapshot.jobs key with
      | Some job -> Ok job
      | None -> corrupt "developer result intent names an absent job"
    in
    let%bind bytes =
      Retention_reader.read_with_checkpoint
        reader
        ~path:(Filename.concat directory name)
        ~max_bytes:(128 * 1024)
        ~checkpoint
    in
    checkpoint ();
    let%bind named =
      Result_intent_file.decode bytes ~limits:Doc.limits ~max_frame_bytes:(128 * 1024)
    in
    let%bind identity =
      Doc.stored_identity named
      |> Result.map_error ~f:(fun error -> Store_error.Document error)
    in
    let%bind document =
      Doc.of_document named
      |> Result.map_error ~f:(fun error -> Store_error.Document error)
    in
    let stored_owner = Doc.Stored_identity.owner identity in
    let job_owner = P.Developer_job.owner job in
    let%bind () =
      if
        P.Execution_owner.equal_developer stored_owner job_owner
        && P.Execution_owner.equal_developer (Doc.owner document) job_owner
        && P.Id.Job.equal (Doc.Stored_identity.job_id identity) job_id
        && P.Id.Job.equal (Doc.job_id document) job_id
        && Int.equal (Doc.Stored_identity.attempt identity) attempt
        && Int.equal (Doc.attempt document) attempt
        && Int.equal (P.Developer_job.state job).attempt attempt
        && Chatml_source.Digest.equal capture (Doc.capture_fingerprint document)
        && Chatml_source.Digest.equal capture (P.Developer_job.capture_fingerprint job)
      then Ok ()
      else corrupt "developer result intent differs from persisted historical job"
    in
    let%bind _ =
      P.Developer_job.with_state job (Doc.after document)
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "developer result intent terminal differs from its job")
    in
    let%bind () =
      match (P.Developer_job.state job).status with
      | P.Job.Succeeded | Failed _ | Cancelled | Interrupted _ ->
        if
          Jsonaf.exactly_equal
            (P.Developer_job.State.to_json (P.Developer_job.state job))
            (P.Developer_job.State.to_json (Doc.after document))
        then Ok ()
        else corrupt "retained intent disagrees with installed terminal"
      | Queued | Running | Waiting_permission _ | Waiting_completion _ -> Ok ()
    in
    let%bind () =
      match Doc.selection document with
      | Inline _ -> Ok ()
      | Artifact { reference; stage } ->
        expected_ref snapshot ~reference ~stage:(Some stage) ~terminal:false
    in
    add_unique
      snapshot.intents
      ~key
      ~data:{ key; root = root_rows.root; job; document; bytes }
      ~message:"developer result intent key occurs twice"
  ;;

  let intents snapshot ~reader ~checkpoint ~directory =
    let open Result.Let_syntax in
    let%bind exists = inspect_directory reader ~checkpoint directory in
    if not exists
    then Ok ()
    else (
      let%bind reader = Retention_reader.at_root reader ~root:directory in
      let%bind runs =
        Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint
      in
      List.fold_result runs ~init:() ~f:(fun () run_name ->
        checkpoint ();
        let%bind run = typed_run run_name in
        let%bind root_rows =
          match Hashtbl.find snapshot.roots run with
          | Some rows -> Ok rows
          | None -> corrupt "developer result intent has no authentic run"
        in
        let%bind () =
          Retention_reader.kind reader ~path:run_name
          |> Result.bind ~f:(function
            | `Directory -> Ok ()
            | `File -> corrupt "developer result run entry is a file")
        in
        let%bind epochs =
          Retention_reader.list_with_checkpoint reader ~directory:run_name ~checkpoint
        in
        List.fold_result epochs ~init:() ~f:(fun () epoch_name ->
          checkpoint ();
          let%bind epoch = typed_epoch epoch_name in
          let%bind () =
            if Hashtbl.mem root_rows.epochs epoch
            then Ok ()
            else corrupt "developer result intent has no authentic epoch"
          in
          let%bind () =
            Retention_reader.kind reader ~path:(Filename.concat run_name epoch_name)
            |> Result.bind ~f:(function
              | `Directory -> Ok ()
              | `File -> corrupt "developer result epoch entry is a file")
          in
          let%bind names =
            Retention_reader.list_with_checkpoint
              reader
              ~directory:(Filename.concat run_name epoch_name)
              ~checkpoint
          in
          List.fold_result names ~init:() ~f:(fun () name ->
            intent_file
              snapshot
              ~reader
              ~checkpoint
              ~directory:(Filename.concat run_name epoch_name)
              ~run
              ~epoch
              name))))
  ;;

  let matching_metadata expected metadata ~durable =
    let reference = expected.reference in
    match expected.stage with
    | Some stage ->
      let selected =
        if durable
        then Blob_stage_documents.durable stage
        else Blob_stage_documents.temporary stage
      in
      Blob_metadata.equal metadata (Blob_metadata_document.value selected)
    | None ->
      let owner = Developer_result_reference.owner reference in
      Blob_metadata.blob_equal metadata.blob (Developer_result_reference.blob reference)
      && P.Id.Principal.equal metadata.creating_principal owner.principal_id
      && Blob_metadata.Target.equal
           metadata.target
           (Blob_metadata.Target.Developer
              { host_id = owner.host_id
              ; run_id = owner.run_id
              ; principal_id = owner.principal_id
              })
      && String.equal
           metadata.allowed_use
           (Developer_result_reference.allowed_use reference)
      && Bool.equal metadata.durable durable
  ;;

  let add_blob snapshot ~checkpoint entry =
    checkpoint ();
    let id = Blob_developer_namespace.Entry.id entry in
    let durable = Blob_developer_namespace.Entry.durable entry in
    let open Result.Let_syntax in
    let%bind expected =
      match Hashtbl.find snapshot.expected id with
      | Some expected -> Ok expected
      | None -> corrupt "developer Blob has no real selected or terminal reference"
    in
    let%bind () =
      match Blob_developer_namespace.Entry.state entry with
      | Complete { metadata; _ } ->
        if matching_metadata expected metadata ~durable
        then Ok ()
        else corrupt "developer Blob metadata differs from selected result"
      | Partial None ->
        if durable then corrupt "durable developer Blob is incomplete" else Ok ()
      | Partial (Some metadata) ->
        if durable
        then corrupt "durable developer Blob is incomplete"
        else if matching_metadata expected metadata ~durable
        then Ok ()
        else corrupt "partial developer Blob metadata differs from selection"
    in
    let prior =
      Hashtbl.find snapshot.copies id
      |> Option.value ~default:{ temporary = None; durable = None }
    in
    let%bind updated =
      if durable
      then
        if Option.is_some prior.durable
        then corrupt "developer Blob durable identity repeats"
        else Ok { prior with durable = Some entry }
      else if Option.is_some prior.temporary
      then corrupt "developer Blob temporary identity repeats"
      else Ok { prior with temporary = Some entry }
    in
    Hashtbl.set snapshot.copies ~key:id ~data:updated;
    Ok ()
  ;;

  let blob_namespace snapshot ~reader ~checkpoint ~directory ~durable ~max_blob_bytes =
    let open Result.Let_syntax in
    let%bind exists = inspect_directory reader ~checkpoint directory in
    if not exists
    then Ok ()
    else (
      let%bind runs =
        Blob_developer_namespace.run_ids_with_checkpoint ~reader ~checkpoint ~directory
      in
      List.fold_result runs ~init:() ~f:(fun () run ->
        checkpoint ();
        let%bind root_rows =
          match Hashtbl.find snapshot.roots run with
          | Some rows -> Ok rows
          | None -> corrupt "developer Blob namespace has no authentic run"
        in
        let target : Blob_metadata.Target.developer =
          { host_id = Store.Result_root.host_id root_rows.root
          ; run_id = run
          ; principal_id = Store.Result_root.principal_id root_rows.root
          }
        in
        let%bind entries =
          Blob_developer_namespace.scan_run
            ~reader
            ~checkpoint
            ~directory:(Filename.concat directory (P.Id.Developer_run.to_string run))
            ~target
            ~durable
            ~max_blob_bytes
        in
        List.fold_result entries ~init:() ~f:(fun () entry ->
          add_blob snapshot ~checkpoint entry)))
  ;;

  let complete_content snapshot ~checkpoint reference =
    checkpoint ();
    let id = (Developer_result_reference.blob reference).id in
    let copy = Hashtbl.find snapshot.copies id in
    let state = function
      | None -> None
      | Some entry -> Some (Blob_developer_namespace.Entry.state entry)
    in
    let temporary = Option.bind copy ~f:(fun copy -> state copy.temporary) in
    let durable = Option.bind copy ~f:(fun copy -> state copy.durable) in
    match temporary, durable with
    | ( Some (Complete { content = left; metadata = left_metadata })
      , Some (Complete { content = right; metadata = right_metadata }) ) ->
      if
        String.equal left right
        && Blob_metadata.equal { left_metadata with durable = true } right_metadata
      then Ok (Some right)
      else corrupt "developer Blob temporary and durable copies disagree"
    | _, Some (Complete { content; _ }) -> Ok (Some content)
    | Some (Complete { content; _ }), None -> Ok (Some content)
    | Some (Complete { content; _ }), Some (Partial _) ->
      corrupt "developer Blob durable publication is uncertain"
    | Some (Partial _), None | None, None -> Ok None
    | None, Some (Partial _) | Some (Partial _), Some (Partial _) ->
      corrupt "developer Blob durable publication is incomplete"
  ;;

  let decoded_completion ~checkpoint ~max_bytes ~reference content =
    let open Result.Let_syntax in
    let length = String.length content in
    let%bind () =
      if length <= max_bytes
      then Ok ()
      else corrupt "developer result completion exceeds recovery byte limit"
    in
    for index = 0 to (length - 1) / 4096 do
      ignore (index : int);
      checkpoint ()
    done;
    let%bind limits =
      Document_schema.Limits.create
        ~max_bytes
        ~max_depth:132
        ~max_fields:max_bytes
        ~max_nodes:max_bytes
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "developer result recovery JSON ceiling is invalid")
    in
    let%bind json =
      Document_schema.Json.decode ~limits content
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "developer result completion JSON is invalid or too complex")
    in
    let%bind completion =
      P.Completion.of_json json
      |> Result.map_error ~f:(fun _ ->
        Store_error.Corrupt "developer result completion is invalid")
    in
    checkpoint ();
    if
      P.Stored_completion.equal_outcome
        (P.Stored_completion.outcome (Inline completion))
        (Developer_result_reference.outcome reference)
      && String.equal (P.Completion.to_json completion |> Jsonaf.to_string) content
    then Ok completion
    else corrupt "developer result completion differs from selected bytes or outcome"
  ;;

  let verify_terminal_blobs snapshot ~checkpoint ~max_result_bytes =
    Hashtbl.fold snapshot.expected ~init:(Ok ()) ~f:(fun ~key:_ ~data:expected prior ->
      let open Result.Let_syntax in
      let%bind () = prior in
      checkpoint ();
      if not expected.terminal
      then Ok ()
      else (
        let id = (Developer_result_reference.blob expected.reference).id in
        let complete_durable =
          Hashtbl.find snapshot.copies id
          |> Option.bind ~f:(fun copy -> copy.durable)
          |> Option.exists ~f:(fun entry ->
            match Blob_developer_namespace.Entry.state entry with
            | Complete _ -> true
            | Partial _ -> false)
        in
        if not complete_durable
        then corrupt "published developer Artifact has no exact durable Blob"
        else (
          match Hashtbl.find snapshot.copies id with
          | Some { durable = Some entry; _ } ->
            (match Blob_developer_namespace.Entry.state entry with
             | Complete { content; _ } ->
               decoded_completion
                 ~checkpoint
                 ~max_bytes:max_result_bytes
                 ~reference:expected.reference
                 content
               |> Result.map ~f:(fun _ -> ())
             | Partial _ -> corrupt "published developer Artifact is partial")
          | Some { durable = None; _ } | None ->
            corrupt "published developer Artifact disappeared")))
  ;;

  let verify_intent snapshot ~checkpoint ~max_result_bytes intent =
    let open Result.Let_syntax in
    match Doc.selection intent.document with
    | Inline completion ->
      Ok (`Complete { intent; completion; content = None; reference = None })
    | Artifact { reference; _ } ->
      let%bind content = complete_content snapshot ~checkpoint reference in
      (match content with
       | None -> Ok `Incomplete
       | Some content ->
         let%map completion =
           decoded_completion ~checkpoint ~max_bytes:max_result_bytes ~reference content
         in
         `Complete
           { intent; completion; content = Some content; reference = Some reference })
  ;;

  let verify_all_intents snapshot ~checkpoint ~max_result_bytes =
    Hashtbl.fold snapshot.intents ~init:(Ok ()) ~f:(fun ~key ~data:intent prior ->
      let open Result.Let_syntax in
      let%bind () = prior in
      checkpoint ();
      let%map verified = verify_intent snapshot ~checkpoint ~max_result_bytes intent in
      Hashtbl.set snapshot.validated ~key ~data:verified)
  ;;
end

module Evidence = struct
  type t =
    | No_selection of Store.Recovery.Candidate.t
    | Incomplete_selection of Store.Recovery.Candidate.t * intent
    | Selected of Store.Recovery.Candidate.t * verified
end

let for_candidate snapshot ~checkpoint candidate =
  let open Result.Let_syntax in
  checkpoint ();
  let job = Store.Recovery.Candidate.job candidate in
  let key = Snapshot.key_of_job job in
  let%bind current =
    match Hashtbl.find snapshot.Snapshot.jobs key with
    | Some current when P.Developer_job.same_immutable current job -> Ok current
    | Some _ | None -> corrupt "recovery candidate is absent from retained job inventory"
  in
  let%bind () =
    if
      Jsonaf.exactly_equal
        (P.Developer_job.State.to_json (P.Developer_job.state current))
        (P.Developer_job.State.to_json (P.Developer_job.state job))
    then Ok ()
    else corrupt "recovery candidate changed during inventory"
  in
  match Hashtbl.find snapshot.validated key with
  | None -> Ok (Evidence.No_selection candidate)
  | Some `Incomplete ->
    let intent = Hashtbl.find_exn snapshot.intents key in
    Ok (Evidence.Incomplete_selection (candidate, intent))
  | Some (`Complete verified) -> Ok (Evidence.Selected (candidate, verified))
;;

exception Work_limit

let scan
      ~sw
      ~runs
      ~root
      ~blobs
      ~max_entries
      ~max_bytes
      ~max_work
      ~max_result_bytes
      ~window
      ~view
  =
  let remaining = ref max_work in
  let checkpoint () =
    Eio.Switch.check sw;
    Eio.Fiber.check ();
    if !remaining <= 0 then raise Work_limit;
    decr remaining;
    Eio.Fiber.check ()
  in
  let run () =
    let open Result.Let_syntax in
    let%bind scan =
      Blob_store.with_developer_result_inventory
        blobs
        ~sw
        ~runs
        ~root
        ~max_entries
        ~max_bytes
        ~checkpoint
        ~f:(fun ~reader ~intent_directory ~temporary_namespace ~durable_namespace ->
          let open Result.Let_syntax in
          let checked_view () =
            Store.Result_recovery_affinity.check_view
              runs
              ~root
              ~window
              ~view
              ~reader
              ~checkpoint
            |> Result.map_error ~f:(fun _ ->
              Store_error.Corrupt "developer result View is stale or changed")
          in
          let%bind () = checked_view () in
          let snapshot = Snapshot.create () in
          let%bind () = Snapshot.roots snapshot ~runs ~reader ~checkpoint in
          let%bind () = Snapshot.terminal_references snapshot ~checkpoint in
          let%bind () =
            Snapshot.intents snapshot ~reader ~checkpoint ~directory:intent_directory
          in
          let%bind () =
            Snapshot.blob_namespace
              snapshot
              ~reader
              ~checkpoint
              ~directory:temporary_namespace
              ~durable:false
              ~max_blob_bytes:(Int64.of_int max_result_bytes)
          in
          let%bind () =
            Snapshot.blob_namespace
              snapshot
              ~reader
              ~checkpoint
              ~directory:durable_namespace
              ~durable:true
              ~max_blob_bytes:(Int64.of_int max_result_bytes)
          in
          let%bind () =
            Snapshot.verify_terminal_blobs snapshot ~checkpoint ~max_result_bytes
          in
          let%bind () =
            Snapshot.verify_all_intents snapshot ~checkpoint ~max_result_bytes
          in
          let%bind () = checked_view () in
          List.fold_result
            (Store.Recovery.View.candidates view)
            ~init:[]
            ~f:(fun acc candidate ->
              let%map evidence = for_candidate snapshot ~checkpoint candidate in
              evidence :: acc)
          |> Result.map ~f:List.rev)
      |> Result.map_error ~f:Store_error.to_protocol_error
    in
    match scan with
    | Some evidence -> Ok evidence
    | None -> Error (P.Error.invalid_request "developer result inventory is busy")
  in
  try run () with
  | Work_limit -> Error (P.Error.invalid_request "developer result inventory work limit")
;;
