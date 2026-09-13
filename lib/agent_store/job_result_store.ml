open Core
module P = Agent_protocol
module Artifact = P.Job_artifact

type phase =
  | Allocated
  | Prepared of Blob_store.Handle.t
  | Attempted
  | Retained
  | Discarded

type prepared =
  { store : Blob_store.t
  ; env : Eio_unix.Stdenv.base
  ; session : Session_store.Handle.t
  ; intent : Job_result_intent.t
  ; reference : Artifact.t
  ; completion : P.Completion.t
  ; mutex : Eio.Mutex.t
  ; mutable phase : phase
  }

let reference prepared = prepared.reference

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let allocate store ~env ~session ~job ~creating_principal ~now ~max_bytes completion =
  let open Result.Let_syntax in
  let%bind () = P.Completion.validate completion |> protocol in
  let%bind () =
    match job.P.Job.kind, job.status with
    | Async_tool, (Running | Waiting_completion _)
      when job.attempt > 0
           && P.Id.Session.equal job.session_id (Session_store.Handle.session_id session)
      -> Ok ()
    | _ ->
      Error (Store_error.Corrupt "result artifact requires its running async job attempt")
  in
  let content = P.Completion.to_json completion |> Jsonaf.to_string in
  let%bind () =
    match max_bytes > 0 && String.length content <= max_bytes with
    | true -> Ok ()
    | false ->
      Error (Store_error.Corrupt "result artifact exceeds its captured output limit")
  in
  let id = P.Id.Blob.create () in
  let digest = Digestif.SHA256.(digest_string content |> to_hex) in
  let%bind blob =
    P.Blob.Metadata.create
      ~id
      ~kind:File
      ~media_type:Artifact.media_type
      ~byte_length:(Int64.of_int (String.length content))
      ~digest
      ~display_name:"job-result.json"
      ()
    |> protocol
  in
  let%bind reference =
    Artifact.create
      ~session_id:job.session_id
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~blob
    |> protocol
  in
  let expires_at =
    Some
      (P.Timestamp.to_time_ns now
       |> fun at -> Time_ns.add at (Time_ns.Span.of_day 1.) |> P.Timestamp.of_time_ns)
  in
  let metadata : Blob_store.Metadata.t =
    { blob
    ; creating_principal
    ; target_session = Some job.session_id
    ; allowed_use = Artifact.allowed_use reference
    ; created_at = now
    ; expires_at
    ; durable = false
    }
  in
  let%map intent = Job_result_intent.make ~session ~reference ~metadata in
  { store
  ; env
  ; session
  ; intent
  ; reference
  ; completion
  ; mutex = Eio.Mutex.create ()
  ; phase = Allocated
  }
;;

let ensure_prepared prepared ~sw =
  Eio.Mutex.use_rw ~protect:true prepared.mutex (fun () ->
    match prepared.phase with
    | Retained | Discarded -> Error (Store_error.Corrupt "result preparation has ended")
    | (Allocated | Prepared _ | Attempted) as phase ->
      let open Result.Let_syntax in
      let%bind () =
        Job_result_intent.save ~env:prepared.env ~session:prepared.session prepared.intent
      in
      let content = P.Completion.to_json prepared.completion |> Jsonaf.to_string in
      let%map handle =
        Blob_store.ensure_staged_content
          prepared.store
          ~sw
          prepared.session
          ~metadata:(Job_result_intent.metadata prepared.intent)
          content
      in
      (match phase with
       | Allocated | Prepared _ -> prepared.phase <- Prepared handle
       | Attempted -> ()
       | Retained | Discarded -> assert false))
;;

let prepare store ~env ~sw ~session ~job ~creating_principal ~now ~max_bytes completion =
  let open Result.Let_syntax in
  let%bind prepared =
    allocate store ~env ~session ~job ~creating_principal ~now ~max_bytes completion
  in
  let%map () = ensure_prepared prepared ~sw in
  prepared
;;

let commit prepared ~persist =
  Eio.Mutex.use_rw ~protect:true prepared.mutex (fun () ->
    match prepared.phase with
    | Retained | Discarded ->
      Error
        (P.Error.create
           Already_resolved
           ~message:"result artifact preparation has ended"
           ~retryable:false
           ())
    | Allocated ->
      Error
        (P.Error.create
           Invalid_state
           ~message:"result bytes are not prepared"
           ~retryable:true
           ())
    | Prepared _ | Attempted ->
      prepared.phase <- Attempted;
      let open Result.Let_syntax in
      let%map result = persist prepared.reference in
      prepared.phase <- Retained;
      (* Publication already succeeded. A failed marker cleanup leaves an intent
         for reconciliation; it cannot turn success into another publication. *)
      ignore
        (Job_result_intent.remove
           ~env:prepared.env
           ~session:prepared.session
           prepared.intent
         : (unit, Store_error.t) result);
      result)
;;

let discard prepared =
  Eio.Mutex.use_rw ~protect:true prepared.mutex (fun () ->
    match prepared.phase with
    | Retained -> Error (Store_error.Corrupt "cannot discard a retained result artifact")
    | Allocated | Attempted ->
      Error
        (Store_error.Corrupt
           "result artifact requires durable reference reconciliation before discard")
    | Discarded -> Ok ()
    | Prepared handle ->
      let open Result.Let_syntax in
      let%bind () =
        Blob_store.discard_unreferenced prepared.store prepared.session handle
      in
      let%map () =
        Job_result_intent.remove
          ~env:prepared.env
          ~session:prepared.session
          prepared.intent
      in
      prepared.phase <- Discarded)
;;

let load store ~sw ~session ~max_bytes reference =
  let open Result.Let_syntax in
  let%bind reference = Artifact.of_json (Artifact.to_json reference) |> protocol in
  let%bind () =
    match
      P.Id.Session.equal reference.session_id (Session_store.Handle.session_id session)
    with
    | true -> Ok ()
    | false -> Error (Store_error.Corrupt "result artifact belongs to another session")
  in
  let%bind handle = Blob_store.open_session store session reference.blob.id in
  let metadata = Blob_store.Handle.metadata handle in
  let%bind () =
    match
      String.equal metadata.allowed_use (Artifact.allowed_use reference)
      && Jsonaf.exactly_equal
           (P.Blob.Metadata.to_json metadata.blob)
           (P.Blob.Metadata.to_json reference.blob)
    with
    | true -> Ok ()
    | false ->
      Error
        (Store_error.Corrupt "result artifact metadata differs from its saved reference")
  in
  let%bind content = Blob_store.load_verified store ~sw handle ~max_bytes in
  let%bind json =
    Result.try_with (fun () -> Jsonaf.of_string content)
    |> Result.map_error ~f:(fun _ -> Store_error.Corrupt "invalid result artifact JSON")
  in
  P.Completion.of_json json |> protocol
;;

module Publisher = struct
  type collection_limits =
    { max_intents : int
    ; max_entries : int
    ; max_bytes : int
    ; max_file_bytes : int
    }

  type collection_stats =
    { discarded : int
    ; retired : int
    ; retained : int
    }
  [@@deriving sexp]

  type t =
    { blobs : Blob_store.t
    ; env : Eio_unix.Stdenv.base
    ; sw : Eio.Switch.t
    ; session : Session_store.Handle.t
    ; principal : P.Id.Principal.t
    ; inline_bytes : int
    ; max_bytes : int
    ; mutex : Eio.Mutex.t
    ; mutable pending : prepared list
    }

  let create ~env ~blobs ~sw ~session ~principal ~inline_bytes ~max_bytes =
    match
      inline_bytes >= 0
      && max_bytes > 0
      && inline_bytes <= max_bytes
      && Int64.(of_int max_bytes <= Blob_store.max_upload_bytes blobs)
    with
    | false -> Error (P.Error.invalid_request "invalid job result storage policy")
    | true ->
      Ok
        { blobs
        ; env
        ; sw
        ; session
        ; principal
        ; inline_bytes
        ; max_bytes
        ; mutex = Eio.Mutex.create ()
        ; pending = []
        }
  ;;

  let prune t jobs =
    let current, stale =
      List.partition_tf t.pending ~f:(fun prepared ->
        let reference = reference prepared in
        List.exists jobs ~f:(fun job ->
          P.Id.Job.equal reference.job_id job.P.Job.id
          && Int.equal reference.generation job.generation
          && Int.equal reference.attempt job.attempt
          &&
          match job.status with
          | Running | Waiting_completion _ -> true
          | _ -> false))
    in
    t.pending <- current;
    (* Attempted writes must survive until durable orphan reconciliation. *)
    List.iter stale ~f:(fun prepared ->
      ignore (discard prepared : (unit, Store_error.t) result))
  ;;

  let check_completion t completion =
    let open Result.Let_syntax in
    let%bind () = P.Completion.validate completion in
    match
      String.length (P.Completion.to_json completion |> Jsonaf.to_string) <= t.max_bytes
    with
    | true -> Ok ()
    | false ->
      Error
        (P.Error.create
           Resource_limit
           ~message:"job completion exceeds the host storage limit"
           ~retryable:false
           ())
  ;;

  let publish t ~jobs ~job ~now completion ~persist =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let open Result.Let_syntax in
      prune t jobs;
      let%bind () = check_completion t completion in
      let content = P.Completion.to_json completion |> Jsonaf.to_string in
      let cached =
        List.find t.pending ~f:(fun prepared ->
          P.Id.Job.equal (reference prepared).job_id job.P.Job.id)
      in
      match cached, String.length content <= t.inline_bytes with
      | None, true -> persist (P.Stored_completion.Inline completion)
      | _, _ ->
        let%bind prepared =
          match cached with
          | Some prepared -> Ok prepared
          | None ->
            let%map prepared =
              allocate
                t.blobs
                ~env:t.env
                ~session:t.session
                ~job
                ~creating_principal:t.principal
                ~now
                ~max_bytes:t.max_bytes
                completion
              |> Result.map_error ~f:Store_error.to_protocol_error
            in
            t.pending <- prepared :: t.pending;
            prepared
        in
        let%bind stored = P.Stored_completion.artifact (reference prepared) completion in
        let%bind () =
          ensure_prepared prepared ~sw:t.sw
          |> Result.map_error ~f:Store_error.to_protocol_error
        in
        let%map result = commit prepared ~persist:(fun _ -> persist stored) in
        t.pending
        <- List.filter t.pending ~f:(fun other -> not (phys_equal other prepared));
        result)
  ;;

  let pending_completion t ~(job : P.Job.t) =
    Eio.Mutex.use_ro t.mutex (fun () ->
      match job.status with
      | Running | Waiting_completion _ ->
        List.find_map t.pending ~f:(fun prepared ->
          let reference = reference prepared in
          match
            P.Id.Session.equal reference.session_id job.session_id
            && P.Id.Job.equal reference.job_id job.id
            && Int.equal reference.generation job.generation
            && Int.equal reference.attempt job.attempt
          with
          | true -> Some prepared.completion
          | false -> None)
      | _ -> None)
  ;;

  let restore t ~jobs ~generation ~max_count ~max_total_bytes =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let open Result.Let_syntax in
      let invalid message = Error (P.Error.invalid_request message) in
      let%bind () =
        match max_count >= 0 && max_total_bytes >= 0 with
        | true -> Ok ()
        | false -> invalid "job result recovery limits must be nonnegative"
      in
      let%bind intents =
        Job_result_intent.list ~env:t.env ~session:t.session ~max_count
        |> Result.map_error ~f:Store_error.to_protocol_error
      in
      let%bind selected =
        List.fold_result intents ~init:[] ~f:(fun selected intent ->
          let reference = Job_result_intent.reference intent in
          match
            List.find jobs ~f:(fun job ->
              P.Id.Session.equal reference.session_id job.P.Job.session_id
              && P.Id.Job.equal reference.job_id job.id
              && Int.equal reference.generation generation
              && Int.equal reference.generation job.generation
              && Int.equal reference.attempt job.attempt
              &&
              match job.kind, job.status with
              | Async_tool, (Running | Waiting_completion _) -> true
              | _ -> false)
          with
          | None -> Ok selected
          | Some job ->
            (match
               List.find selected ~f:(fun (other, _) ->
                 P.Id.Job.equal job.id other.P.Job.id)
             with
             | None -> Ok ((job, intent) :: selected)
             | Some (_, previous) ->
               let prior = Job_result_intent.reference previous in
               (match
                  String.equal reference.blob.digest prior.blob.digest
                  && Int64.equal reference.blob.byte_length prior.blob.byte_length
                with
                | true -> Ok ((job, intent) :: selected)
                | false -> invalid "conflicting prepared completions for one job attempt")))
      in
      let%bind _ =
        List.fold_result selected ~init:max_total_bytes ~f:(fun remaining (_, intent) ->
          let size = (Job_result_intent.reference intent).blob.byte_length in
          match Int64.(size <= of_int remaining && size <= of_int t.max_bytes) with
          | true -> Ok (remaining - Int64.to_int_exn size)
          | false ->
            Error
              (P.Error.create
                 Resource_limit
                 ~message:"prepared job results exceed the recovery byte budget"
                 ~retryable:false
                 ()))
      in
      let%bind restored =
        List.fold_result (List.rev selected) ~init:[] ~f:(fun restored (job, intent) ->
          let metadata = Job_result_intent.metadata intent in
          let%bind content =
            Blob_store.load_staged_content
              t.blobs
              ~sw:t.sw
              t.session
              ~metadata
              ~max_bytes:t.max_bytes
            |> Result.map_error ~f:Store_error.to_protocol_error
          in
          match content with
          | None -> Ok restored
          | Some _
            when List.exists restored ~f:(fun (other, _, _) ->
                   P.Id.Job.equal other.P.Job.id job.id) -> Ok restored
          | Some content ->
            let%bind json =
              Result.try_with (fun () -> Jsonaf.of_string content)
              |> Result.map_error ~f:(fun _ ->
                P.Error.invalid_request "invalid staged completion JSON")
            in
            let%bind completion = P.Completion.of_json json in
            let reference = Job_result_intent.reference intent in
            let%bind _ = P.Stored_completion.artifact reference completion in
            let%bind () =
              match
                List.find t.pending ~f:(fun prepared ->
                  P.Id.Job.equal prepared.reference.job_id job.id)
              with
              | None -> Ok ()
              | Some prepared ->
                (match P.Id.Blob.equal prepared.reference.blob.id reference.blob.id with
                 | true -> Ok ()
                 | false -> invalid "live result preparation conflicts with recovery")
            in
            let prepared =
              { store = t.blobs
              ; env = t.env
              ; session = t.session
              ; intent
              ; reference
              ; completion
              ; mutex = Eio.Mutex.create ()
              ; phase = Attempted
              }
            in
            Ok ((job, prepared, metadata.created_at) :: restored))
      in
      (* Publish no partial selection if any root or budget check failed. No files
         are mutated here. Attempted is conservative across lost acknowledgements. *)
      List.iter restored ~f:(fun (job, prepared, _) ->
        t.pending
        <- prepared
           :: List.filter t.pending ~f:(fun old ->
             not (P.Id.Job.equal old.reference.job_id job.P.Job.id)));
      Ok
        (List.rev_map restored ~f:(fun (job, prepared, at) ->
           job, prepared.completion, at)))
  ;;

  let load t reference =
    load t.blobs ~sw:t.sw ~session:t.session ~max_bytes:t.max_bytes reference
    |> Result.map_error ~f:(fun failure ->
      let retryable = (Store_error.to_protocol_error failure).retryable in
      P.Error.create
        Blob_unavailable
        ~message:"The saved job result is unavailable or failed verification."
        ~retryable
        ())
  ;;

  let collect_locked t ~jobs ~generation ~(limits : collection_limits) ~with_roots =
    (* The actor already excludes transitions. Take the publisher before the
       response cache, and storage last; publication callbacks can need the cache. *)
    let open Result.Let_syntax in
    let%bind reader =
      Retention_reader.create
        ~env:t.env
        ~root:(Session_store.Handle.directory t.session)
        ~max_entries:limits.max_entries
        ~max_bytes:limits.max_bytes
    in
    let%bind intents =
      Job_result_intent.list_with_reader
        ~reader
        ~session:t.session
        ~max_count:limits.max_intents
    in
    match intents with
    | [] -> Ok (Some { discarded = 0; retired = 0; retained = 0 })
    | _ ->
      let candidates =
        List.map intents ~f:(fun intent -> (Job_result_intent.reference intent).blob.id)
      in
      let%bind scanner = Blob_reference_scan.create candidates |> protocol in
      let same_attempt reference (job : P.Job.t) =
        P.Id.Session.equal reference.Artifact.session_id job.session_id
        && P.Id.Job.equal reference.job_id job.id
        && Int.equal reference.generation job.generation
        && Int.equal reference.attempt job.attempt
      in
      let live reference =
        List.exists jobs ~f:(fun job ->
          same_attempt reference job
          && Int.equal job.generation generation
          &&
          match job.status with
          | Queued | Running | Waiting_permission _ | Waiting_completion _ -> true
          | Succeeded | Failed _ | Cancelled | Interrupted _ -> false)
      in
      (* No filesystem discard here: even stale cache entries may have reached
           durable storage before an acknowledgement failed. *)
      t.pending <- List.filter t.pending ~f:(fun prepared -> live prepared.reference);
      let%bind () =
        List.fold_result t.pending ~init:() ~f:(fun () prepared ->
          let text = P.Completion.to_json prepared.completion |> Jsonaf.to_string in
          let%map () = Retention_reader.charge_bytes reader (String.length text) in
          Blob_reference_scan.begin_root scanner;
          Blob_reference_scan.feed scanner text)
      in
      let owned =
        List.filter_map intents ~f:(fun intent ->
          let reference = Job_result_intent.reference intent in
          match live reference with
          | true -> Some reference.blob.id
          | false -> None)
      in
      with_roots ~reader ~candidates ~f:(fun roots ->
        Blob_store.with_retention t.blobs ~f:(fun scope ->
          let%bind graph =
            Blob_retention.scan
              ~scope
              ~session:t.session
              ~reader
              ~intents
              ~max_file_bytes:limits.max_file_bytes
          in
          (* Validate every terminal descriptor before the first mutation. *)
          let%bind published =
            List.fold_result jobs ~init:[] ~f:(fun refs job ->
              let%map stored = P.Job.terminal_result job |> protocol in
              match stored with
              | Some (Artifact { reference; _ }) -> reference :: refs
              | None | Some (Inline _) -> refs)
          in
          let%bind retained =
            Blob_retention.references
              graph
              ~roots:
                (List.map published ~f:(fun reference -> reference.Artifact.blob.id)
                 @ owned
                 @ Blob_reference_scan.references scanner
                 @ roots)
          in
          List.fold_result
            intents
            ~init:{ discarded = 0; retired = 0; retained = 0 }
            ~f:(fun stats intent ->
              let reference = Job_result_intent.reference intent in
              match List.mem retained reference.blob.id ~equal:P.Id.Blob.equal with
              | false ->
                let%map () =
                  Job_result_intent.discard_unreferenced
                    ~env:t.env
                    ~scope
                    ~reader
                    ~session:t.session
                    intent
                in
                t.pending
                <- List.filter t.pending ~f:(fun prepared ->
                     not (P.Id.Blob.equal prepared.reference.blob.id reference.blob.id));
                { stats with discarded = stats.discarded + 1 }
              | true ->
                let installed =
                  Blob_retention.published graph reference.blob.id
                  && List.exists published ~f:(fun other ->
                    Sexp.equal (Artifact.sexp_of_t other) (Artifact.sexp_of_t reference))
                in
                (match installed with
                 | false -> Ok { stats with retained = stats.retained + 1 }
                 | true ->
                   let%map () =
                     Job_result_intent.retire_published
                       ~env:t.env
                       ~reader
                       ~session:t.session
                       intent
                   in
                   { stats with retired = stats.retired + 1 }))))
  ;;

  let collect t ~jobs ~generation ~limits ~with_roots =
    let outcome =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        try Ok (collect_locked t ~jobs ~generation ~limits ~with_roots) with
        | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
    in
    match outcome with
    | Ok result -> result
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  ;;
end
