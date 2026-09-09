open Core
module P = Agent_protocol
module Artifact = P.Job_artifact

type phase =
  | Prepared
  | Attempted
  | Retained
  | Discarded

type prepared =
  { store : Blob_store.t
  ; env : Eio_unix.Stdenv.base
  ; session : Session_store.Handle.t
  ; intent : Job_result_intent.t
  ; handle : Blob_store.Handle.t
  ; reference : Artifact.t
  ; mutex : Eio.Mutex.t
  ; mutable phase : phase
  }

let reference prepared = prepared.reference

let protocol result =
  Result.map_error result ~f:(fun error -> Store_error.Corrupt error.P.Error.message)
;;

let prepare store ~env ~sw ~session ~job ~creating_principal ~now ~max_bytes completion =
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
  Eio.Cancel.protect (fun () ->
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
    let%bind intent = Job_result_intent.create ~env ~session ~reference ~metadata in
    let%bind upload =
      Blob_store.begin_upload
        store
        ~sw
        ~id
        ~creating_principal
        ~target_session:(Some job.session_id)
        ~kind:File
        ~media_type:Artifact.media_type
        ~display_name:(Some "job-result.json")
        ~allowed_use:(Artifact.allowed_use reference)
        ~created_at:now
        ~expires_at
    in
    Exn.protect
      ~finally:(fun () -> Blob_store.abort upload)
      ~f:(fun () ->
        let%bind () = Blob_store.write_string upload content in
        let%bind handle = Blob_store.finish upload ~expected_digest:(Some digest) in
        let%map handle = Blob_store.adopt store session handle in
        { store
        ; env
        ; session
        ; intent
        ; handle
        ; reference
        ; mutex = Eio.Mutex.create ()
        ; phase = Prepared
        }))
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
    | Prepared | Attempted ->
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
    | Attempted ->
      Error
        (Store_error.Corrupt
           "result artifact requires durable reference reconciliation before discard")
    | Discarded -> Ok ()
    | Prepared ->
      let open Result.Let_syntax in
      let%bind () =
        Blob_store.discard_unreferenced prepared.store prepared.session prepared.handle
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
              prepare
                t.blobs
                ~env:t.env
                ~sw:t.sw
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
        let%map result = commit prepared ~persist:(fun _ -> persist stored) in
        t.pending
        <- List.filter t.pending ~f:(fun other -> not (phys_equal other prepared));
        result)
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
end
