open! Core
module P = Agent_protocol
module Store = Developer_run_store
module Doc = Developer_result_intent_document

module Limits = struct
  type t =
    { inline_bytes : int
    ; max_result_bytes : int
    ; max_pending : int
    }

  let create ~inline_bytes ~max_result_bytes ~max_pending =
    if
      inline_bytes < 0
      || inline_bytes > Doc.max_inline_bytes
      || max_result_bytes <= 0
      || max_result_bytes > 9 * 1024 * 1024
      || inline_bytes > max_result_bytes
      || max_pending <= 0
      || max_pending > 1024
      || max_pending > 64 * 1024 * 1024 / max_result_bytes
    then Error (P.Error.invalid_request "invalid developer result publication limits")
    else Ok { inline_bytes; max_result_bytes; max_pending }
  ;;
end

module Storage = struct
  type t =
    | Automatic
    | Artifact
  [@@deriving equal, sexp]
end

type selected =
  { publisher : t
  ; claim : Store.Claim.t
  ; proposed : P.Developer_job.State.t
  ; after : P.Developer_job.State.t
  ; completion : P.Completion.t
  ; selected_at : P.Timestamp.t
  ; reference : Developer_result_reference.t option
  ; intent : Developer_result_intent.t
  ; content : string
  ; phase : Blob_store.Result_read.t option Result_publication_phase.t
  }

and t =
  { runs : Store.t
  ; root : Store.Result_root.t
  ; blobs : Blob_store.t
  ; sw : Eio.Switch.t
  ; limits : Limits.t
  ; lock : Result_publication_lock.t
  ; pending_by_job : (P.Id.Job.t, selected) Hashtbl.t
  }

let invalid message = Error (P.Error.invalid_request message)
let store_error result = Result.map_error result ~f:Store_error.to_protocol_error

let same_state a b =
  Jsonaf.exactly_equal (P.Developer_job.State.to_json a) (P.Developer_job.State.to_json b)
;;

let create ~sw ~runs ~root ~blobs ~(limits : Limits.t) =
  Eio.Switch.check sw;
  if
    (not (Store.Result_root.issued_by runs root))
    || not (Blob_store.developer_binding_matches blobs runs)
  then invalid "developer result publisher has another physical Store or Blob binding"
  else if Int64.(of_int limits.max_result_bytes > Blob_store.max_upload_bytes blobs)
  then invalid "developer result exceeds bound Blob upload limit"
  else
    Ok
      { runs
      ; root
      ; blobs
      ; sw
      ; limits
      ; lock = Result_publication_lock.create ()
      ; pending_by_job = Hashtbl.create (module P.Id.Job)
      }
;;

let selected_document publisher ~claim ~selected_at ~after ~selection =
  let job = Store.Claim.job claim in
  let open Result.Let_syntax in
  let%bind document =
    Doc.create
      ~owner:(P.Developer_job.owner job)
      ~capture_fingerprint:(P.Developer_job.capture_fingerprint job)
      ~job_id:(P.Developer_job.id job)
      ~attempt:(Store.Claim.attempt claim)
      ~selected_at
      ~after
      ~selection
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "developer result selection is invalid or too large")
  in
  store_error
    (Developer_result_intent.make
       ~runs:publisher.runs
       ~root:publisher.root
       ~claim
       document)
;;

let artifact
      publisher
      ~claim
      ~selected_at
      ~(proposed : P.Developer_job.State.t)
      ~completion
      ~content
  =
  let open Result.Let_syntax in
  let blob_id = P.Id.Blob.create () in
  let digest = Digestif.SHA256.(digest_string content |> to_hex) in
  let%bind blob =
    P.Blob.Metadata.create
      ~id:blob_id
      ~kind:File
      ~media_type:"application/vnd.ochat.developer-job-result+json"
      ~byte_length:(Int64.of_int (String.length content))
      ~digest
      ~display_name:"developer-job-result.json"
      ()
  in
  let outcome = P.Stored_completion.outcome (P.Stored_completion.Inline completion) in
  let%bind reference =
    store_error (Developer_result_reference.create ~claim ~outcome ~blob)
  in
  let owner = P.Developer_job.owner (Store.Claim.job claim) in
  let metadata : Blob_metadata.t =
    { blob
    ; creating_principal = owner.principal_id
    ; target =
        Blob_metadata.Target.Developer
          { host_id = owner.host_id
          ; run_id = owner.run_id
          ; principal_id = owner.principal_id
          }
    ; allowed_use = Developer_result_reference.allowed_use reference
    ; created_at = selected_at
    ; expires_at = None
    ; durable = false
    }
  in
  let%bind metadata_document =
    Blob_metadata_document.create metadata
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "developer result metadata is invalid")
  in
  let%bind stage =
    Blob_stage_documents.create metadata_document
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "developer result stage is invalid")
  in
  let after =
    { proposed with
      result = Some (P.Developer_job.Result_value.Artifact { outcome; blob })
    }
  in
  let%map intent =
    selected_document
      publisher
      ~claim
      ~selected_at
      ~after
      ~selection:(Doc.Selection.Artifact { reference; stage })
  in
  after, Some reference, intent
;;

let inline publisher ~claim ~selected_at ~proposed ~completion =
  let open Result.Let_syntax in
  let job = Store.Claim.job claim in
  let%bind document =
    Doc.create
      ~owner:(P.Developer_job.owner job)
      ~capture_fingerprint:(P.Developer_job.capture_fingerprint job)
      ~job_id:(P.Developer_job.id job)
      ~attempt:(Store.Claim.attempt claim)
      ~selected_at
      ~after:proposed
      ~selection:(Doc.Selection.Inline completion)
  in
  Result.map
    (Developer_result_intent.make
       ~runs:publisher.runs
       ~root:publisher.root
       ~claim
       document)
    ~f:(fun intent -> proposed, None, intent)
  |> Result.map_error ~f:(fun _ ->
    Document_schema.Error.Invalid_field
      { path = [ "selection" ]; reason = "selection does not match its physical claim" })
;;

let select publisher ~storage ~claim ~(proposed : P.Developer_job.State.t) ~selected_at =
  Result_publication_lock.with_owner
    publisher.lock
    ~on_reentry:(fun () -> invalid "recursive developer result selection")
    (fun () ->
       Eio.Switch.check publisher.sw;
       let job = Store.Claim.job claim in
       let job_id = P.Developer_job.id job in
       let open Result.Let_syntax in
       let%bind () =
         if
           Store.Claim.issued_by claim publisher.runs
           && Store.Result_root.issued_by publisher.runs publisher.root
           && Blob_store.developer_binding_matches publisher.blobs publisher.runs
           && P.Id.Developer_run.equal
                (P.Developer_job.owner job).run_id
                (Store.Result_root.run_id publisher.root)
         then Ok ()
         else invalid "developer result claim or root is foreign"
       in
       let%bind () =
         if Int.equal (Store.Claim.attempt claim) proposed.attempt
         then Ok ()
         else invalid "developer result attempt differs from its physical claim"
       in
       let%bind _ = P.Developer_job.with_state job proposed in
       match Hashtbl.find publisher.pending_by_job job_id with
       | Some prior ->
         if
           phys_equal prior.claim claim
           && same_state prior.proposed proposed
           && P.Developer_job.same_immutable (Store.Claim.job prior.claim) job
         then Ok prior
         else invalid "another developer result is already selected for this job"
       | None ->
         let%bind () =
           if Hashtbl.length publisher.pending_by_job < publisher.limits.max_pending
           then Ok ()
           else invalid "developer result pending capacity is full"
         in
         let%bind completion =
           match proposed.result with
           | Some (P.Developer_job.Result_value.Inline completion) -> Ok completion
           | _ -> invalid "developer result selection requires an inline completion"
         in
         let%bind () = P.Completion.validate completion in
         let content = P.Completion.to_json completion |> Jsonaf.to_string in
         let%bind () =
           if String.length content <= publisher.limits.max_result_bytes
           then Ok ()
           else invalid "developer result exceeds its captured canonical limit"
         in
         let attempt_inline =
           Storage.equal storage Automatic
           && String.length content <= publisher.limits.inline_bytes
           && String.length content <= Doc.max_inline_bytes
         in
         let%bind after, reference, intent =
           if attempt_inline
           then (
             match inline publisher ~claim ~selected_at ~proposed ~completion with
             | Ok selection -> Ok selection
             | Error (Document_schema.Error.Limit_exceeded _) ->
               artifact publisher ~claim ~selected_at ~proposed ~completion ~content
             | Error _ -> invalid "developer inline selection is invalid")
           else artifact publisher ~claim ~selected_at ~proposed ~completion ~content
         in
         let selection =
           { publisher
           ; claim
           ; proposed
           ; after
           ; completion
           ; selected_at
           ; reference
           ; intent
           ; content
           ; phase = Result_publication_phase.create ()
           }
         in
         Hashtbl.set publisher.pending_by_job ~key:job_id ~data:selection;
         Ok selection)
;;

module Selected = struct
  type t = selected

  let claim t = t.claim
  let proposed t = t.proposed
  let after t = t.after
  let completion t = t.completion
  let selected_at t = t.selected_at
  let reference t = t.reference

  let ensure_attempted t =
    Eio.Switch.check t.publisher.sw;
    Result_publication_phase.ensure_attempted t.phase ~stage:(fun () ->
      let open Result.Let_syntax in
      Eio.Switch.check t.publisher.sw;
      let%bind () = Developer_result_intent.save t.intent in
      Eio.Switch.check t.publisher.sw;
      match Developer_result_intent.stage t.intent with
      | None -> Ok None
      | Some stage ->
        let%map read =
          Blob_store.ensure_result_staged_content
            t.publisher.blobs
            ~sw:t.publisher.sw
            (Blob_store.Result_root.developer t.publisher.root)
            ~stage
            t.content
        in
        Eio.Switch.check t.publisher.sw;
        Some read)
    |> store_error
  ;;

  let acknowledge_installed t installed =
    let job = Store.Claim.job t.claim in
    let installed_job =
      Store.find_job (Store.Installed.observation installed) (P.Developer_job.id job)
    in
    let open Result.Let_syntax in
    let%bind () =
      if
        Store.Installed.issued_by installed t.publisher.runs
        && Option.exists installed_job ~f:(fun value ->
          P.Developer_job.same_immutable value job
          && same_state (P.Developer_job.state value) t.after)
      then Ok ()
      else invalid "installed developer terminal differs from selected result"
    in
    let cleanup_started = ref false in
    let settled_on_return = ref false in
    let settle () =
      Result_publication_phase.acknowledge_installed t.phase ~cleanup:(fun () ->
        cleanup_started := true;
        Developer_result_intent.remove_exact t.intent)
    in
    let retire_slot () =
      if !cleanup_started || !settled_on_return
      then
        Result_publication_lock.with_owner
          t.publisher.lock
          ~on_reentry:(fun () -> invalid_arg "recursive developer result retirement")
          (fun () ->
             match Hashtbl.find t.publisher.pending_by_job (P.Developer_job.id job) with
             | Some current when phys_equal current t ->
               Hashtbl.remove t.publisher.pending_by_job (P.Developer_job.id job)
             | Some _ | None -> ())
    in
    Exn.protect
      ~f:(fun () ->
        let result = store_error (settle ()) in
        if Result.is_ok result then settled_on_return := true;
        result)
      ~finally:retire_slot
  ;;
end

let pending publisher ~job =
  Result_publication_lock.with_owner
    publisher.lock
    ~on_reentry:(fun () -> invalid "recursive developer result query")
    (fun () ->
       match Hashtbl.find publisher.pending_by_job (P.Developer_job.id job) with
       | None -> Ok None
       | Some selected ->
         if P.Developer_job.same_immutable (Store.Claim.job selected.claim) job
         then Ok (Some selected)
         else invalid "developer result query has a different job identity")
;;
