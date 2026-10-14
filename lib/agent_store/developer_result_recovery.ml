open! Core
module P = Agent_protocol
module Store = Developer_run_store
module Doc = Developer_result_intent_document

module Limits = struct
  type t =
    { max_entries : int
    ; max_bytes : int
    ; max_work : int
    ; max_result_bytes : int
    }

  let create ~max_entries ~max_bytes ~max_work ~max_result_bytes =
    if
      max_entries <= 0
      || max_entries > 100_000
      || max_bytes <= 0
      || max_bytes > 128 * 1024 * 1024
      || max_work <= 0
      || max_work > 10_000_000
      || max_result_bytes <= 0
      || max_result_bytes > 9 * 1024 * 1024
    then Error (P.Error.invalid_request "invalid developer result recovery limits")
    else Ok { max_entries; max_bytes; max_work; max_result_bytes }
  ;;
end

type t =
  { sw : Eio.Switch.t
  ; runs : Store.t
  ; root : Store.Result_root.t
  ; blobs : Blob_store.t
  ; limits : Limits.t
  }

type owner = t

let invalid message = Error (P.Error.invalid_request message)
let store_error result = Result.map_error result ~f:Store_error.to_protocol_error

let create ~sw ~runs ~root ~blobs ~limits =
  Eio.Switch.check sw;
  if
    (not (Store.Result_root.issued_by runs root))
    || not (Blob_store.developer_binding_matches blobs runs)
  then invalid "developer result recovery has another physical Store or Blob binding"
  else if
    Int64.(of_int limits.Limits.max_result_bytes > Blob_store.max_upload_bytes blobs)
  then invalid "developer result recovery exceeds bound Blob upload limit"
  else Ok { sw; runs; root; blobs; limits }
;;

exception Work_limit

let checkpoint t remaining () =
  Eio.Switch.check t.sw;
  Eio.Fiber.check ();
  if !remaining <= 0 then raise Work_limit;
  decr remaining;
  Eio.Fiber.check ()
;;

let with_action_reader owner ~f =
  let remaining = ref owner.limits.max_work in
  let checkpoint = checkpoint owner remaining in
  try
    Blob_store.with_developer_result_reader
      owner.blobs
      ~sw:owner.sw
      ~runs:owner.runs
      ~root:owner.root
      ~max_entries:owner.limits.max_entries
      ~max_bytes:owner.limits.max_bytes
      ~checkpoint
      ~f:(fun reader -> f ~reader ~checkpoint)
    |> store_error
  with
  | Work_limit -> invalid "developer result action exceeded work limit"
;;

module Inventory = Developer_result_inventory

type intent = Inventory.intent
type verified = Inventory.verified

module Restored = struct
  type t =
    { owner : owner
    ; window : Store.Recovery.t
    ; view : Store.Recovery.View.t
    ; candidate : Store.Recovery.Candidate.t
    ; verified : verified
    ; file : Result_intent_file.t
    ; phase : Blob_store.Result_read.t option Result_publication_phase.t
    ; mutable pending : Store.Recovery.Pending.t option
    }

  let candidate t = t.candidate
  let after t = Doc.after t.verified.intent.document
  let selected_at t = Doc.selected_at t.verified.intent.document
  let completion t = t.verified.completion
  let reference t = t.verified.reference

  let same_state left right =
    Jsonaf.exactly_equal
      (P.Developer_job.State.to_json left)
      (P.Developer_job.State.to_json right)
  ;;

  let ensure_attempted t ~pending =
    with_action_reader t.owner ~f:(fun ~reader ~checkpoint ->
      let open Result.Let_syntax in
      let after = after t in
      let check () =
        Store.Result_recovery_affinity.check_pending
          t.owner.runs
          ~root:t.owner.root
          ~window:t.window
          ~view:t.view
          ~candidate:t.candidate
          ~pending
          ~after
          ~reader
          ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          Store_error.Corrupt "restored result Pending is foreign or changed")
      in
      let%bind () = check () in
      t.pending <- Some pending;
      let stage () =
        let open Result.Let_syntax in
        checkpoint ();
        Eio.Switch.check t.owner.sw;
        let%bind () =
          Result_intent_file.save_exact
            t.file
            ~max_frame_bytes:(128 * 1024)
            t.verified.intent.bytes
        in
        Eio.Switch.check t.owner.sw;
        let%bind read =
          match Doc.selection t.verified.intent.document, t.verified.content with
          | Inline _, None -> Ok None
          | Artifact { stage; _ }, Some content ->
            Blob_store.ensure_result_staged_content
              t.owner.blobs
              ~sw:t.owner.sw
              (Blob_store.Result_root.developer t.owner.root)
              ~stage
              content
            |> Result.map ~f:Option.some
          | Inline _, Some _ | Artifact _, None ->
            Error (Store_error.Corrupt "restored result has no exact selected bytes")
        in
        Eio.Switch.check t.owner.sw;
        let%map () = check () in
        read
      in
      Result_publication_phase.ensure_attempted t.phase ~stage)
  ;;

  let acknowledge_installed t installed =
    let job = Store.Recovery.Candidate.job t.candidate in
    let installed_job =
      Store.find_job (Store.Installed.observation installed) (P.Developer_job.id job)
    in
    with_action_reader t.owner ~f:(fun ~reader ~checkpoint ->
      let open Result.Let_syntax in
      let%bind pending =
        match t.pending with
        | Some pending -> Ok pending
        | None -> Error (Store_error.Corrupt "restored result has no Pending receipt")
      in
      let%bind () =
        if
          Store.Installed.issued_by installed t.owner.runs
          && Option.exists installed_job ~f:(fun current ->
            P.Developer_job.same_immutable current job
            && same_state (P.Developer_job.state current) (after t))
        then Ok ()
        else
          Error (Store_error.Corrupt "installed developer result differs from selection")
      in
      let%bind () =
        Store.Result_recovery_affinity.check_installed
          t.owner.runs
          ~root:t.owner.root
          ~window:t.window
          ~view:t.view
          ~candidate:t.candidate
          ~pending
          ~after:(after t)
          ~installed
          ~reader
          ~checkpoint
        |> Result.map_error ~f:(fun _ ->
          Store_error.Corrupt "installed result differs from original Recovery Pending")
      in
      Result_publication_phase.acknowledge_installed t.phase ~cleanup:(fun () ->
        checkpoint ();
        Result_intent_file.remove_exact
          t.file
          ~expected:t.verified.intent.bytes
          ~max_frame_bytes:(128 * 1024)))
  ;;
end

module Incomplete = struct
  type t =
    { candidate : Store.Recovery.Candidate.t
    ; intent : intent
    }

  let candidate t = t.candidate
  let after t = Doc.after t.intent.document
  let selected_at t = Doc.selected_at t.intent.document

  let reference t =
    match Doc.selection t.intent.document with
    | Inline _ -> None
    | Artifact { reference; _ } -> Some reference
  ;;
end

module Evidence = struct
  type t =
    | Selected of Restored.t
    | No_selection of Store.Recovery.Candidate.t
    | Incomplete_selection of Incomplete.t
end

let inventory owner ~window ~view =
  let open Result.Let_syntax in
  let%bind evidence =
    Inventory.scan
      ~sw:owner.sw
      ~runs:owner.runs
      ~root:owner.root
      ~blobs:owner.blobs
      ~max_entries:owner.limits.max_entries
      ~max_bytes:owner.limits.max_bytes
      ~max_work:owner.limits.max_work
      ~max_result_bytes:owner.limits.max_result_bytes
      ~window
      ~view
  in
  List.map evidence ~f:(function
    | Inventory.Evidence.No_selection candidate -> Ok (Evidence.No_selection candidate)
    | Incomplete_selection (candidate, intent) ->
      Ok (Evidence.Incomplete_selection { Incomplete.candidate; intent })
    | Selected (candidate, verified) ->
      let intent = verified.intent in
      let%map file =
        Result_intent_file.developer
          ~root:owner.root
          ~owner:(Doc.owner intent.document)
          ~job:(Doc.job_id intent.document)
          ~attempt:(Doc.attempt intent.document)
        |> Result.map_error ~f:Store_error.to_protocol_error
      in
      Evidence.Selected
        { Restored.owner
        ; window
        ; view
        ; candidate
        ; verified
        ; file
        ; phase = Result_publication_phase.restored_attempted ()
        ; pending = None
        })
  |> Result.all
;;
