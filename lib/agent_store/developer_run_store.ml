open! Core
module P = Agent_protocol
module Doc = Developer_run_document
module Root = Session_store.Developer_root
module Id = P.Id.Developer_run

module Limits = struct
  type t =
    { max_runs : int
    ; max_epochs_per_run : int
    ; max_jobs_per_run : int
    ; max_invocations_per_run : int
    ; max_selected_per_commit : int
    ; max_job_payload_bytes : int
    ; max_document_bytes : int
    ; max_recovery_entries : int
    ; max_recovery_bytes : int
    ; max_work_steps : int
    ; document : Document_schema.Limits.t
    }

  let create
        ~max_runs
        ~max_epochs_per_run
        ~max_jobs_per_run
        ~max_invocations_per_run
        ~max_selected_per_commit
        ~max_job_payload_bytes
        ~max_document_bytes
        ~max_recovery_entries
        ~max_recovery_bytes
        ~max_work_steps
        ~document
    =
    let positive values = List.for_all values ~f:(fun value -> value > 0) in
    if
      (not
         (positive
            [ max_runs
            ; max_epochs_per_run
            ; max_jobs_per_run
            ; max_invocations_per_run
            ; max_selected_per_commit
            ; max_job_payload_bytes
            ; max_document_bytes
            ; max_recovery_entries
            ; max_recovery_bytes
            ; max_work_steps
            ]))
      || max_runs > 4096
      || max_epochs_per_run > 1024
      || max_jobs_per_run > 16384
      || max_invocations_per_run > 16384
      || max_selected_per_commit > max_jobs_per_run
      || max_document_bytes > 16 * 1024 * 1024
      || max_recovery_entries < (2 * max_runs) + 1
      || max_recovery_bytes < max_document_bytes
      || max_document_bytes <> Document_schema.Limits.max_bytes document
    then Or_error.error_string "invalid developer run store limits"
    else
      Ok
        { max_runs
        ; max_epochs_per_run
        ; max_jobs_per_run
        ; max_invocations_per_run
        ; max_selected_per_commit
        ; max_job_payload_bytes
        ; max_document_bytes
        ; max_recovery_entries
        ; max_recovery_bytes
        ; max_work_steps
        ; document
        }
  ;;
end

module Error = struct
  type t =
    | Store of Store_error.t
    | Conflict
    | Foreign_or_stale
    | Limit_exceeded
    | Closed
  [@@deriving sexp_of]
end

type t =
  { root_owner : Root.owner
  ; host_id : P.Id.Server.t
  ; directory : Eio.Fs.dir_ty Eio.Path.t
  ; limits : Limits.t
  ; checkpoint : unit -> unit
  ; token : unit ref
  ; mutable closed : bool
  }

type handle =
  { store : t
  ; owner : P.Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; token : unit ref
  }

module Checkpoint = struct
  type phase =
    | Active
    | Interrupted
    | Closed
  [@@deriving equal, sexp_of]

  type t =
    { owner : P.Execution_owner.developer
    ; capture_fingerprint : Chatml_source.Digest.t
    ; phase : phase
    ; revision : int64
    }

  let owner t = t.owner
  let capture_fingerprint t = t.capture_fingerprint
  let phase t = t.phase
  let revision t = t.revision
end

module Observation = struct
  type t =
    { store_token : unit ref
    ; owner : P.Execution_owner.developer
    ; document_bytes : string
    ; document_revision : int64
    ; checkpoint : Checkpoint.t
    }

  let checkpoint t = t.checkpoint
end

module Prepared = struct
  type t =
    { store_token : unit ref
    ; handle_token : unit ref
    ; owner : P.Execution_owner.developer
    ; before_bytes : string
    ; after_bytes : string
    ; after_document : Doc.t
    }
end

module Installed = struct
  type t = { observation : Observation.t }

  let observation t = t.observation
end

type reconciliation =
  | Installed of Installed.t
  | Absent
  | Diverged

let ( let* ) result f = Result.bind result ~f
let store result = Result.map_error result ~f:(fun error -> Error.Store error)
let corrupt message = Error (Error.Store (Store_error.Corrupt message))

exception Work_limit

let directory_name = "developer-runs"
let basename id = "run_" ^ Id.to_string id ^ ".json"

let run_id_of_name name =
  match String.chop_prefix name ~prefix:"run_" with
  | None -> None
  | Some suffix ->
    (match String.chop_suffix suffix ~suffix:".json" with
     | None -> None
     | Some id ->
       Result.ok (Id.of_string id)
       |> Option.filter ~f:(fun parsed -> String.equal name (basename parsed)))
;;

let phase_of_doc = function
  | `Active -> Checkpoint.Active
  | `Interrupted -> Interrupted
  | `Closed -> Closed
;;

let phase_to_doc = function
  | Checkpoint.Active -> `Active
  | Interrupted -> `Interrupted
  | Closed -> `Closed
;;

let find_epoch document ~(owner : P.Execution_owner.developer) =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Server.equal (Doc.host_id document) owner.host_id
      && Id.equal (Doc.run_id document) owner.run_id
      && P.Id.Principal.equal (Doc.principal_id document) owner.principal_id
    then Ok ()
    else Error Error.Foreign_or_stale
  in
  match
    List.find (Doc.epochs document) ~f:(fun epoch -> Doc.epoch_number epoch = owner.epoch)
  with
  | Some epoch -> Ok epoch
  | None -> Error Error.Foreign_or_stale
;;

let observation (t : t) ~owner ~bytes document =
  let* epoch = find_epoch document ~owner in
  let checkpoint : Checkpoint.t =
    { owner
    ; capture_fingerprint = Doc.epoch_capture epoch
    ; phase = phase_of_doc (Doc.epoch_phase epoch)
    ; revision = Doc.epoch_revision epoch
    }
  in
  let observed : Observation.t =
    { store_token = t.token
    ; owner
    ; document_bytes = bytes
    ; document_revision = Doc.revision document
    ; checkpoint
    }
  in
  Ok observed
;;

let valid_handle handle =
  let t = handle.store in
  if t.closed then Error Error.Closed else Ok t
;;

let with_lease t f =
  if t.closed
  then Error Error.Closed
  else (
    try
      Root.with_lease t.root_owner ~f:(fun ~env ~root ->
        if t.closed then Error Error.Closed else f ~env ~root)
      |> store
      |> Result.bind ~f:Fn.id
    with
    | Work_limit -> Error Error.Limit_exceeded)
;;

let read t ~poll id =
  poll ();
  (* A prior replace may have renamed the synced file and then failed or been
     cancelled during parent-directory fsync. Under the single writer lease,
     syncing before observing the name makes a retry/reconcile acknowledgement
     a durability claim rather than merely an existence claim. *)
  let* () = Durable_file.sync_directory_in ~directory:t.directory |> store in
  poll ();
  match
    Durable_file.load_bounded_in
      ~directory:t.directory
      ~basename:(basename id)
      ~max_bytes:t.limits.max_document_bytes
  with
  | Error (Store_error.Missing _) -> Ok None
  | Error error -> Error (Error.Store error)
  | Ok bytes ->
    poll ();
    let* document =
      Doc.of_bytes bytes ~limits:t.limits.document ~max_epochs:t.limits.max_epochs_per_run
      |> store
    in
    poll ();
    if Id.equal (Doc.run_id document) id
    then Ok (Some (bytes, document))
    else corrupt "developer run filename does not match document ID"
;;

let work t checkpoint =
  let remaining = ref t.limits.max_work_steps in
  fun () ->
    if !remaining <= 0 then raise Work_limit;
    decr remaining;
    checkpoint ()
;;

let scan t ~poll =
  poll ();
  let* () = Durable_file.sync_directory_in ~directory:t.directory |> store in
  poll ();
  let* reader =
    Retention_reader.create_at
      ~root:t.directory
      ~max_entries:t.limits.max_recovery_entries
      ~max_bytes:t.limits.max_recovery_bytes
    |> store
  in
  let* names =
    Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint:poll |> store
  in
  List.fold_result names ~init:0 ~f:(fun count name ->
    poll ();
    match run_id_of_name name with
    | Some id ->
      let* () =
        if count >= t.limits.max_runs then Error Error.Limit_exceeded else Ok ()
      in
      let* bytes =
        Retention_reader.read_with_checkpoint
          reader
          ~path:name
          ~max_bytes:t.limits.max_document_bytes
          ~checkpoint:poll
        |> store
      in
      let* document =
        Doc.of_bytes
          bytes
          ~limits:t.limits.document
          ~max_epochs:t.limits.max_epochs_per_run
        |> store
      in
      poll ();
      if
        Id.equal id (Doc.run_id document)
        && P.Id.Server.equal t.host_id (Doc.host_id document)
      then Ok (count + 1)
      else corrupt "developer run authority file has another run ID"
    | None ->
      (match Durable_file.temporary_target name with
       | Some target when Option.is_some (run_id_of_name target) ->
         let* kind = Retention_reader.kind reader ~path:name |> store in
         (match kind with
          | `File -> Ok count
          | `Directory -> corrupt "developer run temporary is a directory")
       | Some _ | None -> corrupt "unknown developer run authority file"))
;;

let close_and_wait t =
  let* () = Root.check_not_active t.root_owner |> store in
  Eio.Cancel.protect (fun () ->
    Root.release t.root_owner;
    t.closed <- true;
    Ok ())
;;

let open_or_create session_store ~sw ~limits ~checkpoint =
  let* root_owner = Root.claim session_store |> store in
  let keep = ref false in
  Exn.protect
    ~finally:(fun () -> if not !keep then Root.release root_owner)
    ~f:(fun () ->
      let* t =
        Root.with_lease root_owner ~f:(fun ~env ~root ->
          let path = Filename.concat (Data_root.path root) directory_name in
          let native = Eio.Path.(Eio.Stdenv.fs env / path) in
          let* () =
            try
              match Eio.Path.kind ~follow:false native with
              | `Not_found ->
                Eio.Path.mkdir ~perm:0o700 native;
                Ok ()
              | `Directory -> Ok ()
              | _ -> Error (Store_error.Corrupt "developer run root is linked or invalid")
            with
            | (Eio.Io _ | Core_unix.Unix_error _) as exn ->
              Error (Store_error.of_exn ~operation:"open developer run root" ~path exn)
          in
          (* A previous attempt may have created the child but failed while
             syncing this parent. Its existence alone does not prove the name
             survived a crash, so every open establishes the parent barrier. *)
          let* () = Durable_file.sync_directory ~env ~path:(Data_root.path root) in
          let directory = Eio.Path.open_dir ~sw native in
          Ok
            { root_owner
            ; host_id = Session_store.server_id session_store
            ; directory
            ; limits
            ; checkpoint
            ; token = ref ()
            ; closed = false
            })
        |> store
        |> Result.bind ~f:(Result.map_error ~f:(fun error -> Error.Store error))
      in
      let recovered =
        try
          with_lease t (fun ~env:_ ~root:_ ->
            let poll = work t checkpoint in
            scan t ~poll)
        with
        | Work_limit -> Error Error.Limit_exceeded
      in
      let* _count = recovered in
      Eio.Switch.on_release sw (fun () ->
        ignore (close_and_wait t : (unit, Error.t) result));
      keep := true;
      Ok t)
;;

let owner handle = handle.owner
let capture_fingerprint handle = handle.capture_fingerprint

let observe handle =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll = work t t.checkpoint in
    let* read = read t ~poll handle.owner.run_id in
    match read with
    | None -> Error Error.Foreign_or_stale
    | Some (bytes, document) ->
      let* observed = observation t ~owner:handle.owner ~bytes document in
      if
        Chatml_source.Digest.equal
          (Checkpoint.capture_fingerprint observed.checkpoint)
          handle.capture_fingerprint
      then Ok observed
      else Error Error.Foreign_or_stale)
;;

let same_observation (t : t) (handle : handle) (expected : Observation.t) bytes document =
  phys_equal expected.Observation.store_token t.token
  && P.Execution_owner.equal_developer expected.owner handle.owner
  && String.equal expected.document_bytes bytes
  && Int64.equal expected.document_revision (Doc.revision document)
  && Chatml_source.Digest.equal
       (Checkpoint.capture_fingerprint expected.checkpoint)
       handle.capture_fingerprint
;;

let admit_epoch t ~owner ~capture_fingerprint ~previous =
  let* () =
    P.Execution_owner.validate (P.Execution_owner.Developer owner)
    |> Result.map_error ~f:(fun _ -> Error.Foreign_or_stale)
  in
  let* () =
    if P.Id.Server.equal owner.host_id t.host_id
    then Ok ()
    else Error Error.Foreign_or_stale
  in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll = work t t.checkpoint in
    let* existing = read t ~poll owner.run_id in
    let* bytes, document =
      match existing with
      | None ->
        let* () = if Option.is_some previous then Error Error.Conflict else Ok () in
        let* count = scan t ~poll in
        let* () =
          if count < t.limits.max_runs then Ok () else Error Error.Limit_exceeded
        in
        let document = Doc.create ~owner ~capture_fingerprint in
        poll ();
        let* bytes =
          Doc.to_bytes
            document
            ~limits:t.limits.document
            ~max_epochs:t.limits.max_epochs_per_run
          |> store
        in
        poll ();
        let* () =
          Durable_file.replace_in
            ~directory:t.directory
            ~durability:Flush_file_and_directory
            ~basename:(basename owner.run_id)
            bytes
          |> store
        in
        Ok (bytes, document)
      | Some (bytes, document) ->
        let current = List.last_exn (Doc.epochs document) in
        let* () =
          if
            P.Id.Server.equal (Doc.host_id document) owner.host_id
            && P.Id.Principal.equal (Doc.principal_id document) owner.principal_id
          then Ok ()
          else Error Error.Foreign_or_stale
        in
        if Doc.epoch_number current = owner.epoch
        then
          if Chatml_source.Digest.equal (Doc.epoch_capture current) capture_fingerprint
          then Ok (bytes, document)
          else Error Error.Conflict
        else
          let* () =
            match previous with
            | None -> Error Error.Conflict
            | Some (expected : Observation.t) ->
              if
                phys_equal expected.store_token t.token
                && String.equal expected.document_bytes bytes
                && Int64.equal expected.document_revision (Doc.revision document)
                && P.Id.Developer_run.equal expected.owner.run_id owner.run_id
                && P.Id.Principal.equal expected.owner.principal_id owner.principal_id
                && P.Id.Server.equal expected.owner.host_id owner.host_id
                && expected.owner.epoch = Doc.epoch_number current
              then Ok ()
              else Error Error.Conflict
          in
          let* () =
            if List.length (Doc.epochs document) >= t.limits.max_epochs_per_run
            then Error Error.Limit_exceeded
            else Ok ()
          in
          let* next =
            Doc.append_epoch
              document
              ~owner
              ~capture_fingerprint
              ~max_epochs:t.limits.max_epochs_per_run
            |> store
          in
          poll ();
          let* after =
            Doc.to_bytes
              next
              ~limits:t.limits.document
              ~max_epochs:t.limits.max_epochs_per_run
            |> store
          in
          poll ();
          let* () =
            Durable_file.replace_in
              ~directory:t.directory
              ~durability:Flush_file_and_directory
              ~basename:(basename owner.run_id)
              after
            |> store
          in
          Ok (after, next)
    in
    let handle = { store = t; owner; capture_fingerprint; token = ref () } in
    let* observed = observation t ~owner ~bytes document in
    Ok (handle, observed))
;;

let prepare_checkpoint handle ~expected ~phase =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll = work t t.checkpoint in
    let* existing = read t ~poll handle.owner.run_id in
    match existing with
    | None -> Error Error.Foreign_or_stale
    | Some (before_bytes, document) ->
      if not (same_observation t handle expected before_bytes document)
      then Error Error.Conflict
      else
        let* epoch = find_epoch document ~owner:handle.owner in
        let current = List.last_exn (Doc.epochs document) in
        let* () =
          match Doc.epoch_phase epoch with
          | `Closed -> Error Error.Conflict
          | `Active | `Interrupted ->
            if Doc.epoch_number current = handle.owner.epoch
            then Ok ()
            else (
              match phase with
              | Checkpoint.Active -> Error Error.Foreign_or_stale
              | Interrupted | Closed -> Ok ())
        in
        let* next =
          Doc.set_phase document ~epoch:handle.owner.epoch ~phase:(phase_to_doc phase)
          |> store
        in
        poll ();
        let* after_bytes =
          Doc.to_bytes
            next
            ~limits:t.limits.document
            ~max_epochs:t.limits.max_epochs_per_run
          |> store
        in
        poll ();
        Ok
          Prepared.
            { store_token = t.token
            ; handle_token = handle.token
            ; owner = handle.owner
            ; before_bytes
            ; after_bytes
            ; after_document = next
            })
;;

let prepared_for handle prepared =
  phys_equal prepared.Prepared.store_token handle.store.token
  && phys_equal prepared.handle_token handle.token
  && P.Execution_owner.equal_developer prepared.owner handle.owner
;;

let installed t prepared =
  let* observed =
    observation
      t
      ~owner:prepared.Prepared.owner
      ~bytes:prepared.after_bytes
      prepared.after_document
  in
  Ok Installed.{ observation = observed }
;;

let commit handle prepared =
  let* t = valid_handle handle in
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else
    with_lease t (fun ~env:_ ~root:_ ->
      let poll = work t t.checkpoint in
      let* existing = read t ~poll handle.owner.run_id in
      match existing with
      | None -> Error Error.Conflict
      | Some (bytes, _) when not (String.equal bytes prepared.before_bytes) ->
        Error Error.Conflict
      | Some _ ->
        poll ();
        let* () =
          Durable_file.replace_in
            ~directory:t.directory
            ~durability:Flush_file_and_directory
            ~basename:(basename handle.owner.run_id)
            prepared.after_bytes
          |> store
        in
        poll ();
        installed t prepared)
;;

let reconcile handle prepared =
  let* t = valid_handle handle in
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else
    with_lease t (fun ~env:_ ~root:_ ->
      let poll = work t t.checkpoint in
      let* existing = read t ~poll handle.owner.run_id in
      match existing with
      | None -> Ok Diverged
      | Some (bytes, _) when String.equal bytes prepared.after_bytes ->
        Result.map (installed t prepared) ~f:(fun value -> Installed value)
      | Some (bytes, _) when String.equal bytes prepared.before_bytes -> Ok Absent
      | Some _ -> Ok Diverged)
;;
