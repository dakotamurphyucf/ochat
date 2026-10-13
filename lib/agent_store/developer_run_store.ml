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
    | Prewrite_conflict
    | Foreign_or_stale
    | Limit_exceeded
    | Closed
  [@@deriving sexp_of]
end

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
    ; document : Doc.t
    }

  let checkpoint t = t.checkpoint
end

module Invocation_observation = struct
  type t =
    { revision : int
    ; value : P.Host_invocation.t
    }

  let revision t = t.revision
  let value t = t.value
end

module Invocation_lookup = struct
  type t =
    { observation : Observation.t
    ; id : P.Id.Invocation.t
    ; prior : Invocation_observation.t option
    }

  let prior t = t.prior
end

let find_invocation observation id =
  let prior =
    Option.map (Doc.find_invocation observation.Observation.document id) ~f:(fun entry ->
      Invocation_observation.
        { revision = Doc.Invocation_entry.revision entry
        ; value = Doc.Invocation_entry.value entry
        })
  in
  { Invocation_lookup.observation; id; prior }
;;

let find_job observation id = Doc.find_job observation.Observation.document id

module Job_lookup = struct
  type t =
    { observation : Observation.t
    ; id : P.Id.Job.t
    ; value : P.Developer_job.t option
    }

  let value t = t.value
end

let find_job_exact observation id =
  Job_lookup.{ observation; id; value = find_job observation id }
;;

type entry_state =
  | Unentered
  | Entered
  | Abandoned
[@@deriving equal]

type claim_data =
  { store_token : unit ref
  ; handle_token : unit ref
  ; owner : P.Execution_owner.developer
  ; job : P.Developer_job.t
  ; entry_state : entry_state Atomic.t
  ; mutable issuing_domain : int option
  ; current_read : claim_data -> (bool, Error.t) result
  }

module Prepared = struct
  type kind =
    | Foundation_checkpoint
    | Developer_dispatch
    | Selected_transaction
    | Worker_claim
    | Claimed_transition

  type claim_status =
    | Unsettled
    | Claim_installed
    | Claim_absent
    | Claim_diverged
    | Claim_prewrite_conflict

  type t =
    { store_token : unit ref
    ; handle_token : unit ref
    ; owner : P.Execution_owner.developer
    ; before_bytes : string
    ; after_bytes : string
    ; after_document : Doc.t
    ; token : unit ref
    ; kind : kind
    ; claim_data : claim_data option
    ; mutable claim_status : claim_status
    ; mutable write_attempted : bool
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

module Settlement = struct
  type t =
    | Installed of Installed.t
    | Absent
    | Prewrite_conflict
    | Diverged
end

module Handoff = struct
  type t =
    { settle : Settlement.t -> unit
    ; mutable bound : unit ref option
    ; mutable settled : Settlement.t option
    }

  let create ~settle = { settle; bound = None; settled = None }
end

type pending =
  | Selected of
      { prepared : Prepared.t
      ; handoff : Handoff.t
      }
  | Claim of Prepared.t
  | Transition of Prepared.t

type run_claim_record =
  { token : unit ref
  ; handle_token : unit ref
  ; domain : int
  ; mutable released : bool
  }

type t =
  { root_owner : Root.owner
  ; host_id : P.Id.Server.t
  ; directory : Eio.Fs.dir_ty Eio.Path.t
  ; limits : Limits.t
  ; checkpoint : unit -> unit
  ; token : unit ref
  ; pending : (Id.t, pending) Hashtbl.t
  ; run_lifecycle_mutex : Stdlib.Mutex.t
  ; run_claims : (Id.t, run_claim_record) Hashtbl.t
  ; admissions_in_flight : (Id.t, unit) Hashtbl.t
  ; closing : bool Atomic.t
  ; closed : bool Atomic.t
  }

type handle =
  { store : t
  ; owner : P.Execution_owner.developer
  ; capture_fingerprint : Chatml_source.Digest.t
  ; token : unit ref
  }

type store = t

let with_run_lifecycle t f =
  Stdlib.Mutex.lock t.run_lifecycle_mutex;
  Exn.protect ~f ~finally:(fun () -> Stdlib.Mutex.unlock t.run_lifecycle_mutex)
;;

let pending_mem t run_id = with_run_lifecycle t (fun () -> Hashtbl.mem t.pending run_id)
let ( let* ) result f = Result.bind result ~f
let store result = Result.map_error result ~f:(fun error -> Error.Store error)
let corrupt message = Error (Error.Store (Store_error.Corrupt message))

exception Work_limit
exception Selected_admission_revoked

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
    ; document
    }
  in
  Ok observed
;;

let valid_handle handle =
  let t = handle.store in
  if Atomic.get t.closed || Atomic.get t.closing then Error Error.Closed else Ok t
;;

module Run_claim = struct
  type nonrec t =
    { store : t
    ; run_id : Id.t
    ; record : run_claim_record
    }

  let acquire ~(store : store) ~(handle : handle) =
    if not (phys_equal handle.store store && phys_equal handle.store.token store.token)
    then Error Error.Foreign_or_stale
    else
      with_run_lifecycle store (fun () ->
        let run_id = handle.owner.run_id in
        if Atomic.get store.closed || Atomic.get store.closing
        then Error Error.Closed
        else if
          Hashtbl.mem store.run_claims run_id
          || Hashtbl.mem store.admissions_in_flight run_id
          || Hashtbl.mem store.pending run_id
        then Error Error.Conflict
        else (
          let record =
            { token = ref ()
            ; handle_token = handle.token
            ; domain = (Stdlib.Domain.self () :> int)
            ; released = false
            }
          in
          Hashtbl.set store.run_claims ~key:run_id ~data:record;
          Ok { store; run_id; record }))
  ;;

  let release t =
    with_run_lifecycle t.store (fun () ->
      if not (Int.equal t.record.domain (Stdlib.Domain.self () :> int))
      then Error Error.Foreign_or_stale
      else if t.record.released
      then Ok ()
      else (
        match Hashtbl.find t.store.run_claims t.run_id with
        | Some record when phys_equal record.token t.record.token ->
          if Hashtbl.mem t.store.pending t.run_id
          then Error Error.Conflict
          else (
            Hashtbl.remove t.store.run_claims t.run_id;
            t.record.released <- true;
            Ok ())
        | Some _ | None -> Error Error.Foreign_or_stale))
  ;;
end

let with_lease t f =
  if Atomic.get t.closed || Atomic.get t.closing
  then Error Error.Closed
  else (
    try
      Root.with_lease t.root_owner ~f:(fun ~env ~root ->
        if Atomic.get t.closed || Atomic.get t.closing
        then Error Error.Closed
        else f ~env ~root)
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
      Doc.of_bytes
        bytes
        ~poll
        ~limits:t.limits.document
        ~max_epochs:t.limits.max_epochs_per_run
        ~max_jobs:t.limits.max_jobs_per_run
        ~max_invocations:t.limits.max_invocations_per_run
        ~max_job_payload_bytes:t.limits.max_job_payload_bytes
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

let claim_current_under_lease t ~poll ~for_entry (claim : claim_data) =
  let* current = read t ~poll claim.owner.run_id in
  match current with
  | None -> Error Error.Foreign_or_stale
  | Some (_, document) ->
    let* epoch = find_epoch document ~owner:claim.owner in
    let latest = List.last (Doc.epochs document) in
    let same_epoch =
      Option.value_map latest ~default:false ~f:(fun latest ->
        Int.equal (Doc.epoch_number latest) claim.owner.epoch)
    in
    let same_capture =
      Chatml_source.Digest.equal
        (Doc.epoch_capture epoch)
        (P.Developer_job.capture_fingerprint claim.job)
    in
    let same_job =
      Option.value_map
        (Doc.find_job document (P.Developer_job.id claim.job))
        ~default:false
        ~f:(fun current ->
          let state = P.Developer_job.state current in
          P.Developer_job.same_immutable current claim.job
          && Int.equal state.attempt (P.Developer_job.state claim.job).attempt
          &&
          match state.status with
          | P.Job.Running -> true
          | Waiting_permission _ -> not for_entry
          | Queued
          | Waiting_completion _
          | Succeeded
          | Failed _
          | Cancelled
          | Interrupted _ -> false)
    in
    let active =
      match Doc.epoch_phase epoch with
      | `Active -> true
      | `Interrupted | `Closed -> false
    in
    Ok (same_epoch && same_capture && same_job && active)
;;

module Claim = struct
  type t = claim_data

  let owner (t : t) = t.owner
  let job (t : t) = t.job
  let attempt (t : t) = (P.Developer_job.state t.job).attempt

  let current_for_effect t =
    if equal_entry_state (Atomic.get t.entry_state) Entered
    then t.current_read t
    else Ok false
  ;;

  let try_enter claim ~handle ~admit ~on_enter =
    let* t = valid_handle handle in
    if
      (not (phys_equal claim.store_token t.token))
      || (not (phys_equal claim.handle_token handle.token))
      || (not (P.Execution_owner.equal_developer claim.owner handle.owner))
      ||
      match claim.issuing_domain with
      | None -> true
      | Some domain -> not (Int.equal domain (Stdlib.Domain.self () :> int))
    then Error Error.Foreign_or_stale
    else
      with_lease t (fun ~env:_ ~root:_ ->
        let poll = work t t.checkpoint in
        let* current = claim_current_under_lease t ~poll ~for_entry:true claim in
        if current && admit ()
        then (
          let entered = Atomic.compare_and_set claim.entry_state Unentered Entered in
          if entered then on_enter ();
          Ok entered)
        else Error Error.Foreign_or_stale)
  ;;

  let abandon_before_callback claim ~handle =
    let* t = valid_handle handle in
    if
      (not (phys_equal claim.store_token t.token))
      || (not (phys_equal claim.handle_token handle.token))
      || (not (P.Execution_owner.equal_developer claim.owner handle.owner))
      ||
      match claim.issuing_domain with
      | None -> true
      | Some domain -> not (Int.equal domain (Stdlib.Domain.self () :> int))
    then Error Error.Foreign_or_stale
    else (
      ignore (Atomic.compare_and_set claim.entry_state Entered Abandoned : bool);
      Ok ())
  ;;
end

module Claimed = struct
  type t =
    { claim : Claim.t
    ; observation : Observation.t
    }

  let claim t = t.claim
  let observation t = t.observation
end

type claim_reconciliation =
  | Claimed of Claimed.t
  | Claim_absent
  | Claim_diverged

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
          ~poll
          ~limits:t.limits.document
          ~max_epochs:t.limits.max_epochs_per_run
          ~max_jobs:t.limits.max_jobs_per_run
          ~max_invocations:t.limits.max_invocations_per_run
          ~max_job_payload_bytes:t.limits.max_job_payload_bytes
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
  let* () =
    with_run_lifecycle t (fun () ->
      if
        (not (Hashtbl.is_empty t.run_claims))
        || not (Hashtbl.is_empty t.admissions_in_flight)
      then Error Error.Conflict
      else (
        Atomic.set t.closing true;
        Ok ()))
  in
  Eio.Cancel.protect (fun () ->
    Root.release t.root_owner;
    Atomic.set t.closed true;
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
            ; pending = Hashtbl.create (module Id)
            ; run_lifecycle_mutex = Stdlib.Mutex.create ()
            ; run_claims = Hashtbl.create (module Id)
            ; admissions_in_flight = Hashtbl.create (module Id)
            ; closing = Atomic.make false
            ; closed = Atomic.make false
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
        match close_and_wait t with
        | Ok () -> ()
        | Error _ -> failwith "developer run store shutdown is incomplete");
      keep := true;
      Ok t)
;;

let owner handle = handle.owner

let issued_by t handle =
  phys_equal t handle.store && phys_equal t.token handle.store.token
;;

let is_latest_active handle =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll = work t t.checkpoint in
    let* current = read t ~poll handle.owner.run_id in
    match current with
    | None -> Error Error.Foreign_or_stale
    | Some (_, document) ->
      Ok
        (Option.value_map
           (List.last (Doc.epochs document))
           ~default:false
           ~f:(fun epoch ->
             Int.equal (Doc.epoch_number epoch) handle.owner.epoch
             &&
             match Doc.epoch_phase epoch with
             | `Active -> true
             | `Interrupted | `Closed -> false)))
;;

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
  let* () =
    with_run_lifecycle t (fun () ->
      if Atomic.get t.closed || Atomic.get t.closing
      then Error Error.Closed
      else if
        Hashtbl.mem t.run_claims owner.run_id
        || Hashtbl.mem t.admissions_in_flight owner.run_id
      then Error Error.Conflict
      else (
        Hashtbl.set t.admissions_in_flight ~key:owner.run_id ~data:();
        Ok ()))
  in
  Exn.protect
    ~finally:(fun () ->
      with_run_lifecycle t (fun () -> Hashtbl.remove t.admissions_in_flight owner.run_id))
    ~f:(fun () ->
      with_lease t (fun ~env:_ ~root:_ ->
        let* () = if pending_mem t owner.run_id then Error Error.Conflict else Ok () in
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
                ~poll
                ~limits:t.limits.document
                ~max_epochs:t.limits.max_epochs_per_run
                ~max_jobs:t.limits.max_jobs_per_run
                ~max_invocations:t.limits.max_invocations_per_run
                ~max_job_payload_bytes:t.limits.max_job_payload_bytes
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
              if
                Chatml_source.Digest.equal (Doc.epoch_capture current) capture_fingerprint
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
                  ~poll
                  ~limits:t.limits.document
                  ~max_epochs:t.limits.max_epochs_per_run
                  ~max_jobs:t.limits.max_jobs_per_run
                  ~max_invocations:t.limits.max_invocations_per_run
                  ~max_job_payload_bytes:t.limits.max_job_payload_bytes
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
        Ok (handle, observed)))
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
            ~poll
            ~limits:t.limits.document
            ~max_epochs:t.limits.max_epochs_per_run
            ~max_jobs:t.limits.max_jobs_per_run
            ~max_invocations:t.limits.max_invocations_per_run
            ~max_job_payload_bytes:t.limits.max_job_payload_bytes
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
            ; token = ref ()
            ; kind = Foundation_checkpoint
            ; claim_data = None
            ; claim_status = Unsettled
            ; write_attempted = false
            })
;;

let prepare_change handle ~expected ~kind ~f =
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
        let* () =
          match List.last (Doc.epochs document) with
          | Some epoch
            when Int.equal (Doc.epoch_number epoch) handle.owner.epoch
                 &&
                 match Doc.epoch_phase epoch with
                 | `Active -> true
                 | `Interrupted | `Closed -> false -> Ok ()
          | None | Some _ -> Error Error.Foreign_or_stale
        in
        let* next = f t document ~poll in
        poll ();
        let* after_bytes =
          Doc.to_bytes
            next
            ~poll
            ~limits:t.limits.document
            ~max_epochs:t.limits.max_epochs_per_run
            ~max_jobs:t.limits.max_jobs_per_run
            ~max_invocations:t.limits.max_invocations_per_run
            ~max_job_payload_bytes:t.limits.max_job_payload_bytes
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
            ; token = ref ()
            ; kind
            ; claim_data = None
            ; claim_status = Unsettled
            ; write_attempted = false
            })
;;

let matching_lookup expected lookup id =
  phys_equal lookup.Invocation_lookup.observation expected
  && P.Id.Invocation.equal lookup.id id
;;

let transition t handle ~poll =
  Developer_run_transition.create
    ~owner:handle.owner
    ~capture_fingerprint:handle.capture_fingerprint
    ~max_jobs:t.limits.max_jobs_per_run
    ~max_invocations:t.limits.max_invocations_per_run
    ~max_selected:t.limits.max_selected_per_commit
    ~max_job_payload_bytes:t.limits.max_job_payload_bytes
    ~limits:t.limits.document
    ~poll
;;

let transition_result result =
  Result.map_error result ~f:(function
    | Developer_run_transition.Error.Conflict -> Error.Conflict
    | Limit_exceeded -> Error.Limit_exceeded
    | Store error -> Error.Store error)
;;

let observed_lookup lookup =
  Option.map lookup.Invocation_lookup.prior ~f:(fun prior ->
    Invocation_observation.revision prior, Invocation_observation.value prior)
;;

let prepare_dispatch handle ~expected ~lookup ~dispatched =
  let context = P.Host_invocation.context dispatched in
  if not (matching_lookup expected lookup context.id)
  then Error Error.Conflict
  else
    prepare_change handle ~expected ~kind:Developer_dispatch ~f:(fun t document ~poll ->
      Developer_run_transition.dispatch
        (transition t handle ~poll)
        document
        ~observed:(observed_lookup lookup)
        ~dispatched
      |> transition_result)
;;

let prepare_root_selected handle ~expected ~phase ~selected =
  prepare_change handle ~expected ~kind:Selected_transaction ~f:(fun t document ~poll ->
    Developer_run_transition.root_selected
      (transition t handle ~poll)
      document
      ~phase:(phase_to_doc phase)
      ~selected
    |> transition_result)
;;

let prepare_terminal_selected handle ~expected ~lookup ~terminal ~selected =
  let context = P.Host_invocation.context terminal in
  if not (matching_lookup expected lookup context.id)
  then Error Error.Conflict
  else
    prepare_change handle ~expected ~kind:Selected_transaction ~f:(fun t document ~poll ->
      Developer_run_transition.terminal_selected
        (transition t handle ~poll)
        document
        ~observed:(observed_lookup lookup)
        ~terminal
        ~selected
      |> transition_result)
;;

let matching_job_lookup expected lookup id =
  phys_equal lookup.Job_lookup.observation expected && P.Id.Job.equal lookup.id id
;;

let prepare_job_claim handle ~expected ~lookup ~now =
  let* observed =
    match Job_lookup.value lookup with
    | None -> Error Error.Conflict
    | Some observed -> Ok observed
  in
  let id = P.Developer_job.id observed in
  if not (matching_job_lookup expected lookup id)
  then Error Error.Conflict
  else
    let* prepared =
      prepare_change handle ~expected ~kind:Worker_claim ~f:(fun t document ~poll ->
        Developer_run_transition.claim (transition t handle ~poll) document ~observed ~now
        |> transition_result)
    in
    let* claimed_job =
      match Doc.find_job prepared.after_document id with
      | None -> Error Error.Conflict
      | Some job -> Ok job
    in
    let t = handle.store in
    let current_read claim =
      with_lease t (fun ~env:_ ~root:_ ->
        let poll = work t t.checkpoint in
        claim_current_under_lease t ~poll ~for_entry:false claim)
    in
    let claim_data : claim_data =
      { store_token = t.token
      ; handle_token = handle.token
      ; owner = handle.owner
      ; job = claimed_job
      ; entry_state = Atomic.make Unentered
      ; issuing_domain = None
      ; current_read
      }
    in
    Ok { prepared with claim_data = Some claim_data }
;;

let prepare_claimed_transition_internal
      handle
      ~expected
      ~claim
      ~after
      ~revoked_after
      ~required_entry
  =
  let* t = valid_handle handle in
  if
    (not (phys_equal claim.store_token t.token))
    || (not (phys_equal claim.handle_token handle.token))
    || (not (P.Execution_owner.equal_developer claim.owner handle.owner))
    || (not (equal_entry_state (Atomic.get claim.entry_state) required_entry))
    ||
    match claim.issuing_domain with
    | None -> true
    | Some domain -> not (Int.equal domain (Stdlib.Domain.self () :> int))
  then Error Error.Foreign_or_stale
  else
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
          let latest_epoch =
            Option.value_map
              (List.last (Doc.epochs document))
              ~default:false
              ~f:(fun last -> Int.equal (Doc.epoch_number last) handle.owner.epoch)
          in
          let effect_active =
            latest_epoch
            &&
            match Doc.epoch_phase epoch with
            | `Active -> true
            | `Interrupted | `Closed -> false
          in
          let after = if effect_active then after else revoked_after in
          let* next =
            Developer_run_transition.claimed_transition
              (transition t handle ~poll)
              document
              ~claimed:claim.job
              ~after
              ~current_epoch:effect_active
            |> transition_result
          in
          poll ();
          let* after_bytes =
            Doc.to_bytes
              next
              ~poll
              ~limits:t.limits.document
              ~max_epochs:t.limits.max_epochs_per_run
              ~max_jobs:t.limits.max_jobs_per_run
              ~max_invocations:t.limits.max_invocations_per_run
              ~max_job_payload_bytes:t.limits.max_job_payload_bytes
            |> store
          in
          Ok
            Prepared.
              { store_token = t.token
              ; handle_token = handle.token
              ; owner = handle.owner
              ; before_bytes
              ; after_bytes
              ; after_document = next
              ; token = ref ()
              ; kind = Claimed_transition
              ; claim_data = None
              ; claim_status = Unsettled
              ; write_attempted = false
              })
;;

let prepare_claimed_transition handle ~expected ~claim ~after ~revoked_after =
  prepare_claimed_transition_internal
    handle
    ~expected
    ~claim
    ~after
    ~revoked_after
    ~required_entry:Entered
;;

let prepare_unentered_claim_cleanup handle ~expected ~claim ~status ~now =
  let* t = valid_handle handle in
  let* () =
    if
      phys_equal claim.store_token t.token
      && phys_equal claim.handle_token handle.token
      && P.Execution_owner.equal_developer claim.owner handle.owner
      &&
      match claim.issuing_domain with
      | Some domain -> Int.equal domain (Stdlib.Domain.self () :> int)
      | None -> false
    then Ok ()
    else Error Error.Foreign_or_stale
  in
  let* () =
    if
      Atomic.compare_and_set claim.entry_state Unentered Abandoned
      || equal_entry_state (Atomic.get claim.entry_state) Abandoned
    then Ok ()
    else Error Error.Conflict
  in
  let claimed = P.Developer_job.state claim.job in
  let status =
    match status with
    | `Cancelled -> P.Job.Cancelled
    | `Interrupted reason -> P.Job.Interrupted reason
  in
  let after : P.Developer_job.State.t =
    { claimed with status; completed_at = Some now; result = None; retry_failure = None }
  in
  prepare_claimed_transition_internal
    handle
    ~expected
    ~claim
    ~after
    ~revoked_after:after
    ~required_entry:Abandoned
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

let no_pending t run_id = if pending_mem t run_id then Error Error.Conflict else Ok ()

let bind_handoff t handle prepared handoff =
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else if
    match prepared.Prepared.kind with
    | Developer_dispatch | Selected_transaction -> false
    | Foundation_checkpoint | Worker_claim | Claimed_transition -> true
  then Error Error.Conflict
  else
    let* () =
      match handoff.Handoff.bound with
      | None ->
        handoff.bound <- Some prepared.token;
        Ok ()
      | Some token when phys_equal token prepared.token -> Ok ()
      | Some _ -> Error Error.Foreign_or_stale
    in
    match handoff.settled with
    | Some settled -> Ok (Some settled)
    | None ->
      let run_id = handle.owner.run_id in
      with_run_lifecycle t (fun () ->
        match Hashtbl.find t.pending run_id with
        | None ->
          Hashtbl.set t.pending ~key:run_id ~data:(Selected { prepared; handoff });
          Ok None
        | Some (Selected pending)
          when phys_equal pending.prepared.token prepared.token
               && phys_equal pending.handoff handoff -> Ok None
        | Some (Selected _ | Claim _ | Transition _) -> Error Error.Conflict)
;;

let settle t handle prepared handoff value =
  let exact_pending () =
    match Hashtbl.find t.pending handle.owner.run_id with
    | Some (Selected pending) ->
      phys_equal pending.prepared.token prepared.Prepared.token
      && phys_equal pending.handoff handoff
    | None | Some (Claim _ | Transition _) -> false
  in
  if not (with_run_lifecycle t exact_pending)
  then Error Error.Conflict
  else (
    Eio.Cancel.protect (fun () -> handoff.Handoff.settle value);
    handoff.settled <- Some value;
    let still_owned =
      with_run_lifecycle t (fun () ->
        if exact_pending ()
        then (
          (match value with
           | Settlement.Installed _ | Absent | Prewrite_conflict ->
             Hashtbl.remove t.pending handle.owner.run_id
           | Diverged -> ());
          true)
        else false)
    in
    if still_owned then Ok value else Error Error.Conflict)
;;

let classify_prepared t handle prepared ~poll =
  let* existing = read t ~poll handle.owner.run_id in
  match existing with
  | Some (bytes, _) when String.equal bytes prepared.Prepared.after_bytes ->
    if prepared.write_attempted
    then
      Result.map (installed t prepared) ~f:(fun installed ->
        Settlement.Installed installed)
    else Ok Settlement.Prewrite_conflict
  | Some (bytes, _) when String.equal bytes prepared.before_bytes -> Ok Settlement.Absent
  | None | Some _ ->
    Ok (if prepared.write_attempted then Settlement.Diverged else Prewrite_conflict)
;;

let install_prepared ?(admit = fun () -> true) t handle prepared ~poll =
  let* existing = read t ~poll handle.owner.run_id in
  match existing with
  | Some (bytes, _) when String.equal bytes prepared.Prepared.after_bytes ->
    if prepared.write_attempted
    then
      Result.map (installed t prepared) ~f:(fun installed ->
        Settlement.Installed installed)
    else Ok Settlement.Prewrite_conflict
  | Some (bytes, _) when String.equal bytes prepared.before_bytes ->
    poll ();
    let* () =
      Durable_file.replace_in
        ~directory:t.directory
        ~durability:Flush_file_and_directory
        ~basename:(basename handle.owner.run_id)
        ~before_rename:(fun () ->
          if admit ()
          then prepared.write_attempted <- true
          else raise Selected_admission_revoked)
        prepared.after_bytes
      |> store
    in
    poll ();
    Result.map (installed t prepared) ~f:(fun installed -> Settlement.Installed installed)
  | None | Some _ ->
    Ok (if prepared.write_attempted then Settlement.Diverged else Prewrite_conflict)
;;

let reconcile_and_settle handle prepared handoff =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let* settled = bind_handoff t handle prepared handoff in
    match settled with
    | Some value -> Ok value
    | None ->
      let poll = work t t.checkpoint in
      let* value = classify_prepared t handle prepared ~poll in
      settle t handle prepared handoff value)
;;

let commit_and_settle handle prepared handoff ~admit =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let* settled = bind_handoff t handle prepared handoff in
    match settled with
    | Some value -> Ok value
    | None ->
      let poll = work t t.checkpoint in
      (match install_prepared ~admit t handle prepared ~poll with
       | Ok value -> settle t handle prepared handoff value
       | Error original ->
         (match
            Eio.Cancel.protect (fun () -> classify_prepared t handle prepared ~poll)
          with
          | Ok value -> settle t handle prepared handoff value
          | Error _ -> Error original)
       | exception Selected_admission_revoked ->
         (match
            Eio.Cancel.protect (fun () -> classify_prepared t handle prepared ~poll)
          with
          | Ok value ->
            let* _settled = settle t handle prepared handoff value in
            Error Error.Foreign_or_stale
          | Error original -> Error original)
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (try
            Eio.Cancel.protect (fun () ->
              match classify_prepared t handle prepared ~poll with
              | Ok value ->
                ignore
                  (settle t handle prepared handoff value
                   : (Settlement.t, Error.t) result)
              | Error _ -> ())
          with
          | _ -> ());
         Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let bind_claim t handle prepared =
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else (
    match prepared.Prepared.kind, prepared.claim_data with
    | Worker_claim, Some _ ->
      (match prepared.claim_status with
       | Claim_installed | Claim_absent | Claim_diverged | Claim_prewrite_conflict ->
         Ok ()
       | Unsettled ->
         let run_id = handle.owner.run_id in
         with_run_lifecycle t (fun () ->
           match Hashtbl.find t.pending run_id with
           | None ->
             Hashtbl.set t.pending ~key:run_id ~data:(Claim prepared);
             Ok ()
           | Some (Claim pending) when phys_equal pending.token prepared.token -> Ok ()
           | Some (Selected _ | Claim _ | Transition _) -> Error Error.Conflict))
    | ( ( Foundation_checkpoint
        | Developer_dispatch
        | Selected_transaction
        | Claimed_transition )
      , _ )
    | Worker_claim, None -> Error Error.Conflict)
;;

let settle_worker_prepared t handle (prepared : Prepared.t) settlement =
  let identical_unwritten_claim =
    match prepared.kind with
    | Worker_claim -> not prepared.write_attempted
    | Foundation_checkpoint
    | Developer_dispatch
    | Selected_transaction
    | Claimed_transition -> false
  in
  let status =
    match settlement with
    | Settlement.Installed _ when identical_unwritten_claim ->
      Prepared.Claim_prewrite_conflict
    | Settlement.Installed _ -> Prepared.Claim_installed
    | Absent -> Claim_absent
    | Prewrite_conflict -> Claim_prewrite_conflict
    | Diverged when not prepared.write_attempted -> Claim_prewrite_conflict
    | Diverged -> Claim_diverged
  in
  prepared.claim_status <- status;
  (match status with
   | Claim_installed ->
     Option.iter prepared.claim_data ~f:(fun claim ->
       claim.issuing_domain <- Some (Stdlib.Domain.self () :> int))
   | Unsettled | Claim_absent | Claim_diverged | Claim_prewrite_conflict -> ());
  let retire =
    match status, prepared.kind with
    | Claim_installed, (Worker_claim | Claimed_transition)
    | Claim_absent, Worker_claim
    | Claim_prewrite_conflict, (Worker_claim | Claimed_transition) -> true
    | (Unsettled | Claim_absent | Claim_diverged), _
    | Claim_installed, (Foundation_checkpoint | Developer_dispatch | Selected_transaction)
    | ( Claim_prewrite_conflict
      , (Foundation_checkpoint | Developer_dispatch | Selected_transaction) ) -> false
  in
  if retire
  then
    with_run_lifecycle t (fun () ->
      match Hashtbl.find t.pending handle.owner.run_id with
      | Some (Claim pending | Transition pending)
        when phys_equal pending.token prepared.token ->
        Hashtbl.remove t.pending handle.owner.run_id
      | None | Some (Selected _) | Some (Claim _ | Transition _) -> ());
  match status with
  | Claim_prewrite_conflict -> Error Error.Prewrite_conflict
  | Unsettled | Claim_installed | Claim_absent | Claim_diverged -> Ok ()
;;

let claim_result t prepared =
  match prepared.Prepared.claim_status, prepared.claim_data with
  | Claim_installed, Some claim_data ->
    let* value = installed t prepared in
    Ok (Claimed Claimed.{ claim = claim_data; observation = Installed.observation value })
  | Claim_absent, _ -> Ok Claim_absent
  | Claim_diverged, _ -> Ok Claim_diverged
  | Claim_prewrite_conflict, _ -> Error Error.Prewrite_conflict
  | Unsettled, _ | Claim_installed, None -> Error Error.Conflict
;;

let reconcile_job_claim handle prepared =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let* () = bind_claim t handle prepared in
    match prepared.claim_status with
    | Claim_installed | Claim_absent | Claim_diverged | Claim_prewrite_conflict ->
      claim_result t prepared
    | Unsettled ->
      let poll = work t t.checkpoint in
      let* settlement = classify_prepared t handle prepared ~poll in
      let* () = settle_worker_prepared t handle prepared settlement in
      claim_result t prepared)
;;

let commit_job_claim handle prepared =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let* () = bind_claim t handle prepared in
    let result () =
      let* value = claim_result t prepared in
      match value with
      | Claimed value -> Ok value
      | Claim_absent | Claim_diverged -> Error Error.Conflict
    in
    match prepared.claim_status with
    | Claim_installed | Claim_absent | Claim_diverged | Claim_prewrite_conflict ->
      result ()
    | Unsettled ->
      let poll = work t t.checkpoint in
      (match install_prepared t handle prepared ~poll with
       | Ok settlement ->
         let* () = settle_worker_prepared t handle prepared settlement in
         result ()
       | Error original ->
         (match
            Eio.Cancel.protect (fun () -> classify_prepared t handle prepared ~poll)
          with
          | Ok settlement ->
            let* () = settle_worker_prepared t handle prepared settlement in
            result ()
          | Error _ -> Error original)
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (try
            Eio.Cancel.protect (fun () ->
              match classify_prepared t handle prepared ~poll with
              | Ok settlement ->
                ignore
                  (settle_worker_prepared t handle prepared settlement
                   : (unit, Error.t) result)
              | Error _ -> ())
          with
          | _ -> ());
         Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let bind_claimed_transition t handle prepared =
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else (
    match prepared.Prepared.kind with
    | Claimed_transition ->
      (match prepared.claim_status with
       | Claim_installed | Claim_diverged | Claim_prewrite_conflict -> Ok ()
       | Unsettled | Claim_absent ->
         let run_id = handle.owner.run_id in
         with_run_lifecycle t (fun () ->
           match Hashtbl.find t.pending run_id with
           | None ->
             Hashtbl.set t.pending ~key:run_id ~data:(Transition prepared);
             Ok ()
           | Some (Transition pending) when phys_equal pending.token prepared.token ->
             Ok ()
           | Some (Selected _ | Claim _ | Transition _) -> Error Error.Conflict))
    | Foundation_checkpoint | Developer_dispatch | Selected_transaction | Worker_claim ->
      Error Error.Conflict)
;;

let claimed_transition_result t prepared =
  match prepared.Prepared.claim_status with
  | Claim_installed -> installed t prepared
  | Claim_prewrite_conflict -> Error Error.Prewrite_conflict
  | Unsettled | Claim_absent | Claim_diverged -> Error Error.Conflict
;;

let claimed_transition_reconciliation t prepared =
  match prepared.Prepared.claim_status with
  | Claim_installed -> Result.map (installed t prepared) ~f:(fun value -> Installed value)
  | Claim_absent -> Ok Absent
  | Claim_diverged -> Ok Diverged
  | Claim_prewrite_conflict -> Error Error.Prewrite_conflict
  | Unsettled -> Error Error.Conflict
;;

let reconcile_claimed_transition handle prepared =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let* () = bind_claimed_transition t handle prepared in
    match prepared.claim_status with
    | Claim_installed | Claim_diverged | Claim_prewrite_conflict ->
      claimed_transition_reconciliation t prepared
    | Unsettled | Claim_absent ->
      let poll = work t t.checkpoint in
      let* settlement = classify_prepared t handle prepared ~poll in
      let* () = settle_worker_prepared t handle prepared settlement in
      claimed_transition_reconciliation t prepared)
;;

let commit_claimed_transition handle prepared =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let* () = bind_claimed_transition t handle prepared in
    match prepared.claim_status with
    | Claim_installed | Claim_diverged | Claim_prewrite_conflict ->
      claimed_transition_result t prepared
    | Unsettled | Claim_absent ->
      let poll = work t t.checkpoint in
      (match install_prepared t handle prepared ~poll with
       | Ok settlement ->
         let* () = settle_worker_prepared t handle prepared settlement in
         claimed_transition_result t prepared
       | Error original ->
         (match
            Eio.Cancel.protect (fun () -> classify_prepared t handle prepared ~poll)
          with
          | Ok settlement ->
            ignore
              (settle_worker_prepared t handle prepared settlement
               : (unit, Error.t) result)
          | Error _ -> ());
         Error original
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (try
            Eio.Cancel.protect (fun () ->
              match classify_prepared t handle prepared ~poll with
              | Ok settlement ->
                ignore
                  (settle_worker_prepared t handle prepared settlement
                   : (unit, Error.t) result)
              | Error _ -> ())
          with
          | _ -> ());
         Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let commit handle prepared =
  let* t = valid_handle handle in
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else if
    match prepared.kind with
    | Foundation_checkpoint -> false
    | Developer_dispatch | Selected_transaction | Worker_claim | Claimed_transition ->
      true
  then Error Error.Conflict
  else
    with_lease t (fun ~env:_ ~root:_ ->
      let* () = no_pending t handle.owner.run_id in
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
  else if
    match prepared.kind with
    | Foundation_checkpoint -> false
    | Developer_dispatch | Selected_transaction | Worker_claim | Claimed_transition ->
      true
  then Error Error.Conflict
  else
    with_lease t (fun ~env:_ ~root:_ ->
      let* () = no_pending t handle.owner.run_id in
      let poll = work t t.checkpoint in
      let* existing = read t ~poll handle.owner.run_id in
      match existing with
      | None -> Ok Diverged
      | Some (bytes, _) when String.equal bytes prepared.after_bytes ->
        Result.map (installed t prepared) ~f:(fun value -> Installed value)
      | Some (bytes, _) when String.equal bytes prepared.before_bytes -> Ok Absent
      | Some _ -> Ok Diverged)
;;
