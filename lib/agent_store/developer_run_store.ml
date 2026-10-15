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

module Scheduler_candidate = struct
  type t =
    { job : P.Developer_job.t
    ; observation : Observation.t
    ; lookup : Job_lookup.t
    }

  let job t = t.job
  let observation t = t.observation
  let lookup t = t.lookup
end

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
  type claimed_selection =
    { selected : P.Developer_job.t list
    ; effect_active : bool
    }

  type kind =
    | Foundation_checkpoint
    | Developer_dispatch
    | Selected_transaction
    | External_model
    | Worker_claim
    | Claimed_transition
    | Inactive_cancel

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
    ; claimed_selection : claimed_selection option
    }
end

module Installed_data = struct
  type t = { observation : Observation.t }

  let observation t = t.observation
end

type reconciliation =
  | Installed of Installed_data.t
  | Absent
  | Diverged

module Settlement = struct
  type t =
    | Installed of Installed_data.t
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

type recovery_phase =
  | Recovery_unopened
  | Recovery_open of unit ref
  | Recovery_finished
  | Recovery_aborted

type run_claim_record =
  { token : unit ref
  ; handle_token : unit ref
  ; domain : int
  ; mutable released : bool
  ; mutable normal_started : bool
  ; mutable recovery : recovery_phase
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
  ; normal_operations : (Id.t, int) Hashtbl.t
  ; issued_claims : (Id.t, claim_data list) Hashtbl.t
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

module Installed = struct
  include Installed_data

  let issued_by installed (store : store) =
    phys_equal (observation installed).store_token store.token
  ;;
end

let with_run_lifecycle t f =
  Stdlib.Mutex.lock t.run_lifecycle_mutex;
  Exn.protect ~f ~finally:(fun () -> Stdlib.Mutex.unlock t.run_lifecycle_mutex)
;;

let pending_mem t run_id = with_run_lifecycle t (fun () -> Hashtbl.mem t.pending run_id)
let ( let* ) result f = Result.bind result ~f

let enter_normal t run_id =
  with_run_lifecycle t (fun () ->
    match Hashtbl.find t.run_claims run_id with
    | Some { recovery = Recovery_open _; _ } -> Error Error.Conflict
    | None | Some _ ->
      Option.iter (Hashtbl.find t.run_claims run_id) ~f:(fun claim ->
        claim.normal_started <- true);
      let count = Option.value (Hashtbl.find t.normal_operations run_id) ~default:0 in
      if Int.equal count Int.max_value
      then Error Error.Limit_exceeded
      else (
        Hashtbl.set t.normal_operations ~key:run_id ~data:(count + 1);
        Ok ()))
;;

let with_normal t run_id f =
  let* () = enter_normal t run_id in
  Exn.protect ~f ~finally:(fun () ->
    with_run_lifecycle t (fun () ->
      match Hashtbl.find t.normal_operations run_id with
      | Some count when count > 1 ->
        Hashtbl.set t.normal_operations ~key:run_id ~data:(count - 1)
      | Some _ | None -> Hashtbl.remove t.normal_operations run_id))
;;

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

let document_limits handle =
  Result.map (valid_handle handle) ~f:(fun t -> t.limits.document)
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
          || Hashtbl.mem store.normal_operations run_id
          || Hashtbl.mem store.issued_claims run_id
        then Error Error.Conflict
        else (
          let record =
            { token = ref ()
            ; handle_token = handle.token
            ; domain = (Stdlib.Domain.self () :> int)
            ; released = false
            ; normal_started = false
            ; recovery = Recovery_unopened
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
          if
            Hashtbl.mem t.store.pending t.run_id
            || Hashtbl.mem t.store.normal_operations t.run_id
            || Hashtbl.mem t.store.issued_claims t.run_id
            ||
            match t.record.recovery with
            | Recovery_open _ -> true
            | Recovery_unopened | Recovery_finished | Recovery_aborted -> false
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

let with_normal_lease t handle f =
  with_normal t handle.owner.run_id (fun () -> with_lease t f)
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

  let issued_by claim (store : store) = phys_equal claim.store_token store.token
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
      with_normal_lease t handle (fun ~env:_ ~root:_ ->
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

let scan_documents_with t ~poll ~reader ~decode ~document_of ~project =
  poll ();
  let* () = Durable_file.sync_directory_in ~directory:t.directory |> store in
  poll ();
  let* names =
    Retention_reader.list_with_checkpoint reader ~directory:"." ~checkpoint:poll |> store
  in
  let* _, documents =
    List.fold_result names ~init:(0, []) ~f:(fun (count, documents) name ->
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
        let* decoded =
          decode
            bytes
            ~poll
            ~limits:t.limits.document
            ~max_epochs:t.limits.max_epochs_per_run
            ~max_jobs:t.limits.max_jobs_per_run
            ~max_invocations:t.limits.max_invocations_per_run
            ~max_job_payload_bytes:t.limits.max_job_payload_bytes
          |> store
        in
        let document = document_of decoded in
        poll ();
        if
          Id.equal id (Doc.run_id document)
          && P.Id.Server.equal t.host_id (Doc.host_id document)
        then Ok (count + 1, project id bytes decoded :: documents)
        else corrupt "developer run authority file has another run ID"
      | None ->
        (match Durable_file.temporary_target name with
         | Some target when Option.is_some (run_id_of_name target) ->
           let* kind = Retention_reader.kind reader ~path:name |> store in
           (match kind with
            | `File -> Ok (count, documents)
            | `Directory -> corrupt "developer run temporary is a directory")
         | Some _ | None -> corrupt "unknown developer run authority file"))
  in
  Ok (List.rev documents)
;;

let scan_documents t ~poll ~reader =
  scan_documents_with
    t
    ~poll
    ~reader
    ~decode:Doc.of_bytes
    ~document_of:Fn.id
    ~project:(fun id _bytes document -> id, document)
;;

let scan_document_sources t ~poll ~reader =
  scan_documents_with
    t
    ~poll
    ~reader
    ~decode:Doc.of_bytes_with_document
    ~document_of:fst
    ~project:(fun id bytes (document, source) -> id, bytes, document, source)
;;

let scan t ~poll =
  let* reader =
    Retention_reader.create_at
      ~root:t.directory
      ~max_entries:t.limits.max_recovery_entries
      ~max_bytes:t.limits.max_recovery_bytes
    |> store
  in
  let* documents = scan_documents t ~poll ~reader in
  Ok (List.length documents)
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
            ; normal_operations = Hashtbl.create (module Id)
            ; issued_claims = Hashtbl.create (module Id)
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

let issued_by_session_store t session_store = Root.issued_by t.root_owner session_store
let is_closed t = Atomic.get t.closed || Atomic.get t.closing

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

let observe_with_poll t handle ~poll =
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
    else Error Error.Foreign_or_stale
;;

let observe handle =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll = work t t.checkpoint in
    observe_with_poll t handle ~poll)
;;

let observe_latest_active_with_poll t handle ~poll =
  let* observed = observe_with_poll t handle ~poll in
  match List.last (Doc.epochs observed.document) with
  | Some epoch
    when Int.equal (Doc.epoch_number epoch) handle.owner.epoch
         &&
         match Doc.epoch_phase epoch with
         | `Active -> true
         | `Interrupted | `Closed -> false -> Ok observed
  | None | Some _ -> Error Error.Foreign_or_stale
;;

let observe_latest_active handle =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll = work t t.checkpoint in
    observe_latest_active_with_poll t handle ~poll)
;;

let scheduler_candidates handle ~checkpoint =
  let* t = valid_handle handle in
  with_lease t (fun ~env:_ ~root:_ ->
    let poll =
      work t (fun () ->
        t.checkpoint ();
        checkpoint ())
    in
    let* observation = observe_latest_active_with_poll t handle ~poll in
    Doc.jobs observation.document
    |> List.filter_map ~f:(fun job ->
      poll ();
      if
        P.Execution_owner.equal_developer (P.Developer_job.owner job) handle.owner
        &&
        match (P.Developer_job.state job).status with
        | P.Job.Queued -> true
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ -> false
      then (
        poll ();
        let lookup =
          Job_lookup.{ observation; id = P.Developer_job.id job; value = Some job }
        in
        Some { Scheduler_candidate.job; observation; lookup })
      else None)
    |> Result.return)
;;

module Result_root = struct
  type nonrec t =
    { issuer : store
    ; store_token : unit ref
    ; host_id : P.Id.Server.t
    ; run_id : Id.t
    ; principal_id : P.Id.Principal.t
    ; epoch : (int * Chatml_source.Digest.t) option
    }

  let make issuer document ~epoch =
    { issuer
    ; store_token = issuer.token
    ; host_id = Doc.host_id document
    ; run_id = Doc.run_id document
    ; principal_id = Doc.principal_id document
    ; epoch
    }
  ;;

  let host_id t = t.host_id
  let run_id t = t.run_id
  let principal_id t = t.principal_id

  let issued_by issuer t =
    phys_equal issuer t.issuer && phys_equal issuer.token t.store_token
  ;;

  let of_handle handle =
    let* issuer = valid_handle handle in
    let* observed = observe handle in
    Ok
      (make
         issuer
         observed.Observation.document
         ~epoch:(Some (handle.owner.epoch, handle.capture_fingerprint)))
  ;;

  let find_existing issuer run_id =
    with_lease issuer (fun ~env:_ ~root:_ ->
      let poll = work issuer issuer.checkpoint in
      let* found = read issuer ~poll run_id in
      match found with
      | None -> Ok None
      | Some (_, document) ->
        if P.Id.Server.equal issuer.host_id (Doc.host_id document)
        then Ok (Some (make issuer document ~epoch:None))
        else corrupt "developer result root host differs from daemon owner")
  ;;

  let find_job_snapshot t ~owner ~capture_fingerprint ~job_id ~attempt =
    let valid_owner =
      P.Execution_owner.validate (P.Execution_owner.Developer owner) |> Result.is_ok
    in
    let valid_job = P.Id.Job.of_json (P.Id.Job.to_json job_id) |> Result.is_ok in
    if
      (not valid_owner)
      || (not valid_job)
      || attempt < 0
      || (not (issued_by t.issuer t))
      || (not (P.Id.Server.equal t.host_id owner.host_id))
      || (not (Id.equal t.run_id owner.run_id))
      || (not (P.Id.Principal.equal t.principal_id owner.principal_id))
      ||
      match t.epoch with
      | None -> false
      | Some (number, fingerprint) ->
        not
          (Int.equal number owner.epoch
           && Chatml_source.Digest.equal fingerprint capture_fingerprint)
    then Error Error.Foreign_or_stale
    else
      with_lease t.issuer (fun ~env:_ ~root:_ ->
        let poll = work t.issuer t.issuer.checkpoint in
        let* () = if pending_mem t.issuer t.run_id then Error Error.Conflict else Ok () in
        let* found = read t.issuer ~poll t.run_id in
        let* () = if pending_mem t.issuer t.run_id then Error Error.Conflict else Ok () in
        match found with
        | None -> Error Error.Foreign_or_stale
        | Some (_, document) ->
          let epoch =
            List.find (Doc.epochs document) ~f:(fun epoch ->
              poll ();
              Int.equal (Doc.epoch_number epoch) owner.epoch
              && Chatml_source.Digest.equal (Doc.epoch_capture epoch) capture_fingerprint)
          in
          if
            (not (P.Id.Server.equal t.host_id (Doc.host_id document)))
            || (not (Id.equal t.run_id (Doc.run_id document)))
            || (not (P.Id.Principal.equal t.principal_id (Doc.principal_id document)))
            || Option.is_none epoch
          then Error Error.Foreign_or_stale
          else (
            let job =
              List.find (Doc.jobs document) ~f:(fun job ->
                poll ();
                P.Id.Job.equal (P.Developer_job.id job) job_id)
            in
            match job with
            | None -> Ok None
            | Some job ->
              if
                P.Execution_owner.equal_developer (P.Developer_job.owner job) owner
                && Chatml_source.Digest.equal
                     (P.Developer_job.capture_fingerprint job)
                     capture_fingerprint
                && Int.equal (P.Developer_job.state job).attempt attempt
              then Ok (Some job)
              else Error Error.Foreign_or_stale))
  ;;

  let existing issuer =
    with_lease issuer (fun ~env:_ ~root:_ ->
      let poll = work issuer issuer.checkpoint in
      let* reader =
        Retention_reader.create_at
          ~root:issuer.directory
          ~max_entries:issuer.limits.max_recovery_entries
          ~max_bytes:issuer.limits.max_recovery_bytes
        |> store
      in
      let* documents = scan_documents issuer ~poll ~reader in
      Ok (List.map documents ~f:(fun (_, document) -> make issuer document ~epoch:None)))
  ;;

  let shared_reader issuer ~env reader =
    let open Result.Let_syntax in
    let%bind () =
      if Retention_reader.uses_filesystem reader ~env
      then Ok ()
      else Error (Store_error.Corrupt "retention reader has another filesystem owner")
    in
    let%bind reader =
      Retention_reader.with_local_limits
        reader
        ~max_entries:issuer.limits.max_recovery_entries
        ~max_bytes:issuer.limits.max_recovery_bytes
    in
    Retention_reader.at_root reader ~root:(Eio.Path.native_exn issuer.directory)
  ;;

  let existing_for_retention issuer ~reader =
    with_lease issuer (fun ~env ~root:_ ->
      let poll = work issuer issuer.checkpoint in
      let* reader = shared_reader issuer ~env reader |> store in
      let* documents = scan_documents issuer ~poll ~reader in
      Ok (List.map documents ~f:(fun (_, document) -> make issuer document ~epoch:None)))
  ;;

  let existing_for_result_inventory issuer ~reader ~checkpoint =
    with_lease issuer (fun ~env ~root:_ ->
      let store_poll = work issuer issuer.checkpoint in
      let poll () =
        store_poll ();
        checkpoint ()
      in
      let* reader = shared_reader issuer ~env reader |> store in
      let* documents = scan_documents issuer ~poll ~reader in
      Ok
        (List.map documents ~f:(fun (_, document) ->
           poll ();
           make issuer document ~epoch:None)))
  ;;

  let storage_error = function
    | Error.Store error -> error
    | Limit_exceeded ->
      Store_error.Admission_capacity
        (Document_schema.Error.Limit_exceeded "developer result root")
    | Conflict ->
      Store_error.Corrupt "developer result root conflicts with a pending write"
    | Prewrite_conflict ->
      Store_error.Corrupt "developer result root changed before write"
    | Foreign_or_stale -> Store_error.Corrupt "developer result root is foreign or stale"
    | Closed -> Store_error.Corrupt "developer result root Store is closed"
  ;;

  let matches t document =
    P.Id.Server.equal t.host_id (Doc.host_id document)
    && Id.equal t.run_id (Doc.run_id document)
    && P.Id.Principal.equal t.principal_id (Doc.principal_id document)
    &&
    match t.epoch with
    | None -> true
    | Some (number, fingerprint) ->
      List.exists (Doc.epochs document) ~f:(fun epoch ->
        Int.equal number (Doc.epoch_number epoch)
        && Chatml_source.Digest.equal fingerprint (Doc.epoch_capture epoch))
  ;;

  let with_borrow t ~reader ~checkpoint ~f =
    if not (issued_by t.issuer t)
    then Error (Store_error.Corrupt "developer result root has another physical issuer")
    else
      with_lease t.issuer (fun ~env ~root ->
        let store_poll = work t.issuer t.issuer.checkpoint in
        let poll () =
          store_poll ();
          checkpoint ()
        in
        let* bytes, document =
          match reader with
          | None ->
            let* found = read t.issuer ~poll t.run_id in
            (match found with
             | None -> Error Error.Foreign_or_stale
             | Some (bytes, document) -> Ok (bytes, document))
          | Some reader ->
            let* reader = shared_reader t.issuer ~env reader |> store in
            let* () =
              Durable_file.sync_directory_in ~directory:t.issuer.directory |> store
            in
            let* bytes =
              Retention_reader.read_with_checkpoint
                reader
                ~path:(basename t.run_id)
                ~max_bytes:t.issuer.limits.max_document_bytes
                ~checkpoint:poll
              |> store
            in
            let* document =
              Doc.of_bytes
                bytes
                ~poll
                ~limits:t.issuer.limits.document
                ~max_epochs:t.issuer.limits.max_epochs_per_run
                ~max_jobs:t.issuer.limits.max_jobs_per_run
                ~max_invocations:t.issuer.limits.max_invocations_per_run
                ~max_job_payload_bytes:t.issuer.limits.max_job_payload_bytes
              |> store
            in
            Ok (bytes, document)
        in
        if matches t document
        then
          f ~env ~root ~bytes ~document
          |> Result.map_error ~f:(fun error -> Error.Store error)
        else Error Error.Foreign_or_stale)
      |> Result.map_error ~f:storage_error
  ;;

  let no_checkpoint () = ()

  let with_lease t ~f =
    with_borrow
      t
      ~reader:None
      ~checkpoint:no_checkpoint
      ~f:(fun ~env:_ ~root ~bytes:_ ~document:_ -> f ~root)
  ;;

  let with_storage_lease t ~f =
    with_borrow
      t
      ~reader:None
      ~checkpoint:no_checkpoint
      ~f:(fun ~env ~root ~bytes:_ ~document:_ -> f ~env ~root)
  ;;

  let with_retention_lease t ~reader ~f =
    with_borrow
      t
      ~reader:(Some reader)
      ~checkpoint:no_checkpoint
      ~f:(fun ~env:_ ~root ~bytes:_ ~document:_ -> f ~root)
  ;;

  let with_result_inventory_lease t ~reader ~checkpoint ~f =
    with_borrow
      t
      ~reader:(Some reader)
      ~checkpoint
      ~f:(fun ~env:_ ~root ~bytes:_ ~document:_ -> f ~root)
  ;;

  module Inventory_rows = struct
    type t =
      { epochs : (int * Chatml_source.Digest.t) list
      ; jobs : P.Developer_job.t list
      }

    let epochs t = t.epochs
    let jobs t = t.jobs
  end

  let rows_for_inventory t ~reader ~checkpoint =
    with_borrow
      t
      ~reader:(Some reader)
      ~checkpoint
      ~f:(fun ~env:_ ~root:_ ~bytes:_ ~document ->
        let open Result.Let_syntax in
        let epochs = Doc.epochs document in
        let epoch_index = Hashtbl.create (module Int) in
        let%bind () =
          List.fold_result epochs ~init:() ~f:(fun () epoch ->
            checkpoint ();
            let number = Doc.epoch_number epoch in
            if Hashtbl.mem epoch_index number
            then
              Error (Store_error.Corrupt "developer result run repeats historical epoch")
            else (
              Hashtbl.set epoch_index ~key:number ~data:(Doc.epoch_capture epoch);
              Ok ()))
        in
        let%bind jobs =
          List.fold_result (Doc.jobs document) ~init:[] ~f:(fun acc job ->
            checkpoint ();
            let owner = P.Developer_job.owner job in
            if
              not
                (P.Id.Server.equal owner.host_id t.host_id
                 && Id.equal owner.run_id t.run_id
                 && P.Id.Principal.equal owner.principal_id t.principal_id)
            then Error (Store_error.Corrupt "developer result job has foreign owner")
            else (
              match Hashtbl.find epoch_index owner.epoch with
              | Some capture
                when Chatml_source.Digest.equal
                       (P.Developer_job.capture_fingerprint job)
                       capture -> Ok (job :: acc)
              | Some _ | None ->
                Error
                  (Store_error.Corrupt "developer result job has foreign epoch capture")))
          |> Result.map ~f:List.rev
        in
        let%map epochs =
          List.fold_result epochs ~init:[] ~f:(fun acc epoch ->
            checkpoint ();
            Ok ((Doc.epoch_number epoch, Doc.epoch_capture epoch) :: acc))
        in
        { Inventory_rows.epochs = List.rev epochs; jobs })
  ;;
end

module Retention = struct
  type run =
    { root : Result_root.t
    ; bytes : string
    ; source : Document_schema.Document.t
    ; epochs : (int * Chatml_source.Digest.t) list
    ; jobs : P.Developer_job.t list
    }

  type t =
    { issuer : store
    ; borrowed_root : Data_root.t
    ; env : Eio_unix.Stdenv.base
    ; runs : run list
    ; mutable active : bool
    }

  let runs t = t.runs
  let root_directory t = Data_root.path t.borrowed_root
  let result_root run = run.root
  let document_bytes run = run.bytes
  let epochs run = run.epochs
  let jobs run = run.jobs

  let iter_document_strings run ~checkpoint ~f =
    Document_fields.iter_strings_with_checkpoint
      (Document_schema.Document.json run.source)
      ~checkpoint
      ~f
  ;;

  let with_root t ~result_root ~f =
    if
      (not t.active)
      || (not (Result_root.issued_by t.issuer result_root))
      || not (List.exists t.runs ~f:(fun run -> phys_equal run.root result_root))
    then Error (Store_error.Corrupt "developer retention root is stale or foreign")
    else f ~env:t.env ~root:t.borrowed_root
  ;;

  let with_inventory issuer ~reader ~checkpoint ~f =
    with_lease issuer (fun ~env ~root ->
      let collect () =
        let open Result.Let_syntax in
        let store_poll = work issuer issuer.checkpoint in
        let poll () =
          store_poll ();
          checkpoint ()
        in
        let%bind reader = Result_root.shared_reader issuer ~env reader in
        let%bind sources =
          scan_document_sources issuer ~poll ~reader
          |> Result.map_error ~f:Result_root.storage_error
        in
        let%bind runs =
          List.fold_result sources ~init:[] ~f:(fun acc (_id, bytes, document, source) ->
            poll ();
            let epoch_index = Hashtbl.create (module Int) in
            let%bind epochs =
              List.fold_result (Doc.epochs document) ~init:[] ~f:(fun acc epoch ->
                poll ();
                let number = Doc.epoch_number epoch in
                if Hashtbl.mem epoch_index number
                then Error (Store_error.Corrupt "developer retention repeats an epoch")
                else (
                  let capture = Doc.epoch_capture epoch in
                  Hashtbl.set epoch_index ~key:number ~data:capture;
                  Ok ((number, capture) :: acc)))
              |> Result.map ~f:List.rev
            in
            let%bind jobs =
              List.fold_result (Doc.jobs document) ~init:[] ~f:(fun acc job ->
                poll ();
                let owner = P.Developer_job.owner job in
                if
                  not
                    (P.Id.Server.equal owner.host_id (Doc.host_id document)
                     && Id.equal owner.run_id (Doc.run_id document)
                     && P.Id.Principal.equal
                          owner.principal_id
                          (Doc.principal_id document))
                then
                  Error (Store_error.Corrupt "developer retention job has foreign owner")
                else (
                  match Hashtbl.find epoch_index owner.epoch with
                  | Some capture
                    when Chatml_source.Digest.equal
                           capture
                           (P.Developer_job.capture_fingerprint job) -> Ok (job :: acc)
                  | Some _ | None ->
                    Error
                      (Store_error.Corrupt "developer retention job has foreign epoch")))
              |> Result.map ~f:List.rev
            in
            let root = Result_root.make issuer document ~epoch:None in
            Ok ({ root; bytes; source; epochs; jobs } :: acc))
          |> Result.map ~f:List.rev
        in
        let borrow = { issuer; borrowed_root = root; env; runs; active = true } in
        Exn.protect
          ~finally:(fun () -> borrow.active <- false)
          ~f:(fun () ->
            poll ();
            let%map value = f borrow in
            poll ();
            value)
      in
      Result.map_error (collect ()) ~f:(fun error -> Error.Store error))
    |> Result.map_error ~f:Result_root.storage_error
  ;;
end

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
  with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
            ; claimed_selection = None
            })
;;

let prepare_change handle ~expected ~kind ~f =
  let* t = valid_handle handle in
  with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
          |> Result.map_error ~f:(fun error ->
            match kind, error with
            | ( Prepared.External_model
              , Store_error.Document (Document_schema.Error.Limit_exceeded _) ) ->
              Error.Limit_exceeded
            | _ -> Error.Store error)
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
            ; claimed_selection = None
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

module External_model_parent = struct
  type t =
    | Run_root
    | Dispatching of Invocation_lookup.t
end

let prepare_external_model handle ~expected ~parent ~job =
  let launch = P.Developer_job.launch job in
  let parent =
    match parent, P.Developer_job.Launch.parent launch with
    | External_model_parent.Run_root, Run_root ->
      Ok Developer_run_transition.External_model_parent.Run_root
    | Dispatching lookup, Invocation id when matching_lookup expected lookup id ->
      (match observed_lookup lookup with
       | Some observed ->
         let revision, value = observed in
         Ok (Developer_run_transition.External_model_parent.Dispatching (revision, value))
       | None -> Error Error.Conflict)
    | Run_root, Invocation _ | Dispatching _, Run_root | Dispatching _, Invocation _ ->
      Error Error.Conflict
  in
  let* parent = parent in
  prepare_change handle ~expected ~kind:External_model ~f:(fun t document ~poll ->
    Developer_run_transition.external_model
      (transition t handle ~poll)
      document
      ~parent
      ~job
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

let prepare_inactive_cancel handle ~expected ~lookup ~now =
  let* observed =
    match Job_lookup.value lookup with
    | None -> Error Error.Conflict
    | Some observed -> Ok observed
  in
  let id = P.Developer_job.id observed in
  if not (matching_job_lookup expected lookup id)
  then Error Error.Conflict
  else
    prepare_change handle ~expected ~kind:Inactive_cancel ~f:(fun t document ~poll ->
      Developer_run_transition.cancel_inactive
        (transition t handle ~poll)
        document
        ~observed
        ~now
      |> transition_result)
;;

let prepare_claimed_transition_internal
      handle
      ~expected
      ~claim
      ~after
      ~revoked_after
      ~required_entry
      ~selected
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
    with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
            (match selected with
             | None ->
               Developer_run_transition.claimed_transition
                 (transition t handle ~poll)
                 document
                 ~claimed:claim.job
                 ~after
                 ~current_epoch:effect_active
             | Some selected ->
               Developer_run_transition.claimed_selected
                 (transition t handle ~poll)
                 document
                 ~claimed:claim.job
                 ~after
                 ~selected
                 ~current_epoch:effect_active)
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
              ; claim_data = Some claim
              ; claim_status = Unsettled
              ; write_attempted = false
              ; claimed_selection =
                  Option.map selected ~f:(fun selected ->
                    Prepared.{ selected; effect_active })
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
    ~selected:None
;;

let prepare_claimed_selected handle ~expected ~claim ~after ~revoked_after ~selected =
  prepare_claimed_transition_internal
    handle
    ~expected
    ~claim
    ~after
    ~revoked_after
    ~required_entry:Entered
    ~selected:(Some selected)
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
    ~selected:None
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
    | Developer_dispatch | Selected_transaction | External_model -> false
    | Foundation_checkpoint | Worker_claim | Claimed_transition | Inactive_cancel -> true
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
  with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
  with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
        | External_model
        | Claimed_transition
        | Inactive_cancel )
      , _ )
    | Worker_claim, None -> Error Error.Conflict)
;;

let settle_worker_prepared t handle (prepared : Prepared.t) settlement =
  let identical_unwritten_claim =
    match prepared.kind with
    | Worker_claim | Inactive_cancel -> not prepared.write_attempted
    | Foundation_checkpoint
    | Developer_dispatch
    | Selected_transaction
    | External_model
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
    | Claim_installed, (Worker_claim | Claimed_transition | Inactive_cancel)
    | Claim_absent, (Worker_claim | Inactive_cancel)
    | Claim_prewrite_conflict, (Worker_claim | Claimed_transition | Inactive_cancel) ->
      true
    | (Unsettled | Claim_absent | Claim_diverged), _
    | ( Claim_installed
      , ( Foundation_checkpoint
        | Developer_dispatch
        | Selected_transaction
        | External_model ) )
    | ( Claim_prewrite_conflict
      , ( Foundation_checkpoint
        | Developer_dispatch
        | Selected_transaction
        | External_model ) ) -> false
  in
  with_run_lifecycle t (fun () ->
    let run_id = handle.owner.run_id in
    (match status, prepared.kind, prepared.claim_data with
     | Claim_installed, Worker_claim, Some claim ->
       let current = Option.value (Hashtbl.find t.issued_claims run_id) ~default:[] in
       if
         not
           (List.exists current ~f:(fun prior ->
              phys_equal prior.entry_state claim.entry_state))
       then Hashtbl.set t.issued_claims ~key:run_id ~data:(claim :: current)
     | Claim_installed, Claimed_transition, Some claim ->
       let id = P.Developer_job.id claim.job in
       (match Doc.find_job prepared.after_document id with
        | Some job ->
          (match (P.Developer_job.state job).status with
           | Running | Waiting_permission _ -> ()
           | Queued
           | Waiting_completion _
           | Succeeded
           | Failed _
           | Cancelled
           | Interrupted _ ->
             let current =
               Option.value (Hashtbl.find t.issued_claims run_id) ~default:[]
             in
             let remaining =
               List.filter current ~f:(fun prior ->
                 not (phys_equal prior.entry_state claim.entry_state))
             in
             if List.is_empty remaining
             then Hashtbl.remove t.issued_claims run_id
             else Hashtbl.set t.issued_claims ~key:run_id ~data:remaining)
        | None -> ())
     | ( ( Unsettled
         | Claim_absent
         | Claim_diverged
         | Claim_prewrite_conflict
         | Claim_installed )
       , _
       , _ ) -> ());
    if retire
    then (
      match Hashtbl.find t.pending run_id with
      | Some (Claim pending | Transition pending)
        when phys_equal pending.token prepared.token -> Hashtbl.remove t.pending run_id
      | None | Some (Selected _) | Some (Claim _ | Transition _) -> ()));
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
  with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
  with_normal_lease t handle (fun ~env:_ ~root:_ ->
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

type transition_path =
  | Physical_claim
  | Inactive_job

let bind_transition path t handle prepared =
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else (
    match path, prepared.Prepared.kind with
    | Physical_claim, Claimed_transition | Inactive_job, Inactive_cancel ->
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
    | ( (Physical_claim | Inactive_job)
      , ( Foundation_checkpoint
        | Developer_dispatch
        | Selected_transaction
        | External_model
        | Worker_claim
        | Claimed_transition
        | Inactive_cancel ) ) -> Error Error.Conflict)
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

let reconcile_transition_using borrow ~path handle prepared =
  let* t = valid_handle handle in
  borrow t handle (fun ~env:_ ~root:_ ->
    let* () = bind_transition path t handle prepared in
    match prepared.claim_status with
    | Claim_installed | Claim_diverged | Claim_prewrite_conflict ->
      claimed_transition_reconciliation t prepared
    | Unsettled | Claim_absent ->
      let poll = work t t.checkpoint in
      let* settlement = classify_prepared t handle prepared ~poll in
      let* () = settle_worker_prepared t handle prepared settlement in
      claimed_transition_reconciliation t prepared)
;;

let reconcile_claimed_transition handle prepared =
  reconcile_transition_using with_normal_lease ~path:Physical_claim handle prepared
;;

let reconcile_inactive_cancel handle prepared =
  reconcile_transition_using with_normal_lease ~path:Inactive_job handle prepared
;;

let commit_transition_using borrow ~path ~admit handle prepared =
  let* t = valid_handle handle in
  borrow t handle (fun ~env:_ ~root:_ ->
    let* () = bind_transition path t handle prepared in
    match prepared.claim_status with
    | Claim_installed | Claim_diverged | Claim_prewrite_conflict ->
      claimed_transition_result t prepared
    | Unsettled | Claim_absent ->
      let poll = work t t.checkpoint in
      (match install_prepared ~admit t handle prepared ~poll with
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
       | exception Selected_admission_revoked ->
         (match
            Eio.Cancel.protect (fun () -> classify_prepared t handle prepared ~poll)
          with
          | Ok settlement ->
            let* () = settle_worker_prepared t handle prepared settlement in
            Error Error.Foreign_or_stale
          | Error original -> Error original)
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

let commit_claimed_transition ?(admit = fun () -> true) handle prepared =
  commit_transition_using with_normal_lease ~path:Physical_claim ~admit handle prepared
;;

module Claimed_selection = struct
  type disposition =
    | Selected_installed
    | Selected_omitted_after_epoch_change
  [@@deriving sexp_of]

  let installed_disposition ~prepared ~installed =
    let observed = Installed.observation installed in
    match prepared.Prepared.claimed_selection, prepared.claim_status with
    | Some { selected; effect_active }, Claim_installed
      when phys_equal observed.store_token prepared.store_token
           && P.Execution_owner.equal_developer observed.owner prepared.owner
           && String.equal observed.document_bytes prepared.after_bytes
           && Int64.equal
                observed.document_revision
                (Doc.revision prepared.after_document) ->
      let exact_rows =
        List.for_all selected ~f:(fun selected_job ->
          match Doc.find_job observed.document (P.Developer_job.id selected_job) with
          | Some actual ->
            Jsonaf.exactly_equal
              (P.Developer_job.to_json selected_job)
              (P.Developer_job.to_json actual)
          | None -> false)
      in
      if List.is_empty selected || (effect_active && exact_rows)
      then Ok Selected_installed
      else if
        (not effect_active)
        && List.for_all selected ~f:(fun selected_job ->
          Option.is_none
            (Doc.find_job observed.document (P.Developer_job.id selected_job)))
      then Ok Selected_omitted_after_epoch_change
      else Error Error.Conflict
    | None, _
    | Some _, (Unsettled | Claim_absent | Claim_diverged | Claim_prewrite_conflict)
    | Some _, Claim_installed -> Error Error.Foreign_or_stale
  ;;
end

let commit_inactive_cancel ?(admit = fun () -> true) handle prepared =
  commit_transition_using with_normal_lease ~path:Inactive_job ~admit handle prepared
;;

let commit handle prepared =
  let* t = valid_handle handle in
  if not (prepared_for handle prepared)
  then Error Error.Foreign_or_stale
  else if
    match prepared.kind with
    | Foundation_checkpoint -> false
    | Developer_dispatch
    | Selected_transaction
    | External_model
    | Worker_claim
    | Claimed_transition
    | Inactive_cancel -> true
  then Error Error.Conflict
  else
    with_normal_lease t handle (fun ~env:_ ~root:_ ->
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
    | Developer_dispatch
    | Selected_transaction
    | External_model
    | Worker_claim
    | Claimed_transition
    | Inactive_cancel -> true
  then Error Error.Conflict
  else
    with_normal_lease t handle (fun ~env:_ ~root:_ ->
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

module Recovery = struct
  module Candidate = struct
    type t =
      { view_token : unit ref
      ; job : P.Developer_job.t
      }

    let job t = t.job
  end

  module View = struct
    type t =
      { token : unit ref
      ; window_token : unit ref
      ; observation : Observation.t
      ; candidates : Candidate.t list
      }

    let observation t = t.observation
    let candidates t = t.candidates
  end

  module Pending = struct
    type t =
      { job : P.Developer_job.t
      ; after : P.Developer_job.State.t
      ; prepared : Prepared.t
      ; window_token : unit ref
      ; mutable can_reprepare : bool
      }

    let job t = t.job
    let after t = t.after
  end

  type t =
    { store : store
    ; handle : handle
    ; claim : Run_claim.t
    ; token : unit ref
    ; mutable pending : Pending.t option
    ; mutable ever_prepared : bool
    ; mutable busy : bool
    }

  let open_ ~claim ~(handle : handle) =
    let store = claim.Run_claim.store in
    with_run_lifecycle store (fun () ->
      if Atomic.get store.closed || Atomic.get store.closing
      then Error Error.Closed
      else if
        (not (phys_equal handle.store store))
        || (not (phys_equal handle.token claim.record.handle_token))
        || claim.record.released
        || not (Int.equal claim.record.domain (Stdlib.Domain.self () :> int))
      then Error Error.Foreign_or_stale
      else (
        match Hashtbl.find store.run_claims claim.run_id, claim.record.recovery with
        | Some record, Recovery_unopened
          when phys_equal record.token claim.record.token
               && (not record.normal_started)
               && (not (Hashtbl.mem store.normal_operations claim.run_id))
               && (not (Hashtbl.mem store.issued_claims claim.run_id))
               && not (Hashtbl.mem store.pending claim.run_id) ->
          let token = ref () in
          record.recovery <- Recovery_open token;
          Ok
            { store
            ; handle
            ; claim
            ; token
            ; pending = None
            ; ever_prepared = false
            ; busy = false
            }
        | None, _
        | ( Some _
          , (Recovery_unopened | Recovery_open _ | Recovery_finished | Recovery_aborted) )
          -> Error Error.Conflict))
  ;;

  let valid t =
    with_run_lifecycle t.store (fun () ->
      if Atomic.get t.store.closed || Atomic.get t.store.closing
      then Error Error.Closed
      else if not (Int.equal t.claim.record.domain (Stdlib.Domain.self () :> int))
      then Error Error.Foreign_or_stale
      else (
        match Hashtbl.find t.store.run_claims t.claim.run_id with
        | Some record when phys_equal record.token t.claim.record.token ->
          (match record.recovery with
           | Recovery_open token when phys_equal token t.token -> Ok ()
           | Recovery_unopened | Recovery_open _ | Recovery_finished | Recovery_aborted ->
             Error Error.Foreign_or_stale)
        | Some _ | None -> Error Error.Foreign_or_stale))
  ;;

  let with_operation t f =
    try
      let* () = valid t in
      if t.busy
      then Error Error.Conflict
      else (
        t.busy <- true;
        Exn.protect
          ~f:(fun () ->
            let* () = valid t in
            f ())
          ~finally:(fun () -> t.busy <- false))
    with
    | Work_limit -> Error Error.Limit_exceeded
  ;;

  let read_view t ~poll =
    let* observation =
      with_lease t.store (fun ~env:_ ~root:_ -> observe_with_poll t.store t.handle ~poll)
    in
    let token = ref () in
    let* candidates =
      List.fold_result (Doc.jobs observation.document) ~init:[] ~f:(fun acc job ->
        poll ();
        match (P.Developer_job.state job).status with
        | Running | Waiting_permission _ ->
          let owner = P.Developer_job.owner job in
          let* epoch = find_epoch observation.document ~owner in
          if
            Chatml_source.Digest.equal
              (Doc.epoch_capture epoch)
              (P.Developer_job.capture_fingerprint job)
          then (
            let candidate : Candidate.t = { view_token = token; job } in
            Ok (candidate :: acc))
          else Error Error.Foreign_or_stale
        | Queued | Waiting_completion _ | Succeeded | Failed _ | Cancelled | Interrupted _
          -> Ok acc)
    in
    let* () = valid t in
    let view : View.t =
      { token; window_token = t.token; observation; candidates = List.rev candidates }
    in
    Ok view
  ;;

  let observe t =
    with_operation t (fun () ->
      let poll = work t.store t.store.checkpoint in
      read_view t ~poll)
  ;;

  let build_pending t ~view ~candidate ~after =
    if
      (not (phys_equal view.View.window_token t.token))
      || not (phys_equal candidate.Candidate.view_token view.View.token)
    then Error Error.Foreign_or_stale
    else (
      match after.P.Developer_job.State.status with
      | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
        Error Error.Conflict
      | Succeeded | Failed _ | Cancelled | Interrupted _ ->
        if Option.is_some after.retry_failure
        then Error Error.Conflict
        else
          with_lease t.store (fun ~env:_ ~root:_ ->
            let poll = work t.store t.store.checkpoint in
            let* current = read t.store ~poll t.claim.run_id in
            match current with
            | None -> Error Error.Foreign_or_stale
            | Some (before_bytes, document) ->
              if
                not
                  (same_observation
                     t.store
                     t.handle
                     view.observation
                     before_bytes
                     document)
              then Error Error.Conflict
              else (
                let job = candidate.job in
                let owner = P.Developer_job.owner job in
                let* epoch = find_epoch document ~owner in
                let* () =
                  if
                    Chatml_source.Digest.equal
                      (Doc.epoch_capture epoch)
                      (P.Developer_job.capture_fingerprint job)
                    && Option.value_map
                         (Doc.find_job document (P.Developer_job.id job))
                         ~default:false
                         ~f:(fun current ->
                           P.Developer_job.same_immutable current job
                           && Int.equal
                                (P.Developer_job.state current).attempt
                                (P.Developer_job.state job).attempt)
                  then Ok ()
                  else Error Error.Foreign_or_stale
                in
                let transition =
                  Developer_run_transition.create
                    ~owner
                    ~capture_fingerprint:(Doc.epoch_capture epoch)
                    ~max_jobs:t.store.limits.max_jobs_per_run
                    ~max_invocations:t.store.limits.max_invocations_per_run
                    ~max_selected:t.store.limits.max_selected_per_commit
                    ~max_job_payload_bytes:t.store.limits.max_job_payload_bytes
                    ~limits:t.store.limits.document
                    ~poll
                in
                let* next =
                  Developer_run_transition.claimed_transition
                    transition
                    document
                    ~claimed:job
                    ~after
                    ~current_epoch:false
                  |> transition_result
                in
                poll ();
                let* after_bytes =
                  Doc.to_bytes
                    next
                    ~poll
                    ~limits:t.store.limits.document
                    ~max_epochs:t.store.limits.max_epochs_per_run
                    ~max_jobs:t.store.limits.max_jobs_per_run
                    ~max_invocations:t.store.limits.max_invocations_per_run
                    ~max_job_payload_bytes:t.store.limits.max_job_payload_bytes
                  |> store
                in
                let* () = valid t in
                let prepared =
                  Prepared.
                    { store_token = t.store.token
                    ; handle_token = t.handle.token
                    ; owner = t.handle.owner
                    ; before_bytes
                    ; after_bytes
                    ; after_document = next
                    ; token = ref ()
                    ; kind = Claimed_transition
                    ; claim_data = None
                    ; claim_status = Unsettled
                    ; write_attempted = false
                    ; claimed_selection = None
                    }
                in
                let pending : Pending.t =
                  { job; after; prepared; window_token = t.token; can_reprepare = false }
                in
                Ok pending)))
  ;;

  let prepare_terminal t ~view ~candidate ~after =
    with_operation t (fun () ->
      match t.pending with
      | Some _ -> Error Error.Conflict
      | None ->
        let* pending = build_pending t ~view ~candidate ~after in
        t.pending <- Some pending;
        t.ever_prepared <- true;
        Ok pending)
  ;;

  let pending t = t.pending

  let same_pending t pending =
    Option.value_map t.pending ~default:false ~f:(fun current ->
      phys_equal current pending)
  ;;

  let reconcile_terminal t pending =
    with_operation t (fun () ->
      if not (same_pending t pending)
      then Error Error.Foreign_or_stale
      else (
        match
          reconcile_transition_using
            (fun store _handle f -> with_lease store f)
            ~path:Physical_claim
            t.handle
            pending.prepared
        with
        | Ok outcome ->
          (match outcome with
           | Installed _ -> t.pending <- None
           | Absent -> pending.can_reprepare <- true
           | Diverged -> ());
          Ok outcome
        | Error Error.Prewrite_conflict when not pending.prepared.write_attempted ->
          pending.can_reprepare <- true;
          Error Error.Prewrite_conflict
        | Error error -> Error error))
  ;;

  let commit_terminal t pending ~admit =
    with_operation t (fun () ->
      if not (same_pending t pending)
      then Error Error.Foreign_or_stale
      else
        let* installed =
          commit_transition_using
            (fun store _handle f -> with_lease store f)
            ~path:Physical_claim
            ~admit
            t.handle
            pending.prepared
        in
        t.pending <- None;
        Ok installed)
  ;;

  let reprepare_same t pending =
    with_operation t (fun () ->
      if not (same_pending t pending && pending.can_reprepare)
      then Error Error.Conflict
      else (
        let poll = work t.store t.store.checkpoint in
        let* view = read_view t ~poll in
        let candidate =
          List.find view.candidates ~f:(fun candidate ->
            poll ();
            P.Id.Job.equal
              (P.Developer_job.id candidate.job)
              (P.Developer_job.id pending.job)
            && P.Developer_job.same_immutable candidate.job pending.job
            && Int.equal
                 (P.Developer_job.state candidate.job).attempt
                 (P.Developer_job.state pending.job).attempt)
        in
        match candidate with
        | None -> Error Error.Conflict
        | Some candidate ->
          let* next = build_pending t ~view ~candidate ~after:pending.after in
          let* () =
            with_run_lifecycle t.store (fun () ->
              match Hashtbl.find t.store.pending t.claim.run_id with
              | Some (Transition old) when phys_equal old.token pending.prepared.token ->
                Hashtbl.remove t.store.pending t.claim.run_id;
                Ok ()
              | None -> Ok ()
              | Some (Selected _ | Claim _ | Transition _) -> Error Error.Conflict)
          in
          t.pending <- Some next;
          Ok next))
  ;;

  let finish t =
    with_operation t (fun () ->
      if Option.is_some t.pending
      then Error Error.Conflict
      else (
        let poll = work t.store t.store.checkpoint in
        let* view = read_view t ~poll in
        if not (List.is_empty view.candidates)
        then Error Error.Conflict
        else
          with_run_lifecycle t.store (fun () ->
            match t.claim.record.recovery with
            | Recovery_open token when phys_equal token t.token ->
              t.claim.record.recovery <- Recovery_finished;
              Ok ()
            | Recovery_unopened | Recovery_open _ | Recovery_finished | Recovery_aborted
              -> Error Error.Foreign_or_stale)))
  ;;

  let is_finished t =
    with_run_lifecycle t.store (fun () ->
      match Hashtbl.find t.store.run_claims t.claim.run_id with
      | Some record when phys_equal record.token t.claim.record.token ->
        (match record.recovery with
         | Recovery_finished -> true
         | Recovery_unopened | Recovery_open _ | Recovery_aborted -> false)
      | Some _ | None -> false)
  ;;

  let abort_clean t =
    with_operation t (fun () ->
      if t.ever_prepared || Option.is_some t.pending
      then Error Error.Conflict
      else
        with_run_lifecycle t.store (fun () ->
          match t.claim.record.recovery with
          | Recovery_open token when phys_equal token t.token ->
            t.claim.record.recovery <- Recovery_aborted;
            Ok ()
          | Recovery_unopened | Recovery_open _ | Recovery_finished | Recovery_aborted ->
            Error Error.Foreign_or_stale))
  ;;
end

module Result_recovery_affinity = struct
  let bound store ~root ~window ~view =
    Result_root.issued_by store root
    && phys_equal window.Recovery.store store
    && phys_equal view.Recovery.View.window_token window.token
    && Id.equal (Result_root.run_id root) window.claim.run_id
    && P.Id.Principal.equal
         (Result_root.principal_id root)
         window.handle.owner.principal_id
  ;;

  let with_charged_root root ~reader ~checkpoint ~f =
    Result_root.with_borrow
      root
      ~reader:(Some reader)
      ~checkpoint
      ~f:(fun ~env:_ ~root:_ ~bytes ~document -> f ~bytes ~document)
    |> Result.map_error ~f:(fun error -> Error.Store error)
  ;;

  let check_view store ~root ~window ~view ~reader ~checkpoint =
    Recovery.with_operation window (fun () ->
      if not (bound store ~root ~window ~view)
      then Error Error.Foreign_or_stale
      else
        with_charged_root root ~reader ~checkpoint ~f:(fun ~bytes ~document ->
          if same_observation store window.handle view.observation bytes document
          then Ok ()
          else Error (Store_error.Corrupt "developer result View changed")))
  ;;

  let check_pending
        store
        ~root
        ~window
        ~view
        ~candidate
        ~pending
        ~after
        ~reader
        ~checkpoint
    =
    Recovery.with_operation window (fun () ->
      if
        (not (bound store ~root ~window ~view))
        || (not (phys_equal candidate.Recovery.Candidate.view_token view.token))
        || (not (phys_equal pending.Recovery.Pending.window_token window.token))
        || (not (Recovery.same_pending window pending))
        || (not
              (List.exists view.candidates ~f:(fun current ->
                 phys_equal current candidate)))
        || (not (P.Developer_job.same_immutable candidate.job pending.job))
        || (not
              (Int.equal
                 (P.Developer_job.state candidate.job).attempt
                 (P.Developer_job.state pending.job).attempt))
        || not
             (Jsonaf.exactly_equal
                (P.Developer_job.State.to_json after)
                (P.Developer_job.State.to_json pending.after))
      then Error Error.Foreign_or_stale
      else
        with_charged_root root ~reader ~checkpoint ~f:(fun ~bytes ~document ->
          if
            not
              (String.equal bytes pending.prepared.before_bytes
               || String.equal bytes pending.prepared.after_bytes)
          then Error (Store_error.Corrupt "developer result Pending has a third revision")
          else (
            let original = pending.job in
            let owner = P.Developer_job.owner original in
            let* epoch =
              find_epoch document ~owner
              |> Result.map_error ~f:(fun _ ->
                Store_error.Corrupt "developer result Pending epoch changed")
            in
            if
              (not
                 (Chatml_source.Digest.equal
                    (Doc.epoch_capture epoch)
                    (P.Developer_job.capture_fingerprint original)))
              || not
                   (Option.value_map
                      (Doc.find_job document (P.Developer_job.id original))
                      ~default:false
                      ~f:(fun job ->
                        P.Developer_job.same_immutable job original
                        && Int.equal
                             (P.Developer_job.state job).attempt
                             (P.Developer_job.state original).attempt
                        && Jsonaf.exactly_equal
                             (P.Developer_job.State.to_json (P.Developer_job.state job))
                             (P.Developer_job.State.to_json
                                (if String.equal bytes pending.prepared.before_bytes
                                 then P.Developer_job.state original
                                 else after))))
            then Error (Store_error.Corrupt "developer result Pending job changed")
            else Ok ())))
  ;;

  let check_installed
        store
        ~root
        ~window
        ~view
        ~candidate
        ~pending
        ~after
        ~installed
        ~reader
        ~checkpoint
    =
    Recovery.with_operation window (fun () ->
      let observed = Installed.observation installed in
      if
        (not (bound store ~root ~window ~view))
        || (not (phys_equal candidate.Recovery.Candidate.view_token view.token))
        || (not (phys_equal pending.Recovery.Pending.window_token window.token))
        || (not (Installed.issued_by installed store))
        || (not (String.equal observed.document_bytes pending.prepared.after_bytes))
        || not
             (Jsonaf.exactly_equal
                (P.Developer_job.State.to_json after)
                (P.Developer_job.State.to_json pending.after))
      then Error Error.Foreign_or_stale
      else
        with_charged_root root ~reader ~checkpoint ~f:(fun ~bytes:_ ~document ->
          let current = Doc.find_job document (P.Developer_job.id candidate.job) in
          if
            Option.exists current ~f:(fun job ->
              P.Developer_job.same_immutable job candidate.job
              && Jsonaf.exactly_equal
                   (P.Developer_job.State.to_json (P.Developer_job.state job))
                   (P.Developer_job.State.to_json after))
          then Ok ()
          else
            Error (Store_error.Corrupt "installed developer result differs from Pending")))
  ;;
end
