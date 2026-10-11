open Core
module Catalog = Agent_session.Prompt_catalog
module Baseline = Agent_session.Prompt_project_baseline
module Store = Agent_session.Prompt_project_baseline_store
module Capture = Agent_session.Prompt_project_preparation
module Sources = Agent_session.Prompt_script_preparation
module Owner = Agent_session.Prompt_source_owner
module Revision = Agent_session.Prompt_revision
module Builder = Agent_session.Prompt_revision_builder
module Definition = Agent_session.Prompt_definition
module Id = Agent_protocol.Id.Prompt_definition
module History = Store.Snapshot.History

module Policy = struct
  type t =
    { source_preparation : Sources.Policy.t
    ; custody_limits : Chatml_prompt_programs.Limits.t
    ; baseline_limits : Baseline.Limits.t
    ; source_limits : Owner.Limits.t
    ; max_roots : int
    }

  let create
        ~source_preparation
        ~custody_limits
        ~baseline_limits
        ~source_limits
        ~max_roots
    =
    if max_roots < 0
    then Or_error.error_string "Negative prompt catalog root limit"
    else
      Ok { source_preparation; custody_limits; baseline_limits; source_limits; max_roots }
  ;;
end

module Mode = struct
  type t =
    | Build
    | Update of
        { definitions : Id.t list
        ; expected_generation : int
        }
end

module Error = struct
  type t =
    | Busy
    | Closed
    | Foreign_domain
    | Invalid_token
    | Publication_uncertain
    | Invalid_update
    | Stale_generation
    | Unknown_definition of Id.t
    | History of Store.Error.t
    | Association of Baseline.Error.t
    | Catalog of Core.Error.t
    | Seal of Catalog.Seal.Error.t
    | Artifacts of Agent_store.Store_error.t
end

type admission =
  | Available
  | Reserved of unit ref
  | Indeterminate

type t =
  { domain_id : int
  ; store : Agent_store.Session_store.t
  ; catalog : Catalog.t
  ; artifacts : Agent_store.Prompt_artifact_store.t
  ; baselines : Store.t
  ; policy : Policy.t
  ; content_limits : Chatmd_prompt_content.Limits.t option
  ; mutable admission : admission
  ; mutable history : History.t
  }

module Prepared = struct
  type phase =
    | Ready
    | Acknowledged
    | Installed
    | Rejecting
    | Rejected
    | Uncertain

  type nonrec t =
    { owner : t
    ; token : unit ref
    ; entries : Catalog.entry list
    ; seal : Catalog.Seal.t
    ; baseline : Store.Candidate.t option
    ; previous_history : History.t
    ; history : History.t
    ; mutable phase : phase
    ; mutable operation_active : bool
    ; mutable retired : bool
    }

  let entries t = t.entries
  let previous_history t = t.previous_history
  let history t = t.history
end

type certainty =
  | Rejected
  | Uncertain
[@@deriving equal, sexp_of]

let create ~store ~catalog ~artifacts ~baselines ~policy ~content_limits ~checkpoint =
  let open Result.Let_syntax in
  let%map snapshot =
    Store.snapshot baselines ~checkpoint
    |> Result.map_error ~f:(fun error -> Error.History error)
  in
  { domain_id = (Stdlib.Domain.self () :> int)
  ; store
  ; catalog
  ; artifacts
  ; baselines
  ; policy
  ; content_limits
  ; admission = Available
  ; history = Store.Snapshot.history snapshot
  }
;;

let success = Ok ()
let foreign = Error Error.Foreign_domain
let invalid = Error Error.Invalid_token
let uncertain = Error Error.Publication_uncertain
let domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)

let history t =
  if not (domain t)
  then invalid_arg "Prompt_catalog_publication.history: foreign Eio domain";
  t.history
;;

let reserve t =
  if not (domain t)
  then foreign
  else if Agent_store.Session_store.is_closed t.store
  then Error Error.Closed
  else (
    match t.admission with
    | Indeterminate -> uncertain
    | Reserved _ -> Error Error.Busy
    | Available -> Ok ())
;;

let owns t token =
  match t.admission with
  | Reserved current -> phys_equal current token
  | Available | Indeterminate -> false
;;

let release t token = if owns t token then t.admission <- Available

let store_uncertain t =
  match Store.state t.baselines with
  | Uncertain | Closed | Busy -> true
  | Available -> false
;;

let validate_mode t mode snapshot definitions ~checkpoint =
  let open Result.Let_syntax in
  let%bind count =
    List.fold_result definitions ~init:0 ~f:(fun count _ ->
      checkpoint ();
      if count >= t.policy.max_roots then Error Error.Invalid_update else Ok (count + 1))
  in
  ignore count;
  match mode with
  | Mode.Build -> Ok String.Set.empty
  | Update { definitions = targets; expected_generation } ->
    if expected_generation < 0 || List.is_empty targets
    then Error Error.Invalid_update
    else if
      not
        (Option.equal
           Int.equal
           (Store.Snapshot.generation snapshot)
           (Some expected_generation))
    then Error Error.Stale_generation
    else
      List.fold_result targets ~init:String.Set.empty ~f:(fun selected id ->
        checkpoint ();
        if Set.length selected >= t.policy.max_roots || Set.mem selected (Id.to_string id)
        then Error Error.Invalid_update
        else (
          match
            List.find definitions ~f:(fun definition ->
              checkpoint ();
              Id.equal definition.Definition.id id)
          with
          | None -> Error (Error.Unknown_definition id)
          | Some definition when not definition.enabled -> Error Error.Invalid_update
          | Some _ -> Ok (Set.add selected (Id.to_string id))))
;;

let capture_content ~checkpoint limits scope capture =
  Owner.Scope.capture_documents scope ~capture ~limits ~checkpoint
  |> Result.map_error ~f:(fun error ->
    [ Builder.Diagnostic.
        { code = "prompt.document_capture"
        ; message = Sexp.to_string_hum (Owner.Error.sexp_of_t error)
        ; source = None
        }
    ])
;;

let prepare t ~sources ~sw ~env ~mode ~created_at ~checkpoint definitions =
  let open Result.Let_syntax in
  let%bind () = reserve t in
  let token = ref () in
  t.admission <- Reserved token;
  let candidate = ref None in
  let sealed = ref None in
  let transferred = ref false in
  let cleanup () =
    if not !transferred
    then
      Eio.Cancel.protect (fun () ->
        (try
           match !sealed, !candidate with
           | Some seal, _ ->
             (match Catalog.Seal.abort seal with
              | Ok () -> ()
              | Error error ->
                t.admission <- Indeterminate;
                raise_s (Catalog.Seal.Error.sexp_of_t error))
           | None, Some candidate -> Catalog.discard_prepared candidate
           | None, None -> ()
         with
         | exn ->
           let trace = Stdlib.Printexc.get_raw_backtrace () in
           t.admission <- Indeterminate;
           Exn.raise_with_original_backtrace exn trace);
        release t token)
  in
  match
    let remaining = ref (Baseline.Limits.max_work_steps t.policy.baseline_limits) in
    let module Abort = struct
      exception Failure of Error.t
    end
    in
    let poll () =
      Eio.Fiber.check ();
      checkpoint ();
      Eio.Fiber.check ();
      if Agent_store.Session_store.is_closed t.store
      then raise (Abort.Failure Error.Closed);
      if !remaining <= 0
      then raise (Abort.Failure (Error.Association Baseline.Error.Limit_exceeded));
      decr remaining
    in
    try
      poll ();
      let%bind snapshot =
        Store.snapshot t.baselines ~checkpoint:poll
        |> Result.map_error ~f:(fun e -> Error.History e)
      in
      let%bind updates = validate_mode t mode snapshot definitions ~checkpoint:poll in
      let associations = ref String.Map.empty in
      let prepare_programs scope captured =
        let id =
          Agent_store.Prompt_source_receipt.definition_id (Owner.Capture.receipt captured)
        in
        let mode =
          if Set.mem updates (Id.to_string id) then Capture.Mode.Update else Build
        in
        let%map prepared =
          Capture.capture
            t.policy.source_preparation
            ~mode
            ~snapshot
            ~artifact_store:t.artifacts
            ~baseline_limits:t.policy.baseline_limits
            ~custody_limits:t.policy.custody_limits
            ~root_role:Ui
            ~host:sources
            ~scope
            ~capture:captured
            ~provided:[]
            ~env
            ~checkpoint:poll
            ()
        in
        associations
        := Map.set
             !associations
             ~key:(Id.to_string id)
             ~data:(Capture.Prepared.association prepared);
        Capture.Prepared.programs prepared
      in
      let%bind prepared =
        Catalog.prepare_owned
          t.catalog
          ~prepare_programs
          ?prepare_content:
            (Option.map t.content_limits ~f:(capture_content ~checkpoint:poll))
          ~sw
          ~source_limits:t.policy.source_limits
          ~max_roots:t.policy.max_roots
          ~checkpoint:poll
          ~transaction_id:(fun _ -> Agent_protocol.Id.Transaction.create ())
          ~created_at
          definitions
        |> Result.map_error ~f:(fun e -> Error.Catalog e)
      in
      candidate := Some prepared;
      let entries = Catalog.prepared_entries prepared in
      let%bind successes =
        List.fold_result entries ~init:[] ~f:(fun successes entry ->
          poll ();
          match entry.Catalog.availability with
          | Unavailable _ | Disabled -> Ok successes
          | Ready revision ->
            (match Map.find !associations (Id.to_string entry.definition.id) with
             | None ->
               Error
                 (Error.Association
                    (Baseline.Error.Invalid_association
                       "Successful revision lacks authentic prepared roles"))
             | Some association ->
               let%map success =
                 Baseline.Success.finalize
                   association
                   ~revision
                   ~limits:t.policy.baseline_limits
                   ~checkpoint:poll
                 |> Result.map_error ~f:(fun e -> Error.Association e)
               in
               success :: successes))
      in
      let%bind baseline =
        match Store.Snapshot.generation snapshot with
        | None -> Ok None
        | Some _ ->
          Store.prepare t.baselines ~snapshot ~successes ~checkpoint:poll
          |> Result.map ~f:Option.some
          |> Result.map_error ~f:(fun e -> Error.History e)
      in
      let%bind seal =
        Catalog.seal t.catalog prepared ~checkpoint:poll
        |> Result.map_error ~f:(fun e -> Error.Seal e)
      in
      sealed := Some seal;
      let result =
        Ok
          { Prepared.owner = t
          ; token
          ; entries
          ; seal
          ; baseline
          ; previous_history = Store.Snapshot.history snapshot
          ; history =
              (match baseline with
               | None -> Store.Snapshot.history snapshot
               | Some candidate -> History.Tracked (Store.Candidate.generation candidate))
          ; phase = Ready
          ; operation_active = false
          ; retired = false
          }
      in
      transferred := true;
      result
    with
    | Abort.Failure error -> Error error
  with
  | result ->
    cleanup ();
    result
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    (try cleanup () with
     | _ -> ());
    Exn.raise_with_original_backtrace exn trace
;;

let mark_uncertain t =
  t.Prepared.phase <- Uncertain;
  t.owner.admission <- Indeterminate;
  ignore (Catalog.Seal.mark_uncertain t.seal : (unit, Catalog.Seal.Error.t) Result.t)
;;

let publish t ~checkpoint =
  if not (domain t.Prepared.owner)
  then foreign
  else if t.operation_active
  then Error Error.Busy
  else if not (owns t.owner t.token)
  then invalid
  else (
    match t.phase with
    | Acknowledged | Installed | Rejecting | Rejected | Uncertain -> invalid
    | Ready ->
      t.operation_active <- true;
      Fun.protect
        ~finally:(fun () -> t.operation_active <- false)
        (fun () ->
           match
             Eio.Fiber.check ();
             checkpoint ();
             Eio.Fiber.check ();
             match t.baseline with
             | None ->
               t.phase <- Acknowledged;
               success
             | Some candidate ->
               (match Store.publish t.owner.baselines candidate ~checkpoint with
                | Ok _ ->
                  t.phase <- Acknowledged;
                  success
                | Error error ->
                  if store_uncertain t.owner then mark_uncertain t;
                  Error (Error.History error))
           with
           | Ok () -> success
           | Error error ->
             if
               Prepared.(
                 match t.phase with
                 | Acknowledged | Uncertain -> true
                 | Ready | Installed | Rejecting | Rejected -> store_uncertain t.owner)
             then mark_uncertain t;
             Error error
           | exception exn ->
             let trace = Stdlib.Printexc.get_raw_backtrace () in
             if store_uncertain t.owner then mark_uncertain t;
             Exn.raise_with_original_backtrace exn trace))
;;

let commit t =
  if not (domain t.Prepared.owner)
  then foreign
  else if t.operation_active
  then Error Error.Busy
  else (
    match t.phase with
    | Installed -> success
    | Uncertain -> uncertain
    | Ready | Rejecting | Rejected -> invalid
    | Acknowledged ->
      if not (owns t.owner t.token)
      then invalid
      else (
        match Catalog.Seal.commit t.seal with
        | Ok () ->
          t.owner.history <- t.history;
          t.phase <- Installed;
          success
        | Error _ ->
          mark_uncertain t;
          uncertain))
;;

let with_cleanup t ~f =
  if t.Prepared.operation_active
  then invalid_arg "Prompt_catalog_publication: overlapping token operation";
  t.operation_active <- true;
  Fun.protect ~finally:(fun () -> t.operation_active <- false) f
;;

let abort_candidate t =
  t.Prepared.phase <- Rejecting;
  Catalog.Seal.abort t.seal
  |> Result.map_error ~f:(fun e -> Core.Error.create_s (Catalog.Seal.Error.sexp_of_t e))
  |> Or_error.ok_exn;
  t.phase <- Rejected;
  release t.owner t.token;
  Rejected
;;

let discard t =
  if not (domain t.Prepared.owner)
  then invalid_arg "Prompt_catalog_publication.discard: foreign domain";
  with_cleanup t ~f:(fun () ->
    Eio.Cancel.protect (fun () ->
      match t.phase with
      | Installed -> Uncertain
      | Acknowledged | Uncertain ->
        mark_uncertain t;
        Catalog.Seal.retire t.seal
        |> Result.map_error ~f:(fun e ->
          Core.Error.create_s (Catalog.Seal.Error.sexp_of_t e))
        |> Or_error.ok_exn;
        Uncertain
      | Rejected -> Rejected
      | Rejecting -> abort_candidate t
      | Ready ->
        if store_uncertain t.owner
        then (
          mark_uncertain t;
          Catalog.Seal.retire t.seal
          |> Result.map_error ~f:(fun e ->
            Core.Error.create_s (Catalog.Seal.Error.sexp_of_t e))
          |> Or_error.ok_exn;
          Uncertain)
        else abort_candidate t))
;;

let retire t =
  if not (domain t.Prepared.owner)
  then invalid_arg "Prompt_catalog_publication.retire: foreign domain";
  if not t.retired
  then (
    match t.phase with
    | Ready | Acknowledged | Rejecting | Rejected | Uncertain ->
      invalid_arg "Prompt_catalog_publication.retire: token is not installed"
    | Installed ->
      with_cleanup t ~f:(fun () ->
        Eio.Cancel.protect (fun () ->
          Catalog.Seal.retire t.seal
          |> Result.map_error ~f:(fun e ->
            Core.Error.create_s (Catalog.Seal.Error.sexp_of_t e))
          |> Or_error.ok_exn;
          t.retired <- true;
          release t.owner t.token)))
;;

let prune t ~additional ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = reserve t in
  let token = ref () in
  t.admission <- Reserved token;
  Fun.protect
    ~finally:(fun () -> release t token)
    (fun () ->
       let%bind snapshot =
         Store.snapshot t.baselines ~checkpoint
         |> Result.map_error ~f:(fun e -> Error.History e)
       in
       match Store.Snapshot.generation snapshot with
       | None -> Ok 0
       | Some _ ->
         Catalog.prune_unreferenced_artifacts
           t.catalog
           ~additional:(Store.Snapshot.protected_revisions snapshot @ additional)
         |> Result.map_error ~f:(fun e -> Error.Artifacts e))
;;
