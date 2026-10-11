open! Core
module Workspaces = Agent_session.Workspace_catalog
module Policy = Agent_session.Permission_policy

module Seal_error = struct
  type t =
    | Busy
    | Workspaces_busy
    | Publication_uncertain
    | Foreign_domain
    | Stale_preparation
    | Aborted
    | Committed
  [@@deriving equal, sexp_of]
end

module Phase = struct
  type t =
    | Pending
    | Committed
    | Aborted
    | Uncertain
end

type snapshot =
  { profiles : Policy.t String.Map.t
  ; revisions : Policy.t String.Map.t
  ; grants : Operator_manifest_grant.t list
  }

type t =
  { domain_id : int
  ; workspaces : Workspaces.t
  ; mutable snapshot : snapshot
  ; mutable publication : publication
  ; mutable admissions : int
  }

and publication =
  | Open
  | Sealed of sealed
  | Indeterminate of sealed

and sealed =
  { owner : t
  ; workspace : Workspaces.Seal.t
  ; replacement : snapshot
  ; mutable phase : Phase.t
  ; reserved : publication
  ; uncertain : publication
  }

let success = Ok ()
let busy = Error Seal_error.Busy
let workspace_busy = Error Seal_error.Workspaces_busy
let publication_uncertain = Error Seal_error.Publication_uncertain
let foreign_domain = Error Seal_error.Foreign_domain
let stale = Error Seal_error.Stale_preparation
let aborted = Error Seal_error.Aborted
let committed = Error Seal_error.Committed
let same_domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)

let check_domain t =
  if not (same_domain t) then invalid_arg "factory catalog belongs to another Eio domain"
;;

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ()
;;

let poll_key checkpoint key =
  (* Charge traversal before Map compares this key. The standard comparator is
     not preemptible; this does not claim otherwise. No string copy is retained. *)
  let rec chunks remaining =
    if remaining > 0
    then (
      poll checkpoint;
      chunks (remaining - Int.min remaining 4096))
  in
  chunks (String.length key)
;;

let index profiles ~revisions ~checkpoint =
  List.fold
    profiles
    ~init:(String.Map.empty, revisions)
    ~f:(fun (current, history) profile ->
      poll checkpoint;
      poll_key checkpoint profile.Policy.id;
      poll_key checkpoint profile.revision_digest;
      let current = Map.set current ~key:profile.id ~data:profile in
      let history = Map.set history ~key:profile.revision_digest ~data:profile in
      current, history)
;;

let create ~workspaces ~permission_profiles ~manifest_grants =
  ignore (Workspaces.definitions workspaces : Agent_session.Workspace_definition.t list);
  let profiles, revisions =
    index permission_profiles ~revisions:String.Map.empty ~checkpoint:ignore
  in
  { domain_id = (Stdlib.Domain.self () :> int)
  ; workspaces
  ; snapshot = { profiles; revisions; grants = manifest_grants }
  ; publication = Open
  ; admissions = 0
  }
;;

let find_workspace t id =
  check_domain t;
  Workspaces.find t.workspaces id
;;

let find_profile t id =
  check_domain t;
  Map.find t.snapshot.profiles id
;;

let find_profile_revision t digest =
  check_domain t;
  Map.find t.snapshot.revisions digest
;;

let manifest_grants t =
  check_domain t;
  t.snapshot.grants
;;

module Admission = struct
  module Error = struct
    type t =
      | Busy
      | Publication_uncertain
      | Foreign_domain
    [@@deriving equal, sexp_of]
  end
end

let with_admission t ~f =
  if not (same_domain t)
  then Error Admission.Error.Foreign_domain
  else (
    match t.publication with
    | Sealed _ -> Error Admission.Error.Busy
    | Indeterminate _ -> Error Admission.Error.Publication_uncertain
    | Open ->
      if Int.equal t.admissions Int.max_value
      then Error Admission.Error.Busy
      else (
        t.admissions <- t.admissions + 1;
        Exn.protect
          ~finally:(fun () -> t.admissions <- t.admissions - 1)
          ~f:(fun () ->
            match Workspaces.with_admission t.workspaces ~f with
            | Ok _ as result -> result
            | Error Busy -> Error Admission.Error.Busy
            | Error Publication_uncertain -> Error Admission.Error.Publication_uncertain
            | Error Foreign_domain -> Error Admission.Error.Foreign_domain)))
;;

module Seal = struct
  module Error = Seal_error

  type t = sealed

  let admit t =
    match t.owner.publication with
    | Sealed actual -> phys_equal actual t
    | Open | Indeterminate _ -> false
  ;;

  let abort t =
    if not (same_domain t.owner)
    then foreign_domain
    else (
      match t.phase with
      | Aborted -> success
      | Committed -> committed
      | Uncertain -> publication_uncertain
      | Pending ->
        if not (admit t)
        then stale
        else (
          match Workspaces.Seal.abort t.workspace with
          | Error _ -> stale
          | Ok () ->
            t.owner.publication <- Open;
            t.phase <- Aborted;
            success))
  ;;

  let mark_uncertain t =
    if not (same_domain t.owner)
    then foreign_domain
    else (
      match t.phase with
      | Uncertain -> success
      | Committed -> committed
      | Aborted -> aborted
      | Pending ->
        if not (admit t)
        then stale
        else (
          match Workspaces.Seal.mark_uncertain t.workspace with
          | Error _ -> stale
          | Ok () ->
            t.owner.publication <- t.uncertain;
            t.phase <- Uncertain;
            success))
  ;;

  let commit t =
    if not (same_domain t.owner)
    then foreign_domain
    else (
      match t.phase with
      | Committed -> success
      | Aborted -> aborted
      | Uncertain -> publication_uncertain
      | Pending ->
        if not (admit t)
        then stale
        else (
          match Workspaces.Seal.commit t.workspace with
          | Error _ -> stale
          | Ok () ->
            t.owner.snapshot <- t.replacement;
            t.owner.publication <- Open;
            t.phase <- Committed;
            success))
  ;;
end

let workspace_error = function
  | Workspaces.Seal.Error.Busy -> workspace_busy
  | Publication_uncertain -> publication_uncertain
  | Foreign_domain -> foreign_domain
  | Stale_preparation | Aborted | Committed -> stale
;;

let seal t ~workspaces ~permission_profiles ~manifest_grants ~checkpoint =
  if not (same_domain t)
  then foreign_domain
  else (
    match t.publication with
    | Sealed _ -> busy
    | Indeterminate _ -> publication_uncertain
    | Open ->
      if t.admissions > 0
      then busy
      else (
        let previous = t.snapshot in
        poll checkpoint;
        match Workspaces.seal t.workspaces ~replacement:workspaces ~checkpoint with
        | Error error -> workspace_error error
        | Ok workspace ->
          let release () =
            ignore
              (Workspaces.Seal.abort workspace : (unit, Workspaces.Seal.Error.t) Result.t)
          in
          (try
             let profiles, revisions =
               index permission_profiles ~revisions:previous.revisions ~checkpoint
             in
             List.iter manifest_grants ~f:(fun _ -> poll checkpoint);
             let replacement = { profiles; revisions; grants = manifest_grants } in
             let rec sealed =
               { owner = t
               ; workspace
               ; replacement
               ; phase = Pending
               ; reserved = Sealed sealed
               ; uncertain = Indeterminate sealed
               }
             in
             let result = Ok sealed in
             poll checkpoint;
             match t.publication with
             | Sealed _ ->
               release ();
               busy
             | Indeterminate _ ->
               release ();
               publication_uncertain
             | Open ->
               if t.admissions > 0
               then (
                 release ();
                 busy)
               else if not (phys_equal previous t.snapshot)
               then (
                 release ();
                 stale)
               else (
                 t.publication <- sealed.reserved;
                 result)
           with
           | exn ->
             let trace = Stdlib.Printexc.get_raw_backtrace () in
             Eio.Cancel.protect release;
             Exn.raise_with_original_backtrace exn trace)))
;;

let install t ~workspaces ~permission_profiles ~manifest_grants =
  match seal t ~workspaces ~permission_profiles ~manifest_grants ~checkpoint:ignore with
  | Error error -> invalid_arg (Sexp.to_string_hum (Seal_error.sexp_of_t error))
  | Ok sealed ->
    (match Seal.commit sealed with
     | Ok () -> ()
     | Error error -> invalid_arg (Sexp.to_string_hum (Seal_error.sexp_of_t error)))
;;
