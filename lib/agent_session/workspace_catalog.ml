open! Core

module Seal_error = struct
  type t =
    | Busy
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
  { definitions : Workspace_definition.t list
  ; by_id : Workspace_definition.t String.Map.t
  ; by_name : Workspace_definition.t String.Map.t
  }

type t =
  { domain_id : int
  ; mutable snapshot : snapshot
  ; mutable publication : publication
  }

and publication =
  | Open
  | Sealed of sealed
  | Indeterminate of sealed

and sealed =
  { owner : t
  ; replacement : snapshot
  ; mutable phase : Phase.t
  ; reserved : publication
  ; uncertain : publication
  }

let success = Ok ()
let busy = Error Seal_error.Busy
let publication_uncertain = Error Seal_error.Publication_uncertain
let foreign_domain = Error Seal_error.Foreign_domain
let stale = Error Seal_error.Stale_preparation
let aborted = Error Seal_error.Aborted
let committed = Error Seal_error.Committed
let same_domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)

let check_domain t =
  if not (same_domain t)
  then invalid_arg "workspace catalog belongs to another Eio domain"
;;

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ()
;;

let create definitions =
  let duplicate_id =
    List.contains_dup definitions ~compare:(fun left right ->
      Agent_protocol.Id.Workspace_definition.compare
        left.Workspace_definition.id
        right.Workspace_definition.id)
  in
  let duplicate_name =
    List.contains_dup definitions ~compare:(fun left right ->
      String.compare
        left.Workspace_definition.config_name
        right.Workspace_definition.config_name)
  in
  if duplicate_id || duplicate_name
  then
    Error
      (Agent_store.Store_error.Corrupt "workspace catalog contains duplicate identities")
  else
    Ok
      { domain_id = (Stdlib.Domain.self () :> int)
      ; snapshot =
          { definitions
          ; by_id =
              List.fold definitions ~init:String.Map.empty ~f:(fun map definition ->
                Map.set
                  map
                  ~key:(Agent_protocol.Id.Workspace_definition.to_string definition.id)
                  ~data:definition)
          ; by_name =
              List.fold definitions ~init:String.Map.empty ~f:(fun map definition ->
                Map.set map ~key:definition.config_name ~data:definition)
          }
      ; publication = Open
      }
;;

let definitions t =
  check_domain t;
  t.snapshot.definitions
;;

let find t id =
  check_domain t;
  Map.find t.snapshot.by_id (Agent_protocol.Id.Workspace_definition.to_string id)
;;

let find_by_name t name =
  check_domain t;
  Map.find t.snapshot.by_name name
;;

module Seal = struct
  module Error = Seal_error

  type t = sealed

  let admit t =
    match t.owner.publication with
    | Sealed actual -> phys_equal t actual
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
          t.owner.snapshot <- t.replacement;
          t.owner.publication <- Open;
          t.phase <- Committed;
          success))
  ;;
end

let seal t ~replacement ~checkpoint =
  if not (same_domain t && same_domain replacement)
  then foreign_domain
  else (
    match t.publication with
    | Sealed _ -> busy
    | Indeterminate _ -> publication_uncertain
    | Open ->
      let previous = t.snapshot in
      poll checkpoint;
      let rec sealed =
        { owner = t
        ; replacement = replacement.snapshot
        ; phase = Pending
        ; reserved = Sealed sealed
        ; uncertain = Indeterminate sealed
        }
      in
      let result = Ok sealed in
      poll checkpoint;
      (match t.publication with
       | Sealed _ -> busy
       | Indeterminate _ -> publication_uncertain
       | Open ->
         if not (phys_equal previous t.snapshot)
         then stale
         else (
           t.publication <- sealed.reserved;
           result)))
;;

let install t ~replacement =
  match seal t ~replacement ~checkpoint:ignore with
  | Error error -> invalid_arg (Sexp.to_string_hum (Seal_error.sexp_of_t error))
  | Ok sealed ->
    (match Seal.commit sealed with
     | Ok () -> ()
     | Error error -> invalid_arg (Sexp.to_string_hum (Seal_error.sexp_of_t error)))
;;
