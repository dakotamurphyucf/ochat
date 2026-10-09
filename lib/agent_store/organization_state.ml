open Core
module P = Agent_protocol
module Error = P.Error

module Project_entry = struct
  type t =
    { group : Agent_protocol.Organization_group.Project.t
    ; deleted_at : Agent_protocol.Timestamp.t option
    }
end

module Collection_entry = struct
  type t =
    { group : Agent_protocol.Organization_group.Collection.t
    ; deleted_at : Agent_protocol.Timestamp.t option
    }
end

module Receipt = struct
  type t =
    { key : Idempotency_store.Key.t
    ; request_digest : string
    ; result : Agent_protocol.Organization_result.t
    ; created_at : Agent_protocol.Timestamp.t
    ; expires_at : Agent_protocol.Timestamp.t
    }

  let equal left right =
    Idempotency_store.Key.compare left.key right.key = 0
    && String.equal left.request_digest right.request_digest
    && P.Organization_result.equal left.result right.result
    && P.Timestamp.equal left.created_at right.created_at
    && P.Timestamp.equal left.expires_at right.expires_at
  ;;
end

module Mutation = struct
  type t =
    | Create_project of Agent_protocol.Organization_request.Create.t
    | Update_project of Agent_protocol.Organization_request.Project.Update.t
    | Delete_project of Agent_protocol.Organization_request.Project.Delete.t
    | Create_collection of Agent_protocol.Organization_request.Create.t
    | Update_collection of Agent_protocol.Organization_request.Collection.Update.t
    | Delete_collection of Agent_protocol.Organization_request.Collection.Delete.t
end

module Candidate = struct
  type t =
    | Project of Agent_protocol.Id.Project.t
    | Collection of Agent_protocol.Id.Collection.t
end

module Project_key = struct
  module T = struct
    type t = P.Id.Project.t [@@deriving sexp_of]

    let compare = P.Id.Project.compare
  end

  include T
  include Comparator.Make (T)
end

module Collection_key = struct
  module T = struct
    type t = P.Id.Collection.t [@@deriving sexp_of]

    let compare = P.Id.Collection.compare
  end

  include T
  include Comparator.Make (T)
end

type t =
  { server_id : P.Id.Server.t
  ; revision : int64
  ; projects : (P.Id.Project.t, Project_entry.t, Project_key.comparator_witness) Map.t
  ; collections :
      (P.Id.Collection.t, Collection_entry.t, Collection_key.comparator_witness) Map.t
  ; receipts :
      (Idempotency_store.Key.t, Receipt.t, Idempotency_store.Key.comparator_witness) Map.t
  }

let empty ~server_id =
  { server_id
  ; revision = 0L
  ; projects = Map.empty (module Project_key)
  ; collections = Map.empty (module Collection_key)
  ; receipts = Map.empty (module Idempotency_store.Key)
  }
;;

let server_id t = t.server_id
let revision t = t.revision
let projects t = Map.data t.projects
let collections t = Map.data t.collections
let receipts t = Map.data t.receipts
let failure code message = Error (Error.create code ~message ~retryable:false ())
let invalid message = failure Invalid_request message
let not_found () = failure Organization_not_found "organization group not found"

let owns principal creator =
  P.Id.Principal.equal principal.P.Principal.id creator
  || P.Principal.has_scope principal Administer_configuration
;;

let require_scope principal scope =
  if P.Principal.has_scope principal scope
  then Ok ()
  else failure Permission_denied ("missing scope " ^ P.Scope.to_string scope)
;;

let next revision =
  if Int64.equal revision Int64.max_value
  then failure Conflict "organization revision exhausted"
  else Ok (Int64.succ revision)
;;

let check_revision actual expected =
  if Int64.equal actual expected
  then Ok ()
  else failure Conflict "organization revision changed"
;;

let check_host t host =
  if P.Id.Server.equal t.server_id host
  then Ok ()
  else invalid "organization host differs from initialized server"
;;

let visible_project t ~principal id =
  let open Result.Let_syntax in
  let%bind () = require_scope principal View_organization in
  match Map.find t.projects id with
  | Some entry
    when Option.is_none entry.Project_entry.deleted_at
         && owns principal entry.group.creator_principal_id -> Ok entry.group
  | Some _ | None -> not_found ()
;;

let visible_collection t ~principal id =
  let open Result.Let_syntax in
  let%bind () = require_scope principal View_organization in
  match Map.find t.collections id with
  | Some entry
    when Option.is_none entry.Collection_entry.deleted_at
         && owns principal entry.group.creator_principal_id -> Ok entry.group
  | Some _ | None -> not_found ()
;;

let result_owner t result =
  match result with
  | P.Organization_result.Project_created value | Project_updated value ->
    Option.map (Map.find t.projects value.id) ~f:(fun entry ->
      entry.Project_entry.group.creator_principal_id)
  | Project_deleted value ->
    Option.map (Map.find t.projects value.id) ~f:(fun entry ->
      entry.Project_entry.group.creator_principal_id)
  | P.Organization_result.Collection_created value | Collection_updated value ->
    Option.map (Map.find t.collections value.id) ~f:(fun entry ->
      entry.Collection_entry.group.creator_principal_id)
  | Collection_deleted value ->
    Option.map (Map.find t.collections value.id) ~f:(fun entry ->
      entry.Collection_entry.group.creator_principal_id)
;;

let lookup_receipt t ~principal ~now ~key ~request_digest =
  let open Result.Let_syntax in
  let%bind () = require_scope principal Manage_organization in
  if
    not
      (P.Id.Principal.equal
         principal.P.Principal.id
         key.Idempotency_store.Key.principal_id)
  then failure Permission_denied "receipt belongs to another principal"
  else (
    match Map.find t.receipts key with
    | None -> Ok None
    | Some receipt when P.Timestamp.compare now receipt.Receipt.expires_at >= 0 -> Ok None
    | Some receipt ->
      (match result_owner t receipt.result with
       | None -> invalid "receipt references missing organization entry"
       | Some creator when not (owns principal creator) -> not_found ()
       | Some _ when not (String.equal request_digest receipt.request_digest) ->
         failure Idempotency_conflict "idempotency key was used for another request"
       | Some _ -> Ok (Some receipt.result)))
;;

let validate_receipt_target t (receipt : Receipt.t) =
  let open Result.Let_syntax in
  let check_group expected_method original current =
    if not (String.equal receipt.key.method_name expected_method)
    then invalid "receipt method differs from result"
    else original current
  in
  let check_values creator revision current_creator current_revision =
    if
      P.Id.Principal.equal creator current_creator && Int64.(revision <= current_revision)
    then Ok ()
    else invalid "receipt result disagrees with retained group"
  in
  match receipt.result with
  | P.Organization_result.Project_created original ->
    (match Map.find t.projects original.id with
     | None -> invalid "receipt target absent"
     | Some entry ->
       let%bind () =
         check_group
           "project.create"
           (fun group ->
              check_values
                original.creator_principal_id
                original.revision
                group.P.Organization_group.Project.creator_principal_id
                group.revision)
           entry.Project_entry.group
       in
       if
         Int64.equal original.revision 0L
         && P.Id.Principal.equal original.creator_principal_id receipt.key.principal_id
       then Ok ()
       else invalid "invalid create receipt")
  | P.Organization_result.Project_updated original ->
    (match Map.find t.projects original.id with
     | None -> invalid "receipt target absent"
     | Some entry ->
       let%bind () =
         check_group
           "project.update"
           (fun group ->
              check_values
                original.creator_principal_id
                original.revision
                group.P.Organization_group.Project.creator_principal_id
                group.revision)
           entry.Project_entry.group
       in
       Ok ())
  | P.Organization_result.Project_deleted original ->
    (match Map.find t.projects original.id with
     | Some entry
       when String.equal receipt.key.method_name "project.delete"
            && Int64.equal original.revision entry.Project_entry.group.revision
            && Option.equal P.Timestamp.equal entry.deleted_at (Some original.deleted_at)
       -> Ok ()
     | Some _ | None -> invalid "delete receipt differs from tombstone")
  | P.Organization_result.Collection_created original ->
    (match Map.find t.collections original.id with
     | None -> invalid "receipt target absent"
     | Some entry ->
       let%bind () =
         check_group
           "collection.create"
           (fun group ->
              check_values
                original.creator_principal_id
                original.revision
                group.P.Organization_group.Collection.creator_principal_id
                group.revision)
           entry.Collection_entry.group
       in
       if
         Int64.equal original.revision 0L
         && P.Id.Principal.equal original.creator_principal_id receipt.key.principal_id
       then Ok ()
       else invalid "invalid create receipt")
  | P.Organization_result.Collection_updated original ->
    (match Map.find t.collections original.id with
     | None -> invalid "receipt target absent"
     | Some entry ->
       let%bind () =
         check_group
           "collection.update"
           (fun group ->
              check_values
                original.creator_principal_id
                original.revision
                group.P.Organization_group.Collection.creator_principal_id
                group.revision)
           entry.Collection_entry.group
       in
       Ok ())
  | P.Organization_result.Collection_deleted original ->
    (match Map.find t.collections original.id with
     | Some entry
       when String.equal receipt.key.method_name "collection.delete"
            && Int64.equal original.revision entry.Collection_entry.group.revision
            && Option.equal P.Timestamp.equal entry.deleted_at (Some original.deleted_at)
       -> Ok ()
     | Some _ | None -> invalid "delete receipt differs from tombstone")
;;

let restore ~server_id ~revision ~projects ~collections ~receipts =
  let open Result.Let_syntax in
  if
    Int64.(revision < 0L)
    || List.length projects > 4096
    || List.length collections > 4096
    || List.length receipts > 4096
  then invalid "organization state bounds exceeded"
  else (
    let insert map key value =
      if Map.mem map key
      then invalid "duplicate organization identity"
      else Ok (Map.set map ~key ~data:value)
    in
    let check_deleted group_revision updated_at = function
      | _ when Int64.(group_revision > revision) ->
        invalid "group revision exceeds host revision"
      | None -> Ok ()
      | Some deleted_at ->
        if Int64.(group_revision < 1L) || P.Timestamp.compare deleted_at updated_at <> 0
        then invalid "invalid organization tombstone"
        else Ok ()
    in
    let%bind project_map =
      List.fold_result
        projects
        ~init:(Map.empty (module Project_key))
        ~f:(fun map entry ->
          let%bind () =
            check_deleted
              entry.Project_entry.group.revision
              entry.group.updated_at
              entry.deleted_at
          in
          insert map entry.group.id entry)
    in
    let%bind collection_map =
      List.fold_result
        collections
        ~init:(Map.empty (module Collection_key))
        ~f:(fun map entry ->
          let%bind () =
            check_deleted
              entry.Collection_entry.group.revision
              entry.group.updated_at
              entry.deleted_at
          in
          insert map entry.group.id entry)
    in
    let%bind receipt_map =
      List.fold_result
        receipts
        ~init:(Map.empty (module Idempotency_store.Key))
        ~f:(fun map receipt ->
          if
            Option.is_some receipt.Receipt.key.session_id
            || String.length receipt.request_digest <> 64
            || (not
                  (String.for_all receipt.request_digest ~f:(function
                     | '0' .. '9' | 'a' .. 'f' -> true
                     | _ -> false)))
            || P.Timestamp.compare receipt.expires_at receipt.created_at <= 0
          then invalid "invalid organization receipt"
          else insert map receipt.key receipt)
    in
    let restored =
      { server_id
      ; revision
      ; projects = project_map
      ; collections = collection_map
      ; receipts = receipt_map
      }
    in
    let%map () =
      List.fold_result receipts ~init:() ~f:(fun () receipt ->
        validate_receipt_target restored receipt)
    in
    restored)
;;

let validate_retention t ~next_state ~now =
  let open Result.Let_syntax in
  if
    (not (P.Id.Server.equal t.server_id next_state.server_id))
    || Int64.(next_state.revision < t.revision)
  then invalid "organization successor changes host or reverses revision"
  else (
    let%bind () =
      Map.fold t.projects ~init:(Ok ()) ~f:(fun ~key ~data:previous checked ->
        let%bind () = checked in
        match Map.find next_state.projects key with
        | None -> invalid "organization successor removes retained ID"
        | Some next ->
          if
            (not
               (P.Id.Principal.equal
                  previous.Project_entry.group.creator_principal_id
                  next.group.creator_principal_id))
            || (not (P.Timestamp.equal previous.group.created_at next.group.created_at))
            || Int64.(next.group.revision < previous.group.revision)
            || P.Timestamp.compare next.group.updated_at previous.group.updated_at < 0
            || (Option.is_some previous.deleted_at
                && ((not
                       (Option.equal
                          P.Timestamp.equal
                          previous.deleted_at
                          next.deleted_at))
                    || not (P.Organization_group.Project.equal previous.group next.group)
                   ))
          then invalid "organization successor alters immutable identity or tombstone"
          else Ok ())
    in
    let%bind () =
      Map.fold t.collections ~init:(Ok ()) ~f:(fun ~key ~data:previous checked ->
        let%bind () = checked in
        match Map.find next_state.collections key with
        | None -> invalid "organization successor removes retained ID"
        | Some next ->
          if
            (not
               (P.Id.Principal.equal
                  previous.Collection_entry.group.creator_principal_id
                  next.group.creator_principal_id))
            || (not (P.Timestamp.equal previous.group.created_at next.group.created_at))
            || Int64.(next.group.revision < previous.group.revision)
            || P.Timestamp.compare next.group.updated_at previous.group.updated_at < 0
            || (Option.is_some previous.deleted_at
                && ((not
                       (Option.equal
                          P.Timestamp.equal
                          previous.deleted_at
                          next.deleted_at))
                    || not
                         (P.Organization_group.Collection.equal previous.group next.group)
                   ))
          then invalid "organization successor alters immutable identity or tombstone"
          else Ok ())
    in
    Map.fold t.receipts ~init:(Ok ()) ~f:(fun ~key ~data:previous checked ->
      let%bind () = checked in
      match Map.find next_state.receipts key with
      | None when P.Timestamp.compare now previous.Receipt.expires_at >= 0 -> Ok ()
      | None -> invalid "organization successor retires unexpired receipt"
      | Some next ->
        if Receipt.equal previous next
        then Ok ()
        else if P.Timestamp.compare now previous.expires_at >= 0
        then Ok ()
        else invalid "organization successor alters unexpired receipt"))
;;

let request_digest mutation =
  let json =
    match mutation with
    | Mutation.Create_project request -> P.Organization_request.Create.to_json request
    | Mutation.Update_project request ->
      P.Organization_request.Project.Update.to_json request
    | Mutation.Delete_project request ->
      P.Organization_request.Project.Delete.to_json request
    | Mutation.Create_collection request -> P.Organization_request.Create.to_json request
    | Mutation.Update_collection request ->
      P.Organization_request.Collection.Update.to_json request
    | Mutation.Delete_collection request ->
      P.Organization_request.Collection.Delete.to_json request
  in
  P.Json_codec.canonical_string json
  |> Result.map ~f:(fun encoded ->
    Digestif.SHA256.digest_string encoded |> Digestif.SHA256.to_hex)
;;

let apply t ~principal ~audit ~now ~candidate mutation =
  let open Result.Let_syntax in
  let%bind () = require_scope principal Manage_organization in
  let host, method_name, idempotency_key =
    match mutation with
    | Mutation.Create_project request ->
      ( request.P.Organization_request.Create.host_id
      , "project.create"
      , request.idempotency_key )
    | Mutation.Update_project request ->
      ( request.P.Organization_request.Project.Update.host_id
      , "project.update"
      , request.idempotency_key )
    | Mutation.Delete_project request ->
      ( request.P.Organization_request.Project.Delete.host_id
      , "project.delete"
      , request.idempotency_key )
    | Mutation.Create_collection request ->
      ( request.P.Organization_request.Create.host_id
      , "collection.create"
      , request.idempotency_key )
    | Mutation.Update_collection request ->
      ( request.P.Organization_request.Collection.Update.host_id
      , "collection.update"
      , request.idempotency_key )
    | Mutation.Delete_collection request ->
      ( request.P.Organization_request.Collection.Delete.host_id
      , "collection.delete"
      , request.idempotency_key )
  in
  let%bind () = check_host t host in
  if
    (not
       (P.Id.Principal.equal
          principal.id
          audit.Idempotency_store.Command_audit.key.principal_id))
    || (not (String.equal method_name audit.key.method_name))
    || Option.is_some audit.key.session_id
    || not (P.Idempotency_key.equal idempotency_key audit.key.idempotency_key)
  then invalid "organization command audit differs from request"
  else (
    let%bind expected_digest = request_digest mutation in
    let%bind () =
      if String.equal expected_digest audit.request_digest
      then Ok ()
      else invalid "command digest differs from mutation"
    in
    let%bind replay =
      lookup_receipt t ~principal ~now ~key:audit.key ~request_digest:audit.request_digest
    in
    match replay with
    | Some result -> Ok (t, result)
    | None ->
      let retained_receipts =
        Map.filter t.receipts ~f:(fun receipt ->
          P.Timestamp.compare now receipt.Receipt.expires_at < 0)
      in
      if Map.length retained_receipts >= 4096
      then failure Conflict "organization receipt capacity exhausted"
      else (
        let%bind revision = next t.revision in
        let%bind changed, result =
          match mutation with
          | Mutation.Create_project request ->
            if Map.length t.projects >= 4096
            then failure Conflict "organization identity capacity exhausted"
            else (
              match candidate with
              | Some (Candidate.Project id) when not (Map.mem t.projects id) ->
                let%map group =
                  P.Organization_group.Project.create
                    ~id
                    ~creator_principal_id:principal.id
                    ~name:request.name
                    ~revision:0L
                    ~created_at:now
                    ~updated_at:now
                in
                ( { t with
                    projects =
                      Map.set
                        t.projects
                        ~key:id
                        ~data:Project_entry.{ group; deleted_at = None }
                  }
                , P.Organization_result.Project_created group )
              | Some (Candidate.Project _) ->
                failure Conflict "generated organization ID is already retained"
              | Some _ | None -> invalid "create requires matching generated identity")
          | Mutation.Update_project request ->
            if Option.is_some candidate
            then invalid "unexpected generated identity"
            else (
              match Map.find t.projects request.id with
              | None -> not_found ()
              | Some entry
                when Option.is_some entry.Project_entry.deleted_at
                     || not (owns principal entry.group.creator_principal_id) ->
                not_found ()
              | Some entry ->
                let%bind () =
                  check_revision entry.group.revision request.expected_revision
                in
                if P.Organization_group.Name.equal entry.group.name request.name
                then Ok (t, P.Organization_result.Project_updated entry.group)
                else (
                  let%bind group_revision = next entry.group.revision in
                  let updated_at =
                    if P.Timestamp.compare now entry.group.updated_at < 0
                    then entry.group.updated_at
                    else now
                  in
                  let%map group =
                    P.Organization_group.Project.create
                      ~id:entry.group.id
                      ~creator_principal_id:entry.group.creator_principal_id
                      ~name:request.name
                      ~revision:group_revision
                      ~created_at:entry.group.created_at
                      ~updated_at
                  in
                  ( { t with
                      projects =
                        Map.set t.projects ~key:group.id ~data:{ entry with group }
                    }
                  , P.Organization_result.Project_updated group )))
          | Mutation.Delete_project request ->
            if Option.is_some candidate
            then invalid "unexpected generated identity"
            else (
              match Map.find t.projects request.id with
              | None -> not_found ()
              | Some entry
                when Option.is_some entry.Project_entry.deleted_at
                     || not (owns principal entry.group.creator_principal_id) ->
                not_found ()
              | Some entry ->
                let%bind () =
                  check_revision entry.group.revision request.expected_revision
                in
                let%bind group_revision = next entry.group.revision in
                let deleted_at =
                  if P.Timestamp.compare now entry.group.updated_at < 0
                  then entry.group.updated_at
                  else now
                in
                let%bind group =
                  P.Organization_group.Project.create
                    ~id:entry.group.id
                    ~creator_principal_id:entry.group.creator_principal_id
                    ~name:entry.group.name
                    ~revision:group_revision
                    ~created_at:entry.group.created_at
                    ~updated_at:deleted_at
                in
                let%map deleted =
                  P.Organization_result.Project_deleted.create
                    ~id:group.id
                    ~revision:group_revision
                    ~deleted_at
                in
                ( { t with
                    projects =
                      Map.set
                        t.projects
                        ~key:group.id
                        ~data:Project_entry.{ group; deleted_at = Some deleted_at }
                  }
                , P.Organization_result.Project_deleted deleted ))
          | Mutation.Create_collection request ->
            if Map.length t.collections >= 4096
            then failure Conflict "organization identity capacity exhausted"
            else (
              match candidate with
              | Some (Candidate.Collection id) when not (Map.mem t.collections id) ->
                let%map group =
                  P.Organization_group.Collection.create
                    ~id
                    ~creator_principal_id:principal.id
                    ~name:request.name
                    ~revision:0L
                    ~created_at:now
                    ~updated_at:now
                in
                ( { t with
                    collections =
                      Map.set
                        t.collections
                        ~key:id
                        ~data:Collection_entry.{ group; deleted_at = None }
                  }
                , P.Organization_result.Collection_created group )
              | Some (Candidate.Collection _) ->
                failure Conflict "generated organization ID is already retained"
              | Some _ | None -> invalid "create requires matching generated identity")
          | Mutation.Update_collection request ->
            if Option.is_some candidate
            then invalid "unexpected generated identity"
            else (
              match Map.find t.collections request.id with
              | None -> not_found ()
              | Some entry
                when Option.is_some entry.Collection_entry.deleted_at
                     || not (owns principal entry.group.creator_principal_id) ->
                not_found ()
              | Some entry ->
                let%bind () =
                  check_revision entry.group.revision request.expected_revision
                in
                if P.Organization_group.Name.equal entry.group.name request.name
                then Ok (t, P.Organization_result.Collection_updated entry.group)
                else (
                  let%bind group_revision = next entry.group.revision in
                  let updated_at =
                    if P.Timestamp.compare now entry.group.updated_at < 0
                    then entry.group.updated_at
                    else now
                  in
                  let%map group =
                    P.Organization_group.Collection.create
                      ~id:entry.group.id
                      ~creator_principal_id:entry.group.creator_principal_id
                      ~name:request.name
                      ~revision:group_revision
                      ~created_at:entry.group.created_at
                      ~updated_at
                  in
                  ( { t with
                      collections =
                        Map.set t.collections ~key:group.id ~data:{ entry with group }
                    }
                  , P.Organization_result.Collection_updated group )))
          | Mutation.Delete_collection request ->
            if Option.is_some candidate
            then invalid "unexpected generated identity"
            else (
              match Map.find t.collections request.id with
              | None -> not_found ()
              | Some entry
                when Option.is_some entry.Collection_entry.deleted_at
                     || not (owns principal entry.group.creator_principal_id) ->
                not_found ()
              | Some entry ->
                let%bind () =
                  check_revision entry.group.revision request.expected_revision
                in
                let%bind group_revision = next entry.group.revision in
                let deleted_at =
                  if P.Timestamp.compare now entry.group.updated_at < 0
                  then entry.group.updated_at
                  else now
                in
                let%bind group =
                  P.Organization_group.Collection.create
                    ~id:entry.group.id
                    ~creator_principal_id:entry.group.creator_principal_id
                    ~name:entry.group.name
                    ~revision:group_revision
                    ~created_at:entry.group.created_at
                    ~updated_at:deleted_at
                in
                let%map deleted =
                  P.Organization_result.Collection_deleted.create
                    ~id:group.id
                    ~revision:group_revision
                    ~deleted_at
                in
                ( { t with
                    collections =
                      Map.set
                        t.collections
                        ~key:group.id
                        ~data:Collection_entry.{ group; deleted_at = Some deleted_at }
                  }
                , P.Organization_result.Collection_deleted deleted ))
        in
        let%bind expires_at = P.Timestamp.add_ms now 86_400_000 in
        let receipt =
          Receipt.
            { key = audit.key
            ; request_digest = audit.request_digest
            ; result
            ; created_at = now
            ; expires_at
            }
        in
        let changed =
          { changed with
            revision
          ; receipts = Map.set retained_receipts ~key:audit.key ~data:receipt
          }
        in
        let%map validated =
          restore
            ~server_id:changed.server_id
            ~revision:changed.revision
            ~projects:(projects changed)
            ~collections:(collections changed)
            ~receipts:(receipts changed)
        in
        validated, result))
;;

let project_entry t id = Map.find t.projects id
let collection_entry t id = Map.find t.collections id

let authorize_membership t ~principal ~host_id ~references ~require_live =
  let open Result.Let_syntax in
  let%bind () = require_scope principal Manage_organization in
  let%bind () =
    if P.Id.Server.equal host_id t.server_id
    then Ok ()
    else invalid "membership host differs from organization authority"
  in
  let authorize creator deleted_at =
    if (require_live && Option.is_some deleted_at) || not (owns principal creator)
    then not_found ()
    else Ok ()
  in
  let%bind () =
    match references.P.Session_organization.Values.project_id with
    | None -> Ok ()
    | Some id ->
      (match project_entry t id with
       | None -> not_found ()
       | Some entry -> authorize entry.group.creator_principal_id entry.deleted_at)
  in
  List.fold_result references.collection_ids ~init:() ~f:(fun () id ->
    match collection_entry t id with
    | None -> not_found ()
    | Some entry -> authorize entry.group.creator_principal_id entry.deleted_at)
;;
