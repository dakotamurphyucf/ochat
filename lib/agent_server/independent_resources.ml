open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module State = Agent_session.Session_state
module B = Agent_session.Runtime_builder
module Authority = Agent_session.Delegation_authority

type ancestor =
  { state : State.t
  ; owner : Runtime_owner.t
  }

type host =
  { find : P.Id.Session.t -> (ancestor, P.Error.t) result
  ; resolve : D.Reference.t -> (D.record, P.Error.t) result
  ; authorize : D.record -> (unit, P.Error.t) result
  ; build_root : sw:Eio.Switch.t -> ancestor -> (B.resources, P.Error.t) result
  ; build_generated :
      sw:Eio.Switch.t -> parent:B.resources -> ancestor -> (B.resources, P.Error.t) result
  }

type t = (P.Id.Session.t * B.resources) list

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let validate_state (state : State.t) =
  match state.moderator with
  | Some _ ->
    denied
      "delegation.independent_moderation_unavailable: independent ancestry requires an \
       available original policy owner"
  | None -> Authority.fingerprint state |> Result.map ~f:ignore
;;

let validate_record host reference =
  let open Result.Let_syntax in
  let%bind record = host.resolve reference in
  match
    D.Reference.equal reference (D.reference record), record.stage, record.revocation
  with
  | true, Linked, None ->
    let%map () = host.authorize record in
    record
  | _, _, Some _ -> denied "delegation.revoked: ancestor delegation was revoked"
  | _ -> denied "delegation.not_linked: independent ancestry is not privately linked"
;;

let validate_edge (record : D.record) (parent : State.t) =
  let open Result.Let_syntax in
  let%bind () = validate_state parent in
  let%bind fingerprint = Authority.fingerprint parent in
  match
    P.Id.Session.equal parent.identity.session_id record.key.parent_session_id
    && Int.equal parent.identity.generation record.key.parent_generation
    && P.Id.Prompt_revision.equal
         parent.spec.prompt_revision_id
         record.admission.parent_revision_id
    && String.equal fingerprint record.admission.authority_sha256
  with
  | true -> Ok ()
  | false ->
    denied "delegation.authority_changed: independent ancestor identity or policy changed"
;;

let collect ~max_depth host ~parent_id =
  let open Result.Let_syntax in
  let rec loop visited id =
    match
      List.length visited >= max_depth || List.mem visited id ~equal:P.Id.Session.equal
    with
    | true ->
      denied "delegation.ancestry_limit: independent ancestry is cyclic or excessive"
    | false ->
      let%bind ancestor = host.find id in
      let%bind () =
        match P.Id.Session.equal id ancestor.state.identity.session_id with
        | true -> Ok ()
        | false -> denied "delegation.ancestry_changed: foreign ancestor state"
      in
      let%bind () = validate_state ancestor.state in
      let%bind parents =
        match ancestor.state.spec.delegation with
        | None -> Ok []
        | Some reference ->
          let%bind record = validate_record host reference in
          let%bind () =
            match
              P.Id.Session.equal id reference.child_session_id
              && P.Id.Prompt_revision.equal
                   ancestor.state.spec.prompt_revision_id
                   reference.revision_id
            with
            | true -> Ok ()
            | false -> denied "delegation.ancestry_changed: foreign parent reference"
          in
          let%bind parents = loop (id :: visited) record.key.parent_session_id in
          let parent = (List.last_exn parents).state in
          let%bind () = validate_edge record parent in
          let%map () =
            match
              String.equal
                ancestor.state.spec.permission_profile
                parent.spec.permission_profile
              && String.equal
                   ancestor.state.spec.permission_profile_digest
                   parent.spec.permission_profile_digest
            with
            | true -> Ok ()
            | false ->
              denied
                "delegation.child_profile: ancestor changed its inherited permission \
                 profile"
          in
          parents
      in
      Ok (parents @ [ ancestor ])
  in
  loop [] parent_id
;;

let check_parent ~max_depth ~host ~parent_id =
  collect ~max_depth host ~parent_id |> Result.map ~f:ignore
;;

let find t id =
  List.Assoc.find t id ~equal:P.Id.Session.equal
  |> Result.of_option
       ~error:
         (P.Error.create
            Permission_denied
            ~message:
              "delegation.resources_unavailable: ancestor resources are not retained"
            ~retryable:false
            ())
;;

let with_chain ~max_depth ~host ~reference ~f =
  let open Result.Let_syntax in
  let%bind record = validate_record host reference in
  let%bind () =
    match record.admission.lifetime with
    | Independent _ -> Ok ()
    | Owned ->
      denied
        "delegation.lifetime_denied: independent resources require explicit lifetime \
         authority"
  in
  let%bind ancestors = collect ~max_depth host ~parent_id:record.key.parent_session_id in
  let%bind () = validate_edge record (List.last_exn ancestors).state in
  let rec borrow = function
    | [] ->
      Eio.Switch.run (fun sw ->
        let%bind resources =
          List.fold_result ancestors ~init:[] ~f:(fun resources ancestor ->
            let%map prepared =
              match List.last resources with
              | None -> host.build_root ~sw ancestor
              | Some (_, parent) -> host.build_generated ~sw ~parent ancestor
            in
            resources @ [ ancestor.state.identity.session_id, prepared ])
        in
        (* Construction may yield while policies or private records change.
           Recheck before exposing the prepared chain to child construction. *)
        let%bind latest_record = validate_record host reference in
        let%bind latest =
          collect ~max_depth host ~parent_id:record.key.parent_session_id
        in
        let%bind () = validate_edge latest_record (List.last_exn latest).state in
        f resources)
    | ancestor :: rest ->
      Runtime_owner.with_resource_lifetime ancestor.owner (fun () -> borrow rest)
  in
  borrow ancestors
;;
