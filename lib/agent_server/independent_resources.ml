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
  ; build_root :
      sw:Eio.Switch.t
      -> register_tracking:(Graph_tracking.t -> unit)
      -> ancestor
      -> (B.resources, P.Error.t) result
  ; build_generated :
      sw:Eio.Switch.t -> parent:B.resources -> ancestor -> (B.resources, P.Error.t) result
  }

module Source_fingerprint = struct
  type t = ancestor -> (string, P.Error.t) result
end

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

let validate_edge_identity (record : D.record) (parent : State.t) ~fingerprint =
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

let validate_edge (record : D.record) (parent : State.t) =
  let open Result.Let_syntax in
  let%bind () = validate_state parent in
  let%bind fingerprint = Authority.fingerprint parent in
  validate_edge_identity record parent ~fingerprint
;;

let collect ?source_fingerprint ~max_depth host ~parent_id =
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
      let%bind () =
        match source_fingerprint with
        | None -> validate_state ancestor.state
        | Some fingerprint -> fingerprint ancestor |> Result.map ~f:ignore
      in
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
          let parent = List.last_exn parents in
          let%bind () =
            match source_fingerprint with
            | None -> validate_edge record parent.state
            | Some fingerprint ->
              let%bind fingerprint = fingerprint parent in
              validate_edge_identity record parent.state ~fingerprint
          in
          let%map () =
            match
              String.equal
                ancestor.state.spec.permission_profile
                parent.state.spec.permission_profile
              && String.equal
                   ancestor.state.spec.permission_profile_digest
                   parent.state.spec.permission_profile_digest
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

let check_parent_with_source ~max_depth ~host ~source_fingerprint ~parent_id =
  collect ~source_fingerprint ~max_depth host ~parent_id |> Result.map ~f:ignore
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

module CO = Agent_session.Close_observation
module Observed = Native_cleanup_observation

module Cleanup = struct
  type t = Ancestry_cleanup.t

  module Own_post_callback_check = Ancestry_cleanup.Own_post_callback_check
  module Snapshot = Ancestry_cleanup.Snapshot
  module Retry = Ancestry_cleanup.Retry

  let create = Ancestry_cleanup.create
  let graph = Ancestry_cleanup.graph
  let snapshot = Ancestry_cleanup.snapshot
  let finish_without_entry = Ancestry_cleanup.finish_without_entry
  let retry_observed = Ancestry_cleanup.retry_observed
end

type resources = t

module Callback = struct
  type 'a t =
    | Raw : (resources -> ('a, P.Error.t) result) -> 'a t
    | Observed : (resources -> (unit, P.Error.t) result CO.t) -> unit t
end

let append_observation previous next =
  CO.create
    (CO.exit next)
    ~diagnostics:(CO.diagnostics previous @ CO.diagnostics next)
    ?primary:(if Observed.is_negative next then CO.primary next else None)
    ()
;;

let capture_infrastructure cleanup f =
  match f () with
  | observed -> observed
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    let observed =
      Observed.capture (fun () -> Exn.raise_with_original_backtrace exn trace)
    in
    Ancestry_cleanup.record_control
      cleanup
      ~role:Unknown
      (CO.create
         (Raised (exn, trace))
         ~diagnostics:(CO.diagnostics observed)
         ?primary:(CO.primary observed)
         ());
    observed
;;

let bind cleanup observed f =
  match CO.exit observed with
  | Returned (Ok value) ->
    append_observation observed (capture_infrastructure cleanup (fun () -> f value))
  | Returned (Error error) ->
    CO.create
      (Returned (Error error))
      ~diagnostics:(CO.diagnostics observed)
      ?primary:(CO.primary observed)
      ()
  | Raised (exn, trace) ->
    CO.create
      (Raised (exn, trace))
      ~diagnostics:(CO.diagnostics observed)
      ?primary:(CO.primary observed)
      ()
;;

let control cleanup f =
  let observed = Observed.capture f in
  (match CO.exit observed with
   | Returned (Ok _) -> ()
   | Returned (Error error) ->
     Ancestry_cleanup.record_control
       cleanup
       ~role:Preparation
       (CO.create
          (Returned (Error error))
          ~diagnostics:(CO.diagnostics observed)
          ?primary:(CO.primary observed)
          ())
   | Raised (exn, trace) ->
     Ancestry_cleanup.record_control
       cleanup
       ~role:Preparation
       (CO.create
          (Raised (exn, trace))
          ~diagnostics:(CO.diagnostics observed)
          ?primary:(CO.primary observed)
          ()));
  observed
;;

let protect_observation body finally =
  let exit =
    match
      Exn.protect
        ~f:(fun () -> CO.resume body)
        ~finally:(fun () -> ignore (CO.resume finally : (unit, P.Error.t) result))
    with
    | result -> CO.Returned result
    | exception exn -> CO.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let primary =
    match CO.exit body, CO.exit finally with
    | Raised _, _ -> CO.primary body
    | Returned _, Raised _ -> CO.primary finally
    | Returned _, Returned _ ->
      if Observed.is_negative body then CO.primary body else None
  in
  CO.create exit ~diagnostics:(CO.diagnostics body @ CO.diagnostics finally) ?primary ()
;;

let with_chain_internal
      (type a)
      ?consumer
      ~cleanup
      ~max_depth
      ~host
      ~reference
      (callback : a Callback.t)
  =
  Ancestry_cleanup.begin_original cleanup;
  let graph = Ancestry_cleanup.graph cleanup in
  let run_resources sw register_tracking ancestors (record : D.record) : a Observed.t =
    let rec build resources = function
      | [] ->
        bind
          cleanup
          (control cleanup (fun () -> validate_record host reference))
          (fun latest_record ->
             bind
               cleanup
               (control cleanup (fun () ->
                  collect ~max_depth host ~parent_id:record.key.parent_session_id))
               (fun latest ->
                  bind
                    cleanup
                    (control cleanup (fun () ->
                       validate_edge latest_record (List.last_exn latest).state))
                    (fun () ->
                       match callback with
                       | Raw f -> Observed.capture (fun () -> f resources)
                       | Observed f -> Observed.capture_observed (fun () -> f resources))))
      | ancestor :: rest ->
        let prepared =
          control cleanup (fun () ->
            match List.last resources with
            | None -> host.build_root ~sw ~register_tracking ancestor
            | Some (_, parent) -> host.build_generated ~sw ~parent ancestor)
        in
        bind cleanup prepared (fun prepared ->
          build (resources @ [ ancestor.state.identity.session_id, prepared ]) rest)
    in
    build [] ancestors
  in
  let body =
    capture_infrastructure cleanup (fun () ->
      bind
        cleanup
        (control cleanup (fun () -> validate_record host reference))
        (fun record ->
           bind
             cleanup
             (control cleanup (fun () ->
                match record.admission.lifetime with
                | Independent _ -> Ok ()
                | Owned | Invocation_owned _ ->
                  denied
                    "delegation.lifetime_denied: independent resources require explicit \
                     lifetime authority"))
             (fun () ->
                bind
                  cleanup
                  (control cleanup (fun () ->
                     collect ~max_depth host ~parent_id:record.key.parent_session_id))
                  (fun ancestors ->
                     bind
                       cleanup
                       (control cleanup (fun () ->
                          validate_edge record (List.last_exn ancestors).state))
                       (fun () ->
                          let rec borrow : ancestor list -> a Observed.t = function
                            | [] ->
                              (match callback with
                               | Raw _ ->
                                 Observed.capture (fun () ->
                                   Independent_cleanup.run
                                     graph
                                     ~f:(fun ~sw ~register_tracking ->
                                       run_resources sw register_tracking ancestors record
                                       |> CO.resume)
                                   |> Independent_cleanup.Outcome.result
                                   |> Independent_cleanup.Failure.resume)
                               | Observed _ ->
                                 Independent_cleanup.run_observed
                                   graph
                                   ~f:(fun ~sw ~register_tracking ->
                                     run_resources sw register_tracking ancestors record)
                                 |> Independent_cleanup.Outcome.observation)
                            | ancestor :: rest ->
                              let captured = ref None in
                              let capture recovery =
                                captured
                                := Some
                                     (Ancestry_cleanup.capture_lease
                                        cleanup
                                        ~owner:ancestor.owner
                                        recovery)
                              in
                              (match callback with
                               | Raw _ ->
                                 let outcome =
                                   Runtime_owner.with_resource_lifetime_outcome
                                     ?consumer
                                     ancestor.owner
                                     ~capture
                                     (fun () -> borrow rest |> CO.resume)
                                 in
                                 Ancestry_cleanup.record_legacy_lease
                                   cleanup
                                   (Ancestry_cleanup.finish_capture
                                      cleanup
                                      ~owner:ancestor.owner
                                      ~captured:!captured
                                      ~actual:
                                        (Runtime_owner.Lease_outcome.cleanup outcome))
                                   ~outcome;
                                 Observed.capture (fun () ->
                                   Runtime_owner.Lease_outcome.outcome outcome
                                   |> Runtime_owner.Scope_outcome.resume)
                               | Observed _ ->
                                 let passed = ref None in
                                 let outcome =
                                   Runtime_owner.with_resource_lifetime_observed
                                     ?consumer
                                     ancestor.owner
                                     ~capture
                                     (fun () ->
                                        let observed = borrow rest in
                                        passed := Some observed;
                                        observed)
                                 in
                                 Ancestry_cleanup.record_lease
                                   cleanup
                                   (Ancestry_cleanup.finish_capture
                                      cleanup
                                      ~owner:ancestor.owner
                                      ~captured:!captured
                                      ~actual:
                                        (Runtime_owner.Lease_outcome.cleanup outcome))
                                   ~callback:!passed
                                   ~outcome;
                                 Runtime_owner.Lease_outcome.observation outcome)
                          in
                          borrow ancestors)))))
  in
  (* Preserve the existing finalizer even when the original body escaped. The
     DATA projection below uses Exn.protect's exact Finally selection. *)
  let finished = Ancestry_cleanup.seal_observed cleanup in
  protect_observation body finished
;;

let with_chain_observed ?consumer cleanup ~max_depth ~host ~reference ~f =
  with_chain_internal ?consumer ~cleanup ~max_depth ~host ~reference (Callback.Observed f)
;;

let with_chain_reported ~cleanup:graph ~max_depth ~host ~reference ~f =
  let cleanup = Ancestry_cleanup.create_with_graph graph in
  (* Legacy external graph ownership stays exact; this holder retains the
     actual ancestor leases but does not replace the caller's graph holder. *)
  with_chain_internal ~cleanup ~max_depth ~host ~reference (Callback.Raw f) |> CO.resume
;;

let with_chain ~max_depth ~host ~reference ~f =
  let cleanup = Ancestry_cleanup.create () in
  with_chain_internal ~cleanup ~max_depth ~host ~reference (Callback.Raw f) |> CO.resume
;;
