open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator
module M = Credential_registry_model
module C = Credential_registry
module Bridge = Inference_host.Credential_bridge
module O = Provider_oauth
module OR = Provider_oauth_registry
module Owner_records = Owner_records
module Command_intents = Command_intents
module Profile_admin = Profile_admin

module Environment_source = struct
  type t =
    { id : DTO.Source_id.t
    ; name : string
    ; revision : M.Id.t option
    }

  let create ~id ~name ~revision =
    if
      String.is_empty name
      || String.length name > 128
      || (not
            (String.for_all name ~f:(function
               | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
               | _ -> false)))
      ||
      match name.[0] with
      | '0' .. '9' -> true
      | _ -> false
    then Error DTO.Error.Invalid_request
    else Ok { id; name; revision }
  ;;
end

type live =
  { record : Owner_records.Record.t
  ; actor : Actor.t
  ; stop : unit Eio.Promise.t
  ; stop_resolver : unit Eio.Promise.u
  ; finished : unit Eio.Promise.t
  ; mutable cancelled : bool
  ; mutable challenge : DTO.Private_challenge.t option
  }

type t =
  { sw : Eio.Switch.t
  ; server_id : P.Id.Server.t
  ; host : M.Id.t
  ; incarnation : M.Id.t
  ; registry : C.t
  ; bridge : Bridge.t
  ; profiles : Profile_admin.t
  ; oauth : OR.t
  ; records : Owner_records.t
  ; intents : Command_intents.t
  ; start_login :
      sw:Eio.Switch.t
      -> template:Profile_admin.Template.t
      -> mode:DTO.Login_mode.t
      -> (O.Login.t * O.Challenge.t, O.Error.t) Result.t
  ; authorize : Actor.t -> operation:DTO.Operation.t -> profile:DTO.Profile_id.t -> bool
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; maximum_wait : Time_ns.Span.t
  ; now : unit -> P.Timestamp.t
  ; new_operation : unit -> M.Id.t
  ; limits : DTO.Limits.t
  ; environment : Environment_source.t list
  ; live : live String.Table.t
  ; unpublished_terminal : (Owner_records.Record.t * DTO.Flow_result.phase) String.Table.t
  ; mutable closed : bool
  }

let owner_error = function
  | Owner_records.Error.Busy -> DTO.Error.Busy
  | Missing -> Flow_interrupted
  | Full -> Busy
  | Conflict | Corrupt | Storage _ -> Store_unavailable
;;

let registry_error = function
  | C.Error.Authorization_denied -> DTO.Error.Denied
  | Busy | Timed_out -> DTO.Error.Busy
  | Closed -> Closed
  | Publication_uncertain | Binding_unavailable -> Submission_uncertain
  | Model _
  | Storage _
  | Secret_store _
  | Missing_secret
  | Renewal_rejected
  | Revision_quarantined -> Store_unavailable
;;

let bridge_error = function
  | Bridge.Error.Denied -> DTO.Error.Denied
  | Missing_profile -> Missing_profile
  | Invalid_mapping | Invalid_environment | Invalid_credential -> Invalid_request
  | Stale_authorization -> Denied
  | Lifecycle error -> registry_error error
  | Profile _ | Preparation _ -> Store_unavailable
;;

let profile_error = function
  | Profile_admin.Error.Missing_profile -> DTO.Error.Missing_profile
  | Busy -> Busy
  | Publication_uncertain -> Submission_uncertain
  | Authorization_denied -> Denied
  | Invalid_template | Conflict -> Invalid_request
  | Registry error -> registry_error error
  | Bridge error -> bridge_error error
  | Missing_setup | Storage _ -> Store_unavailable
;;

let publish_original t ~template ~operation =
  let started = Eio.Time.Mono.now t.clock in
  let metadata_busy = function
    | Profile_admin.Error.Busy
    | Registry C.Error.Busy
    | Bridge (Bridge.Error.Lifecycle C.Error.Busy) -> true
    | _ -> false
  in
  let expired () =
    let elapsed =
      Mtime.span started (Eio.Time.Mono.now t.clock) |> Mtime.Span.to_float_ns
    in
    Float.(elapsed >= Time_ns.Span.to_ns t.maximum_wait)
  in
  let rec publish ~initial =
    if (not initial) && expired ()
    then Error DTO.Error.Submission_uncertain
    else (
      match Profile_admin.publish_committed t.profiles ~template ~operation with
      | Error
          (Registry C.Error.Timed_out | Bridge (Bridge.Error.Lifecycle C.Error.Timed_out))
        -> Error DTO.Error.Submission_uncertain
      | Error error when metadata_busy error ->
        if expired ()
        then Error DTO.Error.Submission_uncertain
        else (
          Eio.Time.Mono.sleep t.clock 0.01;
          publish ~initial:false)
      | result -> Result.map_error result ~f:profile_error)
  in
  publish ~initial:true
;;

let oauth_error = function
  | OR.Error.Denied -> DTO.Error.Denied
  | Invalid_binding -> Invalid_request
  | Closed -> Closed
  | Registry error -> registry_error error
  | OAuth error ->
    (match O.Error.code error with
     | Submission_uncertain -> Submission_uncertain
     | Authorization_denied -> Denied
     | Transport_unavailable -> Network
     | Timed_out -> Flow_expired
     | Closed -> Closed
     | _ -> Invalid_request)
;;

let required_scope = function
  | DTO.Operation.Status -> P.Scope.Provider_view
  | Select -> Provider_select
  | Setup | Login | Challenge | Cancel | Logout | Configure_environment -> Provider_manage
;;

let can_operate actor operation =
  Actor.is_current actor
  && P.Principal.has_scope (Actor.principal actor) (required_scope operation)
;;

let authorized t actor ~operation ~profile =
  can_operate actor operation && t.authorize actor ~operation ~profile
;;

let check t actor operation profile =
  if t.closed
  then Error DTO.Error.Closed
  else if not (authorized t actor ~operation ~profile)
  then Error DTO.Error.Denied
  else Ok ()
;;

let flow_key (flow : DTO.Flow_ref.t) = DTO.Flow_id.to_string flow.flow_id

let find_owned t actor operation flow =
  let open Result.Let_syntax in
  let%bind () = check t actor operation flow.DTO.Flow_ref.profile in
  let%bind () =
    if P.Id.Server.equal flow.server_id t.server_id then Ok () else Error DTO.Error.Denied
  in
  let%bind record =
    Owner_records.find t.records flow |> Result.map_error ~f:owner_error
  in
  let%map () =
    if P.Id.Principal.equal (Owner_records.Record.owner record) (Actor.principal actor).id
    then Ok ()
    else Error DTO.Error.Denied
  in
  record
;;

let update_phase t flow phase =
  Owner_records.set_phase t.records flow ~phase
  |> Result.map_error ~f:owner_error
  |> Result.map ~f:Owner_records.Record.result
;;

let record_terminal t record phase =
  let flow = (Owner_records.Record.result record).flow in
  let key = flow_key flow in
  (* Retain the exact computed outcome if bounded metadata admission or native
     publication fails. Status can truthfully project it after the worker joins;
     restart recovery still requires the original registry receipt. *)
  let phase =
    match Hashtbl.find t.unpublished_terminal key with
    | Some (_, original_phase) -> original_phase
    | None ->
      Hashtbl.set t.unpublished_terminal ~key ~data:(record, phase);
      phase
  in
  let result =
    Owner_records.set_phase_wait
      t.records
      flow
      ~phase
      ~clock:t.clock
      ~maximum_wait:t.maximum_wait
    |> Result.map_error ~f:owner_error
  in
  (match result with
   | Ok _ -> Hashtbl.remove t.unpublished_terminal key
   | Error _ -> ());
  result
;;

let private_challenge ~(mode : DTO.Login_mode.t) challenge =
  match mode with
  | Browser ->
    O.Challenge.with_browser_uri challenge ~f:(fun authorization_uri ->
      DTO.Private_challenge.browser ~authorization_uri)
    |> Result.map_error ~f:(fun _ -> DTO.Error.Challenge_unavailable)
    |> Result.bind ~f:(Result.map_error ~f:(fun _ -> DTO.Error.Invalid_request))
  | Device ->
    O.Challenge.with_device_prompt challenge ~f:(fun ~verification_uri ~user_code ->
      DTO.Private_challenge.device ~verification_uri ~user_code)
    |> Result.map_error ~f:(fun _ -> DTO.Error.Challenge_unavailable)
    |> Result.bind ~f:(Result.map_error ~f:(fun _ -> DTO.Error.Invalid_request))
;;

let reconcile_cancel t template record =
  match
    C.reconcile_operation
      t.registry
      ~binding:(Owner_records.Record.binding record)
      ~operation:(Owner_records.Record.operation record)
  with
  | Ok M.Operation.Committed ->
    (match
       Profile_admin.publish_committed
         t.profiles
         ~template
         ~operation:(Owner_records.Record.operation record)
     with
     | Ok () -> DTO.Flow_result.Completed
     | Error _ -> Failed Submission_uncertain)
  | Ok Pending ->
    (match
       C.cancel_pending_candidate
         t.registry
         ~binding:(Owner_records.Record.binding record)
         ~operation:(Owner_records.Record.operation record)
     with
     | Ok () -> Cancelled
     | Error _ -> Failed Submission_uncertain)
  | Ok Rejected -> Cancelled
  | Ok Unavailable | Error _ -> Failed Submission_uncertain
;;

let stop_live live =
  live.cancelled <- true;
  live.challenge <- None;
  if not (Eio.Promise.is_resolved live.stop)
  then Eio.Promise.resolve live.stop_resolver ()
;;

let join_live live = Eio.Cancel.protect (fun () -> Eio.Promise.await live.finished)

let close t =
  if not t.closed
  then (
    t.closed <- true;
    let live = Hashtbl.data t.live in
    List.iter live ~f:stop_live;
    List.iter live ~f:join_live)
;;

let create
      ~sw
      ~server_id
      ~host
      ~incarnation
      ~registry
      ~bridge
      ~profiles
      ~oauth
      ~owner_records
      ~command_intents
      ~start_login
      ~authorize
      ~clock
      ~maximum_wait
      ~now
      ~new_operation
      ~limits
      ~environment
  =
  let open Result.Let_syntax in
  let%bind () =
    if
      List.length (Profile_admin.templates profiles) > DTO.Limits.max_profiles limits
      || Time_ns.Span.(maximum_wait <= zero)
      || Option.is_some
           (List.find_a_dup
              (List.map environment ~f:(fun e ->
                 DTO.Source_id.to_string e.Environment_source.id))
              ~compare:String.compare)
    then Error DTO.Error.Invalid_request
    else Ok ()
  in
  let t =
    { sw
    ; server_id
    ; host
    ; incarnation
    ; registry
    ; bridge
    ; profiles
    ; oauth
    ; records = owner_records
    ; intents = command_intents
    ; start_login
    ; authorize
    ; clock :> Eio.Time.Mono.ty Eio.Time.Mono.t
    ; maximum_wait
    ; now
    ; new_operation
    ; limits
    ; environment
    ; live = String.Table.create ()
    ; unpublished_terminal = String.Table.create ()
    ; closed = false
    }
  in
  let%bind records =
    Owner_records.list owner_records |> Result.map_error ~f:owner_error
  in
  let%bind () =
    List.fold_result records ~init:() ~f:(fun () record ->
      match (Owner_records.Record.result record).phase with
      | Pending ->
        let flow = (Owner_records.Record.result record).flow in
        let%bind alive =
          Owner_records.is_live owner_records flow |> Result.map_error ~f:owner_error
        in
        if alive then Ok () else update_phase t flow Interrupted |> Result.map ~f:ignore
      | Completed | Failed _ | Cancelled | Interrupted | Expired -> Ok ())
  in
  let%bind () = Profile_admin.synchronize profiles |> Result.map_error ~f:profile_error in
  Eio.Switch.on_release sw (fun () -> close t);
  Ok t
;;

let login_receipt t ~actor ~key =
  let open Result.Let_syntax in
  let%bind () =
    if t.closed
    then Error DTO.Error.Closed
    else if can_operate actor Login
    then Ok ()
    else Error DTO.Error.Denied
  in
  let%bind records = Owner_records.list t.records |> Result.map_error ~f:owner_error in
  match
    List.find records ~f:(fun record ->
      P.Id.Principal.equal (Owner_records.Record.owner record) (Actor.principal actor).id
      && P.Idempotency_key.equal (Owner_records.Record.key record) key)
  with
  | None -> Ok None
  | Some record ->
    let result = Owner_records.Record.result record in
    let%map () = check t actor DTO.Operation.Login result.flow.profile in
    Some result
;;

let intent_error = function
  | Command_intents.Error.Busy | Full -> DTO.Error.Busy
  | Conflict -> Invalid_request
  | Corrupt | Storage _ -> Store_unavailable
;;

let admit_intent t actor key method_name params =
  Command_intents.begin_
    t.intents
    ~principal:(Actor.principal actor).id
    ~key
    ~method_name
    ~params
    ~operation:(t.new_operation ())
  |> Result.map_error ~f:intent_error
;;

let original_intent = function
  | Command_intents.Fresh intent | Existing intent -> intent
;;

let finish_intent t intent result value =
  Command_intents.complete t.intents intent result
  |> Result.map_error ~f:intent_error
  |> Result.map ~f:(fun () -> value)
;;

let begin_login t ~actor (request : DTO.Login_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Login request.profile in
  let%bind template =
    Profile_admin.find_template t.profiles request.profile
    |> Result.map_error ~f:profile_error
  in
  let%bind () =
    match Profile_admin.Template.authentication template with
    | Api_key -> Error DTO.Error.Invalid_request
    | Direct_codex -> Ok ()
  in
  let%bind admission =
    admit_intent
      t
      actor
      request.idempotency_key
      "provider.login.begin"
      (DTO.Login_request.to_json request)
  in
  let intent = original_intent admission in
  let%bind existing = login_receipt t ~actor ~key:request.idempotency_key in
  match existing with
  | Some result ->
    let%bind record =
      Owner_records.find t.records result.flow |> Result.map_error ~f:owner_error
    in
    if
      DTO.Profile_id.equal result.flow.profile request.profile
      && DTO.Login_mode.equal (Owner_records.Record.mode record) request.mode
    then finish_intent t intent (P.Command_receipt.Provider_login result.flow) result.flow
    else Error DTO.Error.Invalid_request
  | None ->
    let%bind () =
      if
        Hashtbl.length t.live >= DTO.Limits.max_flows t.limits
        || Hashtbl.exists t.live ~f:(fun live ->
          M.Id.equal
            (Owner_records.Record.binding live.record)
            (Profile_admin.Template.binding template))
      then Error DTO.Error.Busy
      else Ok ()
    in
    let%bind () =
      match admission with
      | Fresh _ -> Ok ()
      | Existing _ -> Error DTO.Error.Submission_uncertain
    in
    let operation = Command_intents.Intent.operation intent in
    let%bind flow_id =
      DTO.Flow_id.of_string (M.Id.to_string operation)
      |> Result.map_error ~f:(fun _ -> DTO.Error.Invalid_request)
    in
    let%bind expires_at =
      P.Timestamp.add_ms (t.now ()) (DTO.Limits.max_flow_seconds t.limits * 1000)
      |> Result.map_error ~f:(fun _ -> DTO.Error.Invalid_request)
    in
    let flow =
      { DTO.Flow_ref.server_id = t.server_id
      ; profile = request.profile
      ; flow_id
      ; expires_at
      }
    in
    let%bind flow_lease =
      Owner_records.claim t.records flow ~sw:t.sw |> Result.map_error ~f:owner_error
    in
    let record_result =
      Owner_records.begin_
        t.records
        (Owner_records.Record.create
           ~incarnation:t.incarnation
           ~owner:(Actor.principal actor).id
           ~operation
           ~binding:(Profile_admin.Template.binding template)
           ~key:request.idempotency_key
           ~mode:request.mode
           ~flow)
      |> Result.map_error ~f:owner_error
    in
    let%bind record =
      match record_result with
      | Ok record -> Ok record
      | Error error ->
        Private_storage.Lock.release flow_lease;
        Error error
    in
    (* A concurrent host may already own this original command. Never start a
       second worker when durable admission returns another flow. *)
    let actual = (Owner_records.Record.result record).flow in
    if not (DTO.Flow_id.equal actual.flow_id flow.flow_id)
    then (
      Private_storage.Lock.release flow_lease;
      finish_intent t intent (P.Command_receipt.Provider_login actual) actual)
    else (
      let stop, stop_resolver = Eio.Promise.create () in
      let finished, finished_resolver = Eio.Promise.create () in
      let ready, ready_resolver = Eio.Promise.create () in
      let live =
        { record
        ; actor
        ; stop
        ; stop_resolver
        ; finished
        ; cancelled = false
        ; challenge = None
        }
      in
      Hashtbl.add_exn t.live ~key:(flow_key flow) ~data:live;
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        let ready_once result =
          if not (Eio.Promise.is_resolved ready)
          then Eio.Promise.resolve ready_resolver result
        in
        Exn.protect
          ~finally:(fun () ->
            live.challenge <- None;
            Private_storage.Lock.release flow_lease;
            Hashtbl.remove t.live (flow_key flow);
            ready_once (Error DTO.Error.Flow_interrupted);
            Eio.Promise.resolve finished_resolver ())
          ~f:(fun () ->
            try
              let result =
                Eio.Switch.run (fun sw ->
                  match
                    OR.Acquisition.start
                      t.oauth
                      ~registry:t.registry
                      ~sw
                      ~host:t.host
                      ~binding:(Profile_admin.Template.binding template)
                      ~operation
                      ~expectation:(Profile_admin.Template.expectation template)
                      ~refresh_policy:Require_rotated
                      ~start:(fun ~sw -> t.start_login ~sw ~template ~mode:request.mode)
                  with
                  | Error error -> Error (oauth_error error)
                  | Ok (acquisition, challenge) ->
                    Exn.protect
                      ~finally:(fun () ->
                        Eio.Cancel.protect (fun () ->
                          ignore
                            (OR.Acquisition.close acquisition
                             : (unit, OR.Error.t) Result.t)))
                      ~f:(fun () ->
                        match private_challenge ~mode:request.mode challenge with
                        | Error error -> Error error
                        | Ok challenge ->
                          live.challenge <- Some challenge;
                          ready_once (Ok flow);
                          Eio.Fiber.first
                            (fun () ->
                               OR.Acquisition.complete
                                 acquisition
                                 ~authorize_commit:(fun () ->
                                   (not t.closed)
                                   && (not live.cancelled)
                                   && P.Timestamp.compare (t.now ()) flow.expires_at < 0
                                   && authorized
                                        t
                                        actor
                                        ~operation:Login
                                        ~profile:request.profile)
                               |> Result.map_error ~f:(fun error ->
                                 match error with
                                 | OR.Error.Denied
                                   when P.Timestamp.compare (t.now ()) flow.expires_at
                                        >= 0 -> DTO.Error.Flow_expired
                                 | _ -> oauth_error error)
                               |> Result.bind ~f:(fun () ->
                                 publish_original t ~template ~operation))
                            (fun () ->
                               Eio.Promise.await stop;
                               Error DTO.Error.Flow_interrupted)))
              in
              let phase =
                if live.cancelled
                then reconcile_cancel t template record
                else (
                  match result with
                  | Ok () -> DTO.Flow_result.Completed
                  | Error DTO.Error.Flow_expired -> Expired
                  | Error error -> Failed error)
              in
              ignore
                (record_terminal t record phase
                 : (Owner_records.Record.t, DTO.Error.t) Result.t);
              ready_once (Result.map result ~f:(fun () -> flow))
            with
            | Eio.Cancel.Cancelled _ as ex ->
              Eio.Cancel.protect (fun () ->
                ignore
                  (record_terminal t record Interrupted
                   : (Owner_records.Record.t, DTO.Error.t) Result.t));
              raise ex
            | _ ->
              Eio.Cancel.protect (fun () ->
                ignore
                  (record_terminal t record (Failed Submission_uncertain)
                   : (Owner_records.Record.t, DTO.Error.t) Result.t))));
      let%bind ready_result = Eio.Promise.await ready in
      finish_intent t intent (P.Command_receipt.Provider_login ready_result) ready_result)
;;

let challenge t ~actor ~flow =
  let open Result.Let_syntax in
  let%bind _ = find_owned t actor Challenge flow in
  let%bind () = check t actor Challenge flow.profile in
  let%bind () =
    if P.Timestamp.compare (t.now ()) flow.expires_at >= 0
    then Error DTO.Error.Flow_expired
    else Ok ()
  in
  match Hashtbl.find t.live (flow_key flow) with
  | Some live when not live.cancelled ->
    Result.of_option live.challenge ~error:DTO.Error.Challenge_unavailable
  | _ -> Error DTO.Error.Challenge_unavailable
;;

let cancel_impl t ~actor (request : DTO.Cancel_request.t) =
  let open Result.Let_syntax in
  let%bind record = find_owned t actor Cancel request.flow in
  let%bind template =
    Profile_admin.find_template t.profiles request.flow.profile
    |> Result.map_error ~f:profile_error
  in
  let%bind () = check t actor Cancel request.flow.profile in
  (match Hashtbl.find t.live (flow_key request.flow) with
   | None -> ()
   | Some live ->
     stop_live live;
     join_live live);
  match (Owner_records.Record.result record).phase with
  | Completed | Failed _ | Cancelled | Expired ->
    Owner_records.find t.records request.flow
    |> Result.map_error ~f:owner_error
    |> Result.map ~f:Owner_records.Record.result
  | Pending | Interrupted ->
    Eio.Switch.run (fun sw ->
      match Owner_records.claim t.records request.flow ~sw with
      | Error error -> Error (owner_error error)
      | Ok lease ->
        Exn.protect
          ~finally:(fun () -> Private_storage.Lock.release lease)
          ~f:(fun () -> update_phase t request.flow (reconcile_cancel t template record)))
;;

let snapshot_for t binding =
  C.synchronize t.registry
  |> Result.map_error ~f:registry_error
  |> Result.bind ~f:(fun snapshot ->
    List.find (C.Host_snapshot.bindings snapshot) ~f:(fun item ->
      M.Id.equal (C.Host_snapshot.id item) binding)
    |> Result.of_option ~error:DTO.Error.Missing_profile)
;;

let configuration_result t profile operation =
  let open Result.Let_syntax in
  let%bind template =
    Profile_admin.find_template t.profiles profile |> Result.map_error ~f:profile_error
  in
  let%bind snapshot = snapshot_for t (Profile_admin.Template.binding template) in
  let%map revision =
    DTO.Revision.of_string (M.Id.to_string operation)
    |> Result.map_error ~f:(fun _ -> DTO.Error.Invalid_request)
  in
  { DTO.Configuration_result.profile
  ; auth_epoch = C.Host_snapshot.epoch snapshot
  ; revision
  }
;;

let project_flow t record =
  let result = Owner_records.Record.result record in
  match result.phase with
  | Completed | Failed _ | Cancelled | Expired -> Ok result
  | Pending | Interrupted ->
    let open Result.Let_syntax in
    let%bind alive =
      Owner_records.is_live t.records result.flow |> Result.map_error ~f:owner_error
    in
    if alive
    then Ok result
    else (
      match Hashtbl.find t.unpublished_terminal (flow_key result.flow) with
      | Some (original, phase)
        when M.Id.equal
               (Owner_records.Record.operation original)
               (Owner_records.Record.operation record)
             && P.Id.Principal.equal
                  (Owner_records.Record.owner original)
                  (Owner_records.Record.owner record) -> Ok { result with phase }
      | Some _ -> Error DTO.Error.Store_unavailable
      | None ->
        let%bind receipt =
          C.reconcile_operation
            t.registry
            ~binding:(Owner_records.Record.binding record)
            ~operation:(Owner_records.Record.operation record)
          |> Result.map_error ~f:registry_error
        in
        (match receipt with
         | M.Operation.Committed ->
           let%bind template =
             Profile_admin.find_template t.profiles result.flow.profile
             |> Result.map_error ~f:profile_error
           in
           let%map () =
             publish_original
               t
               ~template
               ~operation:(Owner_records.Record.operation record)
           in
           { result with phase = Completed }
         | Pending | Rejected | Unavailable -> Ok { result with phase = Interrupted }))
;;

let status t ~actor (request : DTO.Status_request.t) =
  let open Result.Let_syntax in
  let%bind () =
    if t.closed
    then Error DTO.Error.Closed
    else if can_operate actor Status
    then Ok ()
    else Error DTO.Error.Denied
  in
  let%bind () = if t.closed then Error DTO.Error.Closed else Ok () in
  let%bind templates =
    match request.profile with
    | None ->
      Ok
        (List.filter (Profile_admin.templates t.profiles) ~f:(fun template ->
           authorized
             t
             actor
             ~operation:Status
             ~profile:(Profile_admin.Template.profile template)))
    | Some profile ->
      let%bind () = check t actor Status profile in
      Profile_admin.find_template t.profiles profile
      |> Result.map_error ~f:profile_error
      |> Result.map ~f:List.return
  in
  let%bind profiles =
    List.map templates ~f:(fun template ->
      let profile = Profile_admin.Template.profile template in
      let%bind status =
        match
          Bridge.status
            t.bridge
            ~principal:(P.Id.Principal.to_string (Actor.principal actor).id)
            ~profile:(DTO.Profile_id.to_string profile)
        with
        | Ok status -> Ok (Some status)
        | Error Bridge.Error.Missing_profile -> Ok None
        | Error error -> Error (bridge_error error)
      in
      let%bind snapshot =
        match snapshot_for t (Profile_admin.Template.binding template) with
        | Ok snapshot -> Ok (Some snapshot)
        | Error Missing_profile -> Ok None
        | Error error -> Error error
      in
      let availability =
        match Option.map status ~f:Bridge.Status.availability with
        | None -> DTO.Status_result.Missing
        | Some Configured -> DTO.Status_result.Configured
        | Some Disabled -> Disabled
        | Some (Unavailable Missing) -> Missing
        | Some (Unavailable Available) -> Configured
        | Some (Unavailable Disabled) -> Disabled
        | Some (Unavailable Renewal_required) -> Renewal_required
        | Some (Unavailable Renewal_uncertain) -> Renewal_uncertain
        | Some (Unavailable Secret_unavailable) -> Secret_unavailable
        | Some (Unavailable Store_unavailable) -> Store_unavailable
      in
      let%map credential_revision =
        match Option.bind snapshot ~f:C.Host_snapshot.credential_revision with
        | None -> Ok None
        | Some revision ->
          DTO.Revision.of_string revision
          |> Result.map_error ~f:(fun _ -> DTO.Error.Store_unavailable)
          |> Result.map ~f:Option.some
      in
      { DTO.Status_result.profile
      ; account =
          Option.bind snapshot ~f:(fun snapshot ->
            Option.bind (C.Host_snapshot.identity snapshot) ~f:M.Identity.account)
      ; availability
      ; last_failure = None
      ; auth_epoch = Option.map snapshot ~f:C.Host_snapshot.epoch
      ; credential_revision
      })
    |> Result.all
  in
  let%bind records = Owner_records.list t.records |> Result.map_error ~f:owner_error in
  let%bind flows =
    List.filter_map records ~f:(fun record ->
      let result = Owner_records.Record.result record in
      if
        P.Id.Principal.equal
          (Owner_records.Record.owner record)
          (Actor.principal actor).id
        && authorized t actor ~operation:Status ~profile:result.flow.profile
        &&
        match request.profile with
        | None -> true
        | Some profile -> DTO.Profile_id.equal profile result.flow.profile
      then Some record
      else None)
    |> List.map ~f:(project_flow t)
    |> Result.all
  in
  let%map selection =
    Profile_admin.selection t.profiles |> Result.map_error ~f:profile_error
  in
  { DTO.Status_result.server_id = t.server_id
  ; setup_required = false
  ; profiles
  ; flows
  ; selection =
      (if authorized t actor ~operation:Status ~profile:selection.profile
       then Some selection
       else None)
  }
;;

let select_impl t ~actor ~operation ~reconcile (request : DTO.Select_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Select request.profile in
  Profile_admin.select
    ~authorize_commit:(fun () ->
      (not t.closed) && authorized t actor ~operation:Select ~profile:request.profile)
    t.profiles
    ~principal:(Actor.principal actor).id
    ~operation
    ~reconcile
    request
  |> Result.map_error ~f:profile_error
;;

let configure_environment_impl t ~actor ~operation (request : DTO.Environment_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Configure_environment request.profile in
  let%bind source =
    List.find t.environment ~f:(fun source ->
      DTO.Source_id.equal source.Environment_source.id request.source)
    |> Result.of_option ~error:DTO.Error.Invalid_request
  in
  let%bind () =
    Bridge.configure_environment
      ~authorize_commit:(fun () ->
        (not t.closed)
        && authorized t actor ~operation:Configure_environment ~profile:request.profile)
      t.bridge
      ~principal:(P.Id.Principal.to_string (Actor.principal actor).id)
      ~profile:(DTO.Profile_id.to_string request.profile)
      ~operation
      ~name:source.name
      ~configuration_revision:source.revision
    |> Result.map_error ~f:bridge_error
  in
  configuration_result t request.profile operation
;;

let enroll_private_key_impl t ~actor ~profile ~operation ~sw ~read =
  let open Result.Let_syntax in
  let%bind () = check t actor Configure_environment profile in
  let%bind () =
    Bridge.enroll
      ~authorize_commit:(fun () ->
        (not t.closed) && authorized t actor ~operation:Configure_environment ~profile)
      t.bridge
      ~principal:(P.Id.Principal.to_string (Actor.principal actor).id)
      ~profile:(DTO.Profile_id.to_string profile)
      ~operation
      ~sw
      ~read
    |> Result.map_error ~f:bridge_error
  in
  configuration_result t profile operation
;;

let logout_impl t ~actor ~sw ~operation ~mode (request : DTO.Logout_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Logout request.profile in
  let%bind template =
    Profile_admin.find_template t.profiles request.profile
    |> Result.map_error ~f:profile_error
  in
  let binding = Profile_admin.Template.binding template in
  (* disable publishes its tombstone before any wait. No metadata lock is held
     while local workers join or foreign admission drain remains pending. *)
  let%bind removal =
    C.disable_with_operation
      ~authorize_disable:(fun () ->
        (not t.closed) && authorized t actor ~operation:Logout ~profile:request.profile)
      t.registry
      ~sw
      ~clock:t.clock
      ~maximum_wait:t.maximum_wait
      ~binding
      ~operation
      ~mode
      ~revocation:None
      ~reason:Logout
    |> Result.map_error ~f:registry_error
  in
  let local =
    Hashtbl.data t.live
    |> List.filter ~f:(fun live ->
      M.Id.equal (Owner_records.Record.binding live.record) binding)
  in
  List.iter local ~f:stop_live;
  List.iter local ~f:join_live;
  let%bind records = Owner_records.list t.records |> Result.map_error ~f:owner_error in
  let%bind foreign_live =
    List.filter records ~f:(fun record ->
      M.Id.equal (Owner_records.Record.binding record) binding)
    |> List.map ~f:(fun record ->
      Owner_records.is_live t.records (Owner_records.Record.result record).flow
      |> Result.map_error ~f:owner_error)
    |> Result.all
    |> Result.map ~f:(List.exists ~f:Fn.id)
  in
  let%bind () = Bridge.synchronize t.bridge |> Result.map_error ~f:bridge_error in
  let%map snapshot = snapshot_for t binding in
  let drain =
    match removal.cleanup.drain with
    | C.Status.Drained when not foreign_live -> DTO.Logout_result.Drained
    | _ -> Pending
  in
  let cleanup_pending =
    foreign_live
    ||
    match removal.cleanup.secrets with
    | Clean -> false
    | _ -> true
  in
  { DTO.Logout_result.profile = request.profile
  ; auth_epoch = C.Host_snapshot.epoch snapshot
  ; drain
  ; cleanup_pending
  }
;;

let replay_committed admission extract =
  match admission with
  | Command_intents.Fresh _ -> Ok None
  | Existing intent ->
    (match Command_intents.Intent.committed intent with
     | None -> Ok None
     | Some result ->
       extract result
       |> Result.of_option ~error:DTO.Error.Store_unavailable
       |> Result.map ~f:Option.some)
;;

let cancel t ~actor (request : DTO.Cancel_request.t) =
  let open Result.Let_syntax in
  let%bind _ = find_owned t actor Cancel request.flow in
  let%bind admission =
    admit_intent
      t
      actor
      request.idempotency_key
      "provider.login.cancel"
      (DTO.Cancel_request.to_json request)
  in
  let%bind replay =
    replay_committed admission (function
      | P.Command_receipt.Provider_cancel result -> Some result
      | _ -> None)
  in
  match replay with
  | Some result -> Ok result
  | None ->
    let%bind result = cancel_impl t ~actor request in
    finish_intent
      t
      (original_intent admission)
      (P.Command_receipt.Provider_cancel result)
      result
;;

let select t ~actor (request : DTO.Select_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Select request.profile in
  let%bind admission =
    admit_intent
      t
      actor
      request.idempotency_key
      "provider.select"
      (DTO.Select_request.to_json request)
  in
  let%bind replay =
    replay_committed admission (function
      | P.Command_receipt.Provider_selection result -> Some result
      | _ -> None)
  in
  match replay with
  | Some result -> Ok result
  | None ->
    let intent = original_intent admission in
    let%bind result =
      select_impl
        t
        ~actor
        ~operation:(Command_intents.Intent.operation intent)
        ~reconcile:
          (match admission with
           | Fresh _ -> false
           | Existing _ -> true)
        request
    in
    finish_intent t intent (P.Command_receipt.Provider_selection result) result
;;

let configure_environment t ~actor (request : DTO.Environment_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Configure_environment request.profile in
  let%bind admission =
    admit_intent
      t
      actor
      request.idempotency_key
      "provider.configure_environment"
      (DTO.Environment_request.to_json request)
  in
  let%bind replay =
    replay_committed admission (function
      | P.Command_receipt.Provider_configuration result -> Some result
      | _ -> None)
  in
  match replay with
  | Some result -> Ok result
  | None ->
    let%bind () =
      match admission with
      | Fresh _ -> Ok ()
      | Existing _ -> Error DTO.Error.Submission_uncertain
    in
    let intent = original_intent admission in
    let%bind result =
      configure_environment_impl
        t
        ~actor
        ~operation:(Command_intents.Intent.operation intent)
        request
    in
    finish_intent t intent (P.Command_receipt.Provider_configuration result) result
;;

let logout t ~actor ~sw (request : DTO.Logout_request.t) =
  let open Result.Let_syntax in
  let%bind () = check t actor Logout request.profile in
  let%bind admission =
    admit_intent
      t
      actor
      request.idempotency_key
      "provider.logout"
      (DTO.Logout_request.to_json request)
  in
  let%bind replay =
    replay_committed admission (function
      | P.Command_receipt.Provider_logout result -> Some result
      | _ -> None)
  in
  match replay with
  | Some result -> Ok result
  | None ->
    let intent = original_intent admission in
    let%bind result =
      logout_impl
        t
        ~actor
        ~sw
        ~operation:(Command_intents.Intent.operation intent)
        ~mode:
          (match admission with
           | Fresh _ -> C.Fresh
           | Existing _ -> Reconcile)
        request
    in
    finish_intent t intent (P.Command_receipt.Provider_logout result) result
;;

let enroll_private_key t ~actor ~profile ~key ~source_reference ~sw ~read =
  let open Result.Let_syntax in
  let%bind () = check t actor Configure_environment profile in
  let%bind () =
    if
      String.is_empty source_reference
      || String.length source_reference > 1024
      || not (Stdlib.String.is_valid_utf_8 source_reference)
    then Error DTO.Error.Invalid_request
    else Ok ()
  in
  let params =
    `Object
      [ "profile", DTO.Profile_id.to_json profile
      ; "source_reference", `String source_reference
      ; "idempotency_key", P.Idempotency_key.to_json key
      ]
  in
  let%bind admission = admit_intent t actor key "provider.configure_private_key" params in
  let%bind replay =
    replay_committed admission (function
      | P.Command_receipt.Provider_configuration result -> Some result
      | _ -> None)
  in
  match replay with
  | Some result -> Ok result
  | None ->
    let%bind () =
      match admission with
      | Fresh _ -> Ok ()
      | Existing _ -> Error DTO.Error.Submission_uncertain
    in
    let intent = original_intent admission in
    let%bind result =
      enroll_private_key_impl
        t
        ~actor
        ~profile
        ~operation:(Command_intents.Intent.operation intent)
        ~sw
        ~read
    in
    finish_intent t intent (P.Command_receipt.Provider_configuration result) result
;;

let command_receipt t ~actor command =
  let open Result.Let_syntax in
  let%bind identity =
    match command with
    | P.Command.Provider_login_begin request ->
      Ok (DTO.Operation.Login, request.profile, request.idempotency_key)
    | Provider_login_cancel request ->
      Ok (Cancel, request.flow.profile, request.idempotency_key)
    | Provider_logout request -> Ok (Logout, request.profile, request.idempotency_key)
    | Provider_select request -> Ok (Select, request.profile, request.idempotency_key)
    | Provider_configure_environment request ->
      Ok (Configure_environment, request.profile, request.idempotency_key)
    | _ -> Error DTO.Error.Unsupported
  in
  let operation, profile, key = identity in
  let%bind () = check t actor operation profile in
  let%bind intent =
    Command_intents.lookup
      t.intents
      ~principal:(Actor.principal actor).id
      ~key
      ~method_name:(P.Command.method_name command)
      ~params:(P.Command.params command)
    |> Result.map_error ~f:intent_error
  in
  match intent with
  | None -> Ok P.Command_receipt.Missing
  | Some intent ->
    (match Command_intents.Intent.committed intent with
     | Some result -> Ok (P.Command_receipt.Committed result)
     | None ->
       (match command with
        | Provider_login_begin _ ->
          let%bind flow = login_receipt t ~actor ~key in
          (match flow with
           | None -> Ok P.Command_receipt.Unavailable
           | Some flow ->
             let result = P.Command_receipt.Provider_login flow.flow in
             Ok (P.Command_receipt.Committed result))
        | _ -> Ok P.Command_receipt.Unavailable))
;;
