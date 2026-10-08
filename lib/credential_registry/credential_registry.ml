open! Core
module M = Credential_registry_model
module Storage = Private_storage
module Secrets = Provider_secret_store

module Error = struct
  type t =
    | Model of M.Error.t
    | Storage of Storage.Error.code
    | Secret_store of Secrets.Error.code
    | Missing_secret
    | Binding_unavailable
    | Authorization_denied
    | Busy
    | Timed_out
    | Closed
    | Publication_uncertain
    | Renewal_rejected
    | Revision_quarantined
  [@@deriving sexp_of]
end

module Status = struct
  type availability =
    | Available
    | Missing
    | Disabled
    | Renewal_required
    | Renewal_uncertain
    | Secret_unavailable
    | Store_unavailable
  [@@deriving equal, sexp_of]

  type drain =
    | Drained
    | Drain_pending
  [@@deriving equal, sexp_of]

  type secret_cleanup =
    | Clean
    | Cleanup_pending
    | Cleanup_durability_unknown
    | Cleanup_quarantined
  [@@deriving equal, sexp_of]

  type revocation =
    | Not_requested
    | Revoked
    | Revocation_pending
    | Revocation_unavailable
  [@@deriving equal, sexp_of]

  type cleanup =
    { drain : drain
    ; secrets : secret_cleanup
    ; revocation : revocation
    }
  [@@deriving sexp_of]

  type candidate =
    | No_candidate
    | Candidate_pending
    | Candidate_cleanup_pending
  [@@deriving equal, sexp_of]

  type t =
    { availability : availability
    ; cleanup : cleanup
    ; candidate : candidate
    ; pending_candidate_operation : M.Id.t option
    }

  let availability t = t.availability
  let cleanup t = t.cleanup
  let candidate t = t.candidate
  let pending_candidate_operation t = t.pending_candidate_operation
end

module Material = struct
  type t =
    | Api_key of Secrets.Secret.t
    | Oauth of
        { access : Secrets.Secret.t
        ; refresh : Secrets.Secret.t M.Presence.t
        ; continuity : Secrets.Secret.t M.Presence.t
        }

  let api_key secret = Api_key secret
  let oauth ~access ~refresh ~continuity = Oauth { access; refresh; continuity }

  let with_oauth t ~f =
    match t with
    | Api_key _ -> Error (Error.Model Invalid_grant)
    | Oauth value ->
      Ok (f ~access:value.access ~refresh:value.refresh ~continuity:value.continuity)
  ;;

  let access = function
    | Api_key secret -> secret
    | Oauth value -> value.access
  ;;

  let header_safe secret =
    Secrets.Secret.with_string secret ~f:(fun value ->
      (not (String.is_empty value))
      && String.for_all value ~f:(fun char ->
        let code = Char.to_int char in
        code >= 33 && code <= 126))
  ;;

  let encode ~incarnation ~binding ~operation t =
    let secret value = Secrets.Secret.with_string value ~f:(fun value -> `String value) in
    let json =
      match t with
      | Api_key access -> `Object [ "kind", `String "api_key"; "access", secret access ]
      | Oauth value ->
        let presence name = function
          | M.Presence.Absent -> []
          | Null -> [ name, `Null ]
          | Value value -> [ name, secret value ]
        in
        `Object
          ([ "kind", `String "oauth"; "access", secret value.access ]
           @ presence "refresh" value.refresh
           @ presence "continuity" value.continuity)
    in
    let json =
      `Object
        [ "version", `Number "1"
        ; "registry", `String (M.Id.to_string incarnation)
        ; "binding", `String (M.Id.to_string binding)
        ; "operation", `String (M.Id.to_string operation)
        ; "material", json
        ]
    in
    Secrets.Secret.of_bytes (Bytes.of_string (Jsonaf.to_string json))
    |> Result.map_error ~f:(fun error -> Error.Secret_store (Secrets.Error.code error))
  ;;

  let decode ~incarnation ~binding ~operation secret =
    let malformed = Error (Error.Secret_store Corrupt) in
    Secrets.Secret.with_string secret ~f:(fun value ->
      match Document_schema.Json.decode ~limits:Document_schema.Limits.default value with
      | Error _ -> malformed
      | Ok (`Object envelope) ->
        let envelope_get name = List.Assoc.find envelope name ~equal:String.equal in
        let matches name id =
          match envelope_get name with
          | Some (`String value) -> String.equal value (M.Id.to_string id)
          | _ -> false
        in
        let valid_owner =
          List.length envelope = 5
          && matches "registry" incarnation
          && matches "binding" binding
          && matches "operation" operation
          &&
          match envelope_get "version" with
          | Some (`Number "1") -> true
          | _ -> false
        in
        let body =
          match envelope_get "material" with
          | Some (`Object fields) when valid_owner -> Some fields
          | _ -> None
        in
        (match body with
         | None -> Error Error.Revision_quarantined
         | Some fields ->
           let get name = List.Assoc.find fields name ~equal:String.equal in
           let decode = function
             | `String value ->
               Secrets.Secret.of_bytes (Bytes.of_string value)
               |> Result.map_error ~f:(fun error ->
                 Error.Secret_store (Secrets.Error.code error))
             | _ -> malformed
           in
           (match get "kind", get "access" with
            | Some (`String kind), Some access ->
              Result.bind (decode access) ~f:(fun access ->
                let presence name =
                  match get name with
                  | None -> Ok M.Presence.Absent
                  | Some `Null -> Ok M.Presence.Null
                  | Some value ->
                    Result.map (decode value) ~f:(fun value -> M.Presence.Value value)
                in
                match kind with
                | "api_key" when List.length fields = 2 -> Ok (Api_key access)
                | "oauth" ->
                  let open Result.Let_syntax in
                  let%bind refresh = presence "refresh" in
                  let%map continuity = presence "continuity" in
                  Oauth { access; refresh; continuity }
                | _ -> malformed)
            | _ -> malformed))
      | Ok _ -> malformed)
  ;;
end

module Verified = struct
  type t =
    { identity : M.Identity.t
    ; grant : M.Grant.t option
    ; material : Material.t
    }

  let create ~identity ~grant ~material =
    let consistent =
      match M.Identity.method_ identity, grant, material with
      | Api_key _, None, Material.Api_key _ -> true
      | Oauth _, Some grant, Oauth value ->
        M.Identity.equal identity (M.Grant.identity grant)
        &&
          (match M.Grant.refresh_policy grant, value.refresh with
          | Require_rotated, Value secret -> Material.header_safe secret
          | Require_rotated, (Absent | Null) -> false
          | Preserve_omitted, Value secret -> Material.header_safe secret
          | Preserve_omitted, (Absent | Null) -> true)
      | _ -> false
    in
    if consistent && Material.header_safe (Material.access material)
    then Ok { identity; grant; material }
    else Error (Error.Model Invalid_grant)
  ;;
end

module Renewal = struct
  type outcome =
    | Verified of Verified.t
    | Definitely_not_submitted
    | Authoritative_rejection
    | Possibly_consumed

  type t =
    { exchange :
        sw:Eio.Switch.t
        -> identity:M.Identity.t
        -> grant:M.Grant.t
        -> Material.t
        -> outcome
    }

  let create ~exchange = { exchange }
end

module Environment = struct
  type resolved =
    { access : Secrets.Secret.t
    ; configuration_revision : M.Id.t option
    ; check_current : unit -> (unit, Error.t) result
    }

  let resolved ~access ~configuration_revision ~check_current =
    { access; configuration_revision; check_current }
  ;;

  type t =
    { resolve :
        sw:Eio.Switch.t
        -> binding:M.Id.t
        -> identity:M.Identity.t
        -> name:string
        -> expected_configuration_revision:M.Id.t option
        -> (resolved, Error.t) result
    ; status : binding:M.Id.t -> name:string -> Status.availability
    }

  let create ~resolve ~status = { resolve; status }
end

type wall_clock = Wall_clock : _ Eio.Time.clock -> wall_clock

module Metadata_admission = struct
  type error = Invalid_wait [@@deriving equal, sexp_of]

  type t =
    | Nonblocking
    | Wait of
        { clock : Eio.Time.Mono.ty Eio.Time.Mono.t
        ; maximum_wait : Time_ns.Span.t
        }

  let nonblocking = Nonblocking

  let wait ~clock ~maximum_wait =
    if Time_ns.Span.(maximum_wait <= zero || maximum_wait > of_sec 60.)
    then Error Invalid_wait
    else Ok (Wait { clock :> Eio.Time.Mono.ty Eio.Time.Mono.t; maximum_wait })
  ;;
end

type t =
  { directory : Storage.Directory.t
  ; secrets : Secrets.t
  ; environment : Environment.t option
  ; host : M.Id.t
  ; wall_clock : wall_clock
  ; metadata_admission : Metadata_admission.t
  ; new_operation : unit -> M.Id.t
  ; mutable cached : M.t
  ; mutable closed : bool
  ; mutable outstanding : unit Eio.Promise.t list
  ; mutable after_publication : (unit -> unit) option
  }

let close t =
  t.closed <- true;
  Eio.Cancel.protect (fun () -> List.iter t.outstanding ~f:Eio.Promise.await)
;;

let with_call t f =
  if t.closed
  then Error Error.Closed
  else if List.length t.outstanding >= 256
  then Error Error.Busy
  else (
    let promise, resolver = Eio.Promise.create () in
    t.outstanding <- promise :: t.outstanding;
    Exn.protect ~f ~finally:(fun () ->
      t.outstanding
      <- List.filter t.outstanding ~f:(fun pending -> not (phys_equal pending promise));
      Eio.Promise.resolve resolver ()))
;;

type candidate =
  { owner : t
  ; binding : M.Id.t
  ; expected : M.Expected.t
  }

module Candidate = struct
  type t = candidate
end

let storage_error error =
  match Storage.Error.code error with
  | Busy -> Error.Busy
  | Closed -> Closed
  | code -> Storage code
;;

let model result = Result.map_error result ~f:(fun error -> Error.Model error)
let storage result = Result.map_error result ~f:storage_error

let secret result =
  Result.map_error result ~f:(fun error ->
    match Secrets.Error.code error with
    | Missing -> Error.Missing_secret
    | code -> Secret_store code)
;;

let name value = Storage.Name.create value |> storage

let metadata_name =
  name "provider-registry.json"
  |> function
  | Ok name -> name
  | Error _ -> failwith "invalid fixed registry metadata name"
;;

let metadata_lock =
  name "provider-registry-M.lock"
  |> function
  | Ok name -> name
  | Error _ -> failwith "invalid fixed registry lock name"
;;

let metadata_bytes = 1024 * 1024

let load directory host =
  let open Result.Let_syntax in
  let%bind bytes =
    Storage.Directory.read_bounded directory metadata_name ~max_bytes:metadata_bytes
    |> storage
  in
  let%bind document =
    Document_schema.Document.decode
      ~limits:Document_schema.Limits.default
      (Bytes.to_string bytes)
    |> Result.map_error ~f:(fun _ -> Error.Model Invalid_document)
  in
  let%bind registry = M.of_document document |> model in
  if M.Id.equal host (M.host registry)
  then Ok registry
  else Error (Error.Model Wrong_incarnation)
;;

let acquire_metadata admission ~directory ~sw ~is_closed =
  let acquire_once () =
    Eio.Switch.check sw;
    if is_closed ()
    then Error Error.Closed
    else Storage.Lock.acquire directory metadata_lock ~sw ~mode:Exclusive |> storage
  in
  match admission with
  | Metadata_admission.Nonblocking -> acquire_once ()
  | Wait { clock; maximum_wait } ->
    let started = Eio.Time.Mono.now clock in
    let remaining () =
      let elapsed =
        Mtime.span started (Eio.Time.Mono.now clock) |> Mtime.Span.to_float_ns
      in
      Time_ns.Span.to_ns maximum_wait -. elapsed
    in
    let rec acquire ~initial =
      Eio.Switch.check sw;
      if is_closed ()
      then Error Error.Closed
      else if (not initial) && Float.(remaining () <= 0.)
      then Error Error.Timed_out
      else (
        match acquire_once () with
        | Error Error.Busy ->
          let remaining = remaining () in
          if Float.(remaining <= 0.)
          then Error Error.Timed_out
          else (
            Eio.Time.Mono.sleep clock (Float.min 0.01 (remaining /. 1e9));
            acquire ~initial:false)
        | result -> result)
    in
    acquire ~initial:true
;;

let with_metadata t f =
  if t.closed
  then Error Error.Closed
  else
    Eio.Switch.run (fun sw ->
      let open Result.Let_syntax in
      let%bind lock =
        acquire_metadata
          t.metadata_admission
          ~directory:t.directory
          ~sw
          ~is_closed:(fun () -> t.closed)
      in
      Exn.protect
        ~finally:(fun () -> Storage.Lock.release lock)
        ~f:(fun () ->
          let%bind current = load t.directory t.host in
          if not (M.Id.equal (M.incarnation current) (M.incarnation t.cached))
          then Error (Error.Model Wrong_incarnation)
          else (
            t.cached <- current;
            f current)))
;;

let publish t registry =
  let open Result.Let_syntax in
  let%bind document = M.to_document registry |> model in
  match
    Storage.Directory.replace_metadata
      t.directory
      metadata_name
      (Bytes.of_string (Document_schema.Document.to_string document))
  with
  | Ok () ->
    Option.iter t.after_publication ~f:(fun hook -> hook ());
    t.cached <- registry;
    Ok ()
  | Error error ->
    (match Storage.Error.publication error with
     | Some Published_durability_unknown ->
       (* Current metadata is reread under the original M lease. No old pointer
          fallback and no inference that cancellation rolled the write back. *)
       (match load t.directory t.host with
        | Ok current -> t.cached <- current
        | Error _ -> ());
       Error Error.Publication_uncertain
     | Some Not_published | None -> Error (storage_error error))
;;

let update t transition =
  with_metadata t (fun current ->
    Result.bind (transition current |> model) ~f:(publish t))
;;

let open_existing
      ~sw:owner_sw
      ~wall_clock
      ~metadata_admission
      ~new_operation
      ~directory
      ~secrets
      ~environment
      ~host
  =
  Eio.Switch.run (fun sw ->
    let open Result.Let_syntax in
    let%bind lock =
      acquire_metadata metadata_admission ~directory ~sw ~is_closed:(fun () -> false)
    in
    Exn.protect
      ~finally:(fun () -> Storage.Lock.release lock)
      ~f:(fun () ->
        let%map cached = load directory host in
        let t =
          { directory
          ; secrets
          ; environment
          ; host
          ; wall_clock = Wall_clock wall_clock
          ; metadata_admission
          ; new_operation
          ; cached
          ; closed = false
          ; after_publication = None
          ; outstanding = []
          }
        in
        Eio.Switch.on_release owner_sw (fun () -> close t);
        t))
;;

let initialize_new
      ~sw
      ~wall_clock
      ~metadata_admission
      ~new_operation
      ~directory
      ~secrets
      ~environment
      ~host
      ~incarnation
  =
  let open Result.Let_syntax in
  let%bind registry = M.initialize ~incarnation ~host |> model in
  let%bind document = M.to_document registry |> model in
  let%bind () =
    Eio.Switch.run (fun lock_sw ->
      let%bind lock =
        acquire_metadata metadata_admission ~directory ~sw:lock_sw ~is_closed:(fun () ->
          false)
      in
      Exn.protect
        ~finally:(fun () -> Storage.Lock.release lock)
        ~f:(fun () ->
          Storage.Directory.create_immutable
            directory
            metadata_name
            (Bytes.of_string (Document_schema.Document.to_string document))
          |> storage))
  in
  open_existing
    ~sw
    ~wall_clock
    ~metadata_admission
    ~new_operation
    ~directory
    ~secrets
    ~environment
    ~host
;;

let revision id = Secrets.Revision.create (M.Id.to_string id) |> secret

let read_material t ~binding id =
  Result.bind (revision id) ~f:(fun revision ->
    Result.bind
      (Secrets.read t.secrets ~revision |> secret)
      ~f:(Material.decode ~incarnation:(M.incarnation t.cached) ~binding ~operation:id))
;;

let current_time_ms t =
  match t.wall_clock with
  | Wall_clock clock -> Int64.of_float (Eio.Time.now clock *. 1000.)
;;

let grant_available t grant =
  let effective = M.Grant.effective grant in
  match effective.expiry with
  | Known value -> Int64.(value.at_ms > current_time_ms t + 60_000L)
  | Unknown ->
    (match effective.unknown_expiry_policy with
     | Allow_qualified_unknown -> true
     | Reject_unknown -> false)
;;

let owner registry binding =
  M.Id.to_string (M.incarnation registry) ^ ":" ^ M.Id.to_string binding
;;

let lock_name binding suffix = name (M.Id.to_string binding ^ "-" ^ suffix ^ ".lock")

let acquire t ~sw ~binding ~suffix ~mode =
  Result.bind (lock_name binding suffix) ~f:(fun name ->
    Storage.Lock.acquire t.directory name ~sw ~mode |> storage)
;;

let rec acquire_until t ~sw ~clock ~started ~maximum_wait ~binding ~suffix ~mode =
  if t.closed
  then Error Error.Closed
  else (
    match acquire t ~sw ~binding ~suffix ~mode with
    | Error Busy ->
      let elapsed =
        Mtime.span started (Eio.Time.Mono.now clock) |> Mtime.Span.to_float_ns
      in
      if Float.(elapsed >= Time_ns.Span.to_ns maximum_wait)
      then Error Error.Timed_out
      else (
        Eio.Time.Mono.sleep clock 0.01;
        acquire_until t ~sw ~clock ~started ~maximum_wait ~binding ~suffix ~mode)
    | result -> result)
;;

let begin_candidate t ~binding ~operation ~expectation =
  with_metadata t (fun registry ->
    let open Result.Let_syntax in
    let%bind registry, expected =
      M.begin_candidate registry ~binding ~operation ~expectation |> model
    in
    let%map () = publish t registry in
    { owner = t; binding; expected })
;;

let candidate_owner t candidate =
  if phys_equal t candidate.owner then Ok () else Error (Error.Model Wrong_incarnation)
;;

let cancel_candidate t candidate =
  Result.bind (candidate_owner t candidate) ~f:(fun () ->
    update t (fun registry ->
      M.discard_candidate registry ~binding:candidate.binding ~expected:candidate.expected))
;;

let stage t ~binding material operation =
  let open Result.Let_syntax in
  let%bind revision = revision operation in
  let%bind payload =
    Material.encode ~incarnation:(M.incarnation t.cached) ~binding ~operation material
  in
  match Secrets.create t.secrets ~revision payload with
  | Ok () -> Ok ()
  | Error error ->
    (match Secrets.Error.code error with
     | Exists -> Error Error.Revision_quarantined
     | code -> Error (Error.Secret_store code))
;;

let quarantine_candidate t candidate =
  let open Result.Let_syntax in
  let%bind () = cancel_candidate t candidate in
  update t (fun registry ->
    M.record_cleanup
      registry
      ~binding:candidate.binding
      ~operation:(M.Expected.operation candidate.expected)
      ~revision:(M.Expected.operation candidate.expected)
      ~deletion:Quarantined)
;;

(* Final authorization admission runs under M after its fresh read. The callback
   must not yield. Once admitted, publication is allowed to finish even if wall
   time advances during filesystem I/O. A denied staged candidate is retired by
   its original CAS, never by deleting or replacing the previous active login. *)
let commit_authorized t candidate ~authorize_commit transition =
  let guard_exception = ref None in
  let result =
    with_metadata t (fun current ->
      let allowed =
        try authorize_commit () with
        | exn ->
          guard_exception := Some (exn, Stdlib.Printexc.get_raw_backtrace ());
          false
      in
      if allowed
      then Result.bind (transition current |> model) ~f:(publish t)
      else Error Error.Authorization_denied)
  in
  match result with
  | Error Authorization_denied ->
    let cleanup =
      try Ok (Eio.Cancel.protect (fun () -> cancel_candidate t candidate)) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    (match !guard_exception, cleanup with
     | Some (exn, backtrace), _ | None, Error (exn, backtrace) ->
       Stdlib.Printexc.raise_with_backtrace exn backtrace
     | None, Ok cleanup ->
       Result.bind cleanup ~f:(fun () -> Error Error.Authorization_denied))
  | result -> result
;;

let commit_candidate
      ?(authorize_commit = fun () -> true)
      t
      candidate
      (verified : Verified.t)
  =
  let open Result.Let_syntax in
  let%bind () = candidate_owner t candidate in
  Eio.Switch.run (fun sw ->
    let%bind lock =
      acquire t ~sw ~binding:candidate.binding ~suffix:"R" ~mode:Exclusive
    in
    Exn.protect
      ~finally:(fun () -> Storage.Lock.release lock)
      ~f:(fun () ->
        let operation = M.Expected.operation candidate.expected in
        let%bind () =
          update t (fun registry ->
            M.stage_candidate
              registry
              ~binding:candidate.binding
              ~expected:candidate.expected
              ~revision:operation)
        in
        match stage t ~binding:candidate.binding verified.material operation with
        | Error Revision_quarantined ->
          let%bind () = quarantine_candidate t candidate in
          Error Error.Revision_quarantined
        | Error _ as error -> error
        | Ok () ->
          commit_authorized t candidate ~authorize_commit (fun registry ->
            M.commit_candidate
              registry
              ~binding:candidate.binding
              ~expected:candidate.expected
              ~identity:verified.identity
              ~source:(Protected_revision operation)
              ~grant:verified.grant)))
;;

let commit_environment_candidate
      ?(authorize_commit = fun () -> true)
      t
      candidate
      ~identity
      ~name
      ~configuration_revision
  =
  let open Result.Let_syntax in
  let%bind () = candidate_owner t candidate in
  match t.environment, M.Identity.method_ identity with
  | None, _ -> Error Error.Binding_unavailable
  | Some _, M.Identity.Oauth _ -> Error (Error.Model Invalid_identity)
  | Some _, Api_key _ ->
    commit_authorized t candidate ~authorize_commit (fun registry ->
      M.commit_candidate
        registry
        ~binding:candidate.binding
        ~expected:candidate.expected
        ~identity
        ~source:(Environment_reference { name; configuration_revision })
        ~grant:None)
;;

let refresh_material ~identity ~old (verified : Verified.t) =
  if not (M.Identity.equal identity verified.identity)
  then Error (Error.Model Invalid_identity)
  else (
    match old, verified.material, verified.grant with
    | Material.Oauth previous, Oauth next, Some grant ->
      let continuity_valid =
        match previous.continuity, next.continuity with
        | Value _, Value _ | Null, (Null | Value _) | Absent, (Absent | Null | Value _) ->
          true
        | Value _, (Absent | Null) | Null, Absent -> false
      in
      if not continuity_valid
      then Error (Error.Model Invalid_grant)
      else (
        match M.Grant.refresh_policy grant, next.refresh with
        | Preserve_omitted, Absent ->
          Ok Material.(Oauth { next with refresh = previous.refresh })
        | Require_rotated, (Absent | Null) -> Error (Error.Model Invalid_grant)
        | _ -> Ok verified.material)
    | _ -> Error (Error.Model Invalid_grant))
;;

let refresh_owned t ~sw ~binding (renewal : Renewal.t) =
  let open Result.Let_syntax in
  let%bind snapshot =
    with_metadata t (fun registry -> M.find registry ~binding |> model)
  in
  let%bind active =
    Result.of_option (M.Snapshot.active snapshot) ~error:(Error.Model Not_active)
  in
  let%bind grant =
    Result.of_option (M.Active.grant active) ~error:(Error.Model Invalid_grant)
  in
  let%bind previous_revision =
    match M.Active.source active with
    | Protected_revision revision -> Ok revision
    | Environment_reference _ -> Error (Error.Model Invalid_grant)
  in
  let%bind previous = read_material t ~binding previous_revision in
  let%bind () =
    match previous with
    | Material.Oauth { refresh = Value token; _ } when Material.header_safe token -> Ok ()
    | _ -> Error Error.Renewal_rejected
  in
  let operation = t.new_operation () in
  let%bind expected =
    with_metadata t (fun registry ->
      let%bind registry, expected =
        M.begin_refresh registry ~binding ~operation |> model
      in
      let%map () = publish t registry in
      expected)
  in
  let outcome =
    renewal.exchange ~sw ~identity:(M.Active.identity active) ~grant previous
  in
  match outcome with
  | Definitely_not_submitted ->
    update t (fun registry ->
      M.clear_definitely_unsent_refresh registry ~binding ~expected)
  | Authoritative_rejection ->
    let%bind () =
      update t (fun registry ->
        M.disable
          registry
          ~binding
          ~operation:(t.new_operation ())
          ~reason:Renewal_rejected)
    in
    Error Error.Renewal_rejected
  | Possibly_consumed ->
    let%bind () =
      update t (fun registry -> M.mark_renewal_uncertain registry ~binding ~expected)
    in
    Error (Error.Model Renewal_uncertain)
  | Verified verified ->
    let%bind material =
      refresh_material ~identity:(M.Active.identity active) ~old:previous verified
    in
    let%bind grant = Result.of_option verified.grant ~error:(Error.Model Invalid_grant) in
    let%bind () =
      update t (fun registry ->
        M.stage_refresh registry ~binding ~expected ~revision:operation)
    in
    (match stage t ~binding material operation with
     | Error Revision_quarantined ->
       let%bind () =
         update t (fun registry -> M.quarantine_refresh registry ~binding ~expected)
       in
       Error Error.Revision_quarantined
     | Error _ as error -> error
     | Ok () ->
       update t (fun registry ->
         M.commit_refresh registry ~binding ~expected ~revision:operation ~grant))
;;

let refresh t ~sw:_ ~clock ~maximum_wait ~binding ~renewal =
  Eio.Switch.run (fun sw ->
    let open Result.Let_syntax in
    let%bind lock =
      acquire_until
        t
        ~sw
        ~clock
        ~started:(Eio.Time.Mono.now clock)
        ~maximum_wait
        ~binding
        ~suffix:"R"
        ~mode:Exclusive
    in
    Exn.protect
      ~finally:(fun () -> Storage.Lock.release lock)
      ~f:(fun () ->
        let%bind snapshot =
          with_metadata t (fun registry -> M.find registry ~binding |> model)
        in
        let%bind () =
          match M.Snapshot.refresh snapshot with
          | Idle -> Ok ()
          | Possibly_sent _ | Renewal_uncertain _ -> Error (Error.Model Renewal_uncertain)
        in
        match M.Snapshot.active snapshot with
        | Some active when Option.exists (M.Active.grant active) ~f:(grant_available t) ->
          Ok ()
        | Some _ -> refresh_owned t ~sw ~binding renewal
        | None -> Error (Error.Model Not_active)))
;;

module Admission = struct
  type t =
    { owner : string
    ; identity : M.Identity.t
    ; epoch : int64
    ; credential_revision : string option
    ; access : Secrets.Secret.t
    ; check_current : unit -> (unit, Error.t) result
    }

  let owner t = t.owner
  let identity t = t.identity
  let epoch t = t.epoch
  let credential_revision t = t.credential_revision
  let with_access t ~f = f t.access
  let check_current t = t.check_current ()
end

let same_source (source : M.Active.source) (expected : M.Active.source) =
  match source, expected with
  | M.Active.Protected_revision left, Protected_revision right -> M.Id.equal left right
  | Environment_reference left, Environment_reference right ->
    String.equal left.name right.name
    && Option.equal M.Id.equal left.configuration_revision right.configuration_revision
  | _ -> false
;;

let check_current t ~binding ~expected_owner ~expected_epoch ~source () =
  if t.closed
  then Error Error.Closed
  else if not (String.equal (owner t.cached binding) expected_owner)
  then Error (Error.Model Wrong_incarnation)
  else (
    match M.find t.cached ~binding with
    | Error error -> Error (Error.Model error)
    | Ok snapshot ->
      if Option.is_some (M.Snapshot.disabled snapshot)
      then Error (Error.Model Disabled)
      else if
        not (Int64.equal (M.Epoch.to_int64 (M.Snapshot.epoch snapshot)) expected_epoch)
      then Error (Error.Model Stale_epoch)
      else if
        match M.Snapshot.refresh snapshot with
        | Idle -> false
        | _ -> true
      then Error (Error.Model Renewal_uncertain)
      else (
        match M.Snapshot.active snapshot with
        | Some active when same_source (M.Active.source active) source ->
          if
            Option.exists (M.Active.grant active) ~f:(fun grant ->
              not (grant_available t grant))
          then Error Error.Renewal_rejected
          else Ok ()
        | Some _ -> Error (Error.Model Stale_revision)
        | None -> Error (Error.Model Not_active)))
;;

let ensure_current t ~binding ~expected_owner ~expected_epoch ~source =
  with_metadata t (fun _ ->
    check_current t ~binding ~expected_owner ~expected_epoch ~source ())
;;

let admit t ~sw ~clock ~maximum_wait ~binding ~expected_owner ~expected_epoch ~renewal =
  let open Result.Let_syntax in
  let%bind fence =
    acquire_until
      t
      ~sw
      ~clock
      ~started:(Eio.Time.Mono.now clock)
      ~maximum_wait
      ~binding
      ~suffix:"G"
      ~mode:Shared
  in
  let keep_fence = ref false in
  Exn.protect
    ~finally:(fun () ->
      if not !keep_fence then Eio.Cancel.protect (fun () -> Storage.Lock.release fence))
    ~f:(fun () ->
      let%bind snapshot =
        with_metadata t (fun registry -> M.find registry ~binding |> model)
      in
      let%bind active =
        Result.of_option (M.Snapshot.active snapshot) ~error:(Error.Model Not_active)
      in
      let%bind () =
        if Option.is_some (M.Snapshot.disabled snapshot)
        then Error (Error.Model Disabled)
        else if not (String.equal expected_owner (owner t.cached binding))
        then Error (Error.Model Wrong_incarnation)
        else if
          not (Int64.equal expected_epoch (M.Epoch.to_int64 (M.Snapshot.epoch snapshot)))
        then Error (Error.Model Stale_epoch)
        else (
          match M.Snapshot.refresh snapshot with
          | Idle -> Ok ()
          | Possibly_sent _ | Renewal_uncertain _ -> Error (Error.Model Renewal_uncertain))
      in
      let%bind () =
        match M.Active.grant active with
        | None -> Ok ()
        | Some grant when grant_available t grant -> Ok ()
        | Some _ ->
          (match renewal with
           | None -> Error Error.Renewal_rejected
           | Some renewal -> refresh t ~sw ~clock ~maximum_wait ~binding ~renewal)
      in
      let%bind active =
        with_metadata t (fun registry ->
          let%bind snapshot = M.find registry ~binding |> model in
          Result.of_option (M.Snapshot.active snapshot) ~error:(Error.Model Not_active))
      in
      let%bind () =
        match M.Active.grant active with
        | None -> Ok ()
        | Some grant when grant_available t grant -> Ok ()
        | Some _ -> Error Error.Renewal_rejected
      in
      let source = M.Active.source active in
      let%bind access, credential_revision, config_guard =
        match source with
        | Protected_revision revision ->
          let%map material = read_material t ~binding revision in
          Material.access material, Some (M.Id.to_string revision), fun () -> Ok ()
        | Environment_reference { name; configuration_revision } ->
          (match t.environment with
           | None -> Error Error.Binding_unavailable
           | Some environment ->
             let%bind resolved =
               environment.resolve
                 ~sw
                 ~binding
                 ~identity:(M.Active.identity active)
                 ~name
                 ~expected_configuration_revision:configuration_revision
             in
             if
               (not
                  (Option.equal
                     M.Id.equal
                     resolved.configuration_revision
                     configuration_revision))
               || not (Material.header_safe resolved.access)
             then Error (Error.Model Stale_revision)
             else (
               let%map () = resolved.check_current () in
               ( resolved.access
               , Option.map resolved.configuration_revision ~f:M.Id.to_string
               , resolved.check_current )))
      in
      let%bind () = ensure_current t ~binding ~expected_owner ~expected_epoch ~source in
      let guard () =
        let%bind () =
          check_current t ~binding ~expected_owner ~expected_epoch ~source ()
        in
        config_guard ()
      in
      keep_fence := true;
      Ok
        { Admission.owner = expected_owner
        ; identity = M.Active.identity active
        ; epoch = expected_epoch
        ; credential_revision
        ; access
        ; check_current = guard
        })
;;

let status_of_registry t registry ~binding =
  let open Result.Let_syntax in
  let%bind snapshot = M.find registry ~binding |> model in
  let%bind retired = M.retired registry ~binding |> model in
  let%bind removal = M.removal registry ~binding |> model in
  let%bind candidate_pending = M.candidate_pending registry ~binding |> model in
  let%bind pending_candidate_operation =
    M.pending_candidate_operation registry ~binding |> model
  in
  let secrets =
    if
      List.exists retired ~f:(fun item ->
        match M.Cleanup.deletion item with
        | Quarantined -> true
        | _ -> false)
    then Status.Cleanup_quarantined
    else if
      List.exists retired ~f:(fun item ->
        match M.Cleanup.deletion item with
        | Removed_durability_unknown -> true
        | _ -> false)
    then Status.Cleanup_durability_unknown
    else if List.is_empty retired
    then Status.Clean
    else Status.Cleanup_pending
  in
  let drain, revocation =
    match removal with
    | None -> Status.Drained, Status.Not_requested
    | Some removal ->
      ( (match M.Removal.drain removal with
         | Pending -> Status.Drain_pending
         | Drained -> Status.Drained)
      , (match M.Removal.revocation removal with
         | Not_requested -> Status.Not_requested
         | Unavailable -> Status.Revocation_unavailable
         | Possibly_sent -> Status.Revocation_pending
         | Revoked -> Status.Revoked) )
  in
  let%bind history = M.revocation_history registry ~binding |> model in
  let revocation =
    if List.is_empty history then revocation else Status.Revocation_pending
  in
  let availability =
    if Option.is_some (M.Snapshot.disabled snapshot)
    then Status.Disabled
    else (
      match M.Snapshot.refresh snapshot with
      | Possibly_sent _ | Renewal_uncertain _ -> Status.Renewal_uncertain
      | Idle ->
        (match M.Snapshot.active snapshot with
         | None -> Status.Missing
         | Some active ->
           (match M.Active.grant active with
            | Some grant when not (grant_available t grant) -> Status.Renewal_required
            | _ ->
              (match M.Active.source active with
               | Protected_revision revision ->
                 (match read_material t ~binding revision with
                  | Ok _ -> Status.Available
                  | Error _ -> Status.Secret_unavailable)
               | Environment_reference { name; configuration_revision = _ } ->
                 (match t.environment with
                  | None -> Status.Secret_unavailable
                  | Some environment -> environment.status ~binding ~name)))))
  in
  Ok
    { Status.availability
    ; pending_candidate_operation
    ; cleanup = { drain; secrets; revocation }
    ; candidate =
        (if candidate_pending
         then Candidate_pending
         else if List.is_empty retired
         then No_candidate
         else Candidate_cleanup_pending)
    }
;;

let status t ~binding =
  with_metadata t (fun registry -> status_of_registry t registry ~binding)
;;

let reconcile_operation t ~binding ~operation =
  with_metadata t (fun registry ->
    match M.operation registry ~binding ~operation with
    | Ok receipt -> Ok (M.Operation.result receipt)
    | Error Stale_operation -> Ok M.Operation.Unavailable
    | Error error -> Error (Error.Model error))
;;

module Host_snapshot = struct
  type availability =
    | Ready
    | Missing
    | Disabled
    | Renewal_required
    | Renewal_uncertain
  [@@deriving equal, sexp_of]

  type binding =
    { id : M.Id.t
    ; owner : string
    ; epoch : int64
    ; credential_revision : string option
    ; identity : M.Identity.t option
    ; source : M.Active.source option
    ; availability : availability
    ; pending_candidate_operation : M.Id.t option
    }

  type t = binding list

  let bindings t = t
  let id t = t.id
  let owner t = t.owner
  let epoch t = t.epoch
  let credential_revision t = t.credential_revision
  let identity t = t.identity
  let source t = t.source
  let availability t = t.availability
  let pending_candidate_operation t = t.pending_candidate_operation
end

let synchronize t =
  with_metadata t (fun registry ->
    let open Result.Let_syntax in
    List.fold (M.binding_ids registry) ~init:(Ok []) ~f:(fun result binding ->
      let%bind bindings = result in
      let%bind snapshot = M.find registry ~binding |> model in
      let%map pending_candidate_operation =
        M.pending_candidate_operation registry ~binding |> model
      in
      let availability =
        if Option.is_some (M.Snapshot.disabled snapshot)
        then Host_snapshot.Disabled
        else (
          match M.Snapshot.refresh snapshot with
          | Possibly_sent _ | Renewal_uncertain _ -> Host_snapshot.Renewal_uncertain
          | Idle ->
            (match M.Snapshot.active snapshot with
             | None -> Host_snapshot.Missing
             | Some active ->
               if
                 Option.exists (M.Active.grant active) ~f:(fun grant ->
                   not (grant_available t grant))
               then Host_snapshot.Renewal_required
               else Host_snapshot.Ready))
      in
      let active = M.Snapshot.active snapshot in
      let credential_revision =
        Option.bind active ~f:(fun active ->
          match M.Active.source active with
          | Protected_revision revision -> Some (M.Id.to_string revision)
          | Environment_reference { configuration_revision; _ } ->
            Option.map configuration_revision ~f:M.Id.to_string)
      in
      { Host_snapshot.id = binding
      ; owner = owner registry binding
      ; epoch = M.Epoch.to_int64 (M.Snapshot.epoch snapshot)
      ; credential_revision
      ; identity = Option.map active ~f:M.Active.identity
      ; source = Option.map active ~f:M.Active.source
      ; pending_candidate_operation
      ; availability
      }
      :: bindings)
    |> Result.map ~f:List.rev)
;;

let cleanup_owned t ~binding =
  let open Result.Let_syntax in
  let%bind retired =
    with_metadata t (fun registry -> M.retired registry ~binding |> model)
  in
  List.fold retired ~init:(Ok ()) ~f:(fun result item ->
    let%bind () = result in
    match M.Cleanup.deletion item with
    | Quarantined -> Ok ()
    | Confirmed_removed -> Ok ()
    | Pending | Removed_durability_unknown ->
      with_metadata t (fun registry ->
        let operation = M.Cleanup.owned_by item in
        let id = M.Cleanup.revision item in
        (* The pure check rejects any globally active/staged reference. G/R and M
           stay held through this bounded native cleanup, never external I/O. *)
        let%bind _ =
          M.record_cleanup
            registry
            ~binding
            ~operation
            ~revision:id
            ~deletion:(M.Cleanup.deletion item)
          |> model
        in
        let record deletion =
          let%bind next =
            M.record_cleanup registry ~binding ~operation ~revision:id ~deletion |> model
          in
          publish t next
        in
        let confirm_removed () =
          let%bind revision = revision id in
          let%bind () = Secrets.confirm_absent t.secrets ~revision |> secret in
          record Confirmed_removed
        in
        match read_material t ~binding id with
        | Error Missing_secret -> confirm_removed ()
        | Error Revision_quarantined -> record Quarantined
        | Error (Secret_store Corrupt) -> record Quarantined
        | Error error -> Error error
        | Ok _ ->
          let%bind revision = revision id in
          (match Secrets.delete t.secrets ~revision with
           | Ok () -> record Confirmed_removed
           | Error error ->
             (match Secrets.Error.publication error with
              | Some Published_durability_unknown -> record Removed_durability_unknown
              | Some Not_published | None ->
                (match Secrets.Error.code error with
                 | Missing -> confirm_removed ()
                 | _ -> Error (Error.Secret_store (Secrets.Error.code error)))))))
;;

let reconcile_owned t ~binding ~expected_operation =
  Eio.Switch.run (fun sw ->
    let open Result.Let_syntax in
    let%bind g = acquire t ~sw ~binding ~suffix:"G" ~mode:Exclusive in
    Exn.protect
      ~finally:(fun () -> Storage.Lock.release g)
      ~f:(fun () ->
        let%bind r = acquire t ~sw ~binding ~suffix:"R" ~mode:Exclusive in
        Exn.protect
          ~finally:(fun () -> Storage.Lock.release r)
          ~f:(fun () ->
            let%bind () =
              with_metadata t (fun registry ->
                let%bind removal = M.removal registry ~binding |> model in
                let%bind () =
                  match expected_operation with
                  | None -> Ok ()
                  | Some operation ->
                    let%bind snapshot = M.find registry ~binding |> model in
                    if
                      Option.is_none (M.Snapshot.active snapshot)
                      && Option.is_some (M.Snapshot.disabled snapshot)
                      && Option.exists removal ~f:(fun removal ->
                        M.Id.equal (M.Removal.operation removal) operation)
                    then Ok ()
                    else Error Error.Binding_unavailable
                in
                let%bind next =
                  (match removal with
                   | None -> Ok registry
                   | Some removal ->
                     M.record_removal
                       registry
                       ~binding
                       ~operation:(M.Removal.operation removal)
                       ~drain:Drained
                       ~revocation:(M.Removal.revocation removal))
                  |> model
                in
                publish t next)
            in
            let%bind () = cleanup_owned t ~binding in
            status t ~binding)))
;;

let reconcile t ~binding = reconcile_owned t ~binding ~expected_operation:None

module Revocation = struct
  type outcome =
    | Revoked
    | Definitely_not_submitted
    | Possibly_submitted
    | Unavailable

  type t =
    { revoke :
        sw:Eio.Switch.t
        -> identity:M.Identity.t
        -> grant:M.Grant.t
        -> Material.t
        -> outcome
    }

  let create ~revoke = { revoke }
end

let revoke_removed t ~sw ~binding ~operation ~active revocation =
  let open Result.Let_syntax in
  let record revocation =
    update t (fun registry ->
      M.record_removal registry ~binding ~operation ~drain:Drained ~revocation)
  in
  match revocation, active with
  | None, _ -> Ok ()
  | Some _, None -> record Unavailable
  | Some port, Some active ->
    (match
       ( M.Identity.method_ (M.Active.identity active)
       , M.Active.grant active
       , M.Active.source active )
     with
     | Oauth _, Some grant, Protected_revision revision ->
       (match read_material t ~binding revision with
        | Error _ -> record Unavailable
        | Ok material ->
          let%bind () = record Possibly_sent in
          let outcome =
            port.Revocation.revoke
              ~sw
              ~identity:(M.Active.identity active)
              ~grant
              material
          in
          record
            (match outcome with
             | Revoked -> M.Removal.Revoked
             | Definitely_not_submitted | Unavailable -> M.Removal.Unavailable
             | Possibly_submitted -> M.Removal.Possibly_sent))
     | _ -> record Unavailable)
;;

type removal =
  { disabled : bool
  ; cleanup : Status.cleanup
  }
[@@deriving sexp_of]

type operation_mode =
  | Fresh
  | Reconcile
[@@deriving sexp_of]

let disable_with_operation
      t
      ~sw:_
      ~clock
      ~maximum_wait
      ~binding
      ~operation
      ~mode
      ~revocation
      ~reason
  =
  let open Result.Let_syntax in
  let%bind admission =
    with_metadata t (fun registry ->
      match M.operation registry ~binding ~operation with
      | Ok receipt ->
        (match M.Operation.result receipt with
         | Committed ->
           let%bind snapshot = M.find registry ~binding |> model in
           let%bind removal = M.removal registry ~binding |> model in
           if
             Option.is_none (M.Snapshot.active snapshot)
             && Option.is_some (M.Snapshot.disabled snapshot)
             && Option.exists removal ~f:(fun removal ->
               M.Id.equal (M.Removal.operation removal) operation)
           then Ok `Replay
           else Error Error.Binding_unavailable
         | Pending | Rejected | Unavailable -> Error Error.Binding_unavailable)
      | Error M.Error.Stale_operation ->
        (match mode with
         | Reconcile -> Error Error.Binding_unavailable
         | Fresh ->
           let%bind snapshot = M.find registry ~binding |> model in
           let%bind next = M.disable registry ~binding ~operation ~reason |> model in
           let%map () = publish t next in
           `Fresh (M.Snapshot.active snapshot))
      | Error error -> Error (Error.Model error))
  in
  match admission with
  | `Replay ->
    (* Finish only the original tombstone's local drain/owned cleanup. The
       exact binding is rechecked under G/R and M before mutation; no revocation
       callback or fresh disable is invoked, and a later active login refuses. *)
    let%map current =
      match reconcile_owned t ~binding ~expected_operation:(Some operation) with
      | Ok current -> Ok current
      | Error error -> Error error
    in
    { disabled = true; cleanup = Status.cleanup current }
  | `Fresh active ->
    Eio.Switch.run (fun sw ->
      let locks =
        let%bind g =
          acquire_until
            t
            ~sw
            ~clock
            ~started:(Eio.Time.Mono.now clock)
            ~maximum_wait
            ~binding
            ~suffix:"G"
            ~mode:Exclusive
        in
        Exn.protect
          ~finally:(fun () -> Storage.Lock.release g)
          ~f:(fun () ->
            let%bind r =
              acquire_until
                t
                ~sw
                ~clock
                ~started:(Eio.Time.Mono.now clock)
                ~maximum_wait
                ~binding
                ~suffix:"R"
                ~mode:Exclusive
            in
            Exn.protect
              ~finally:(fun () -> Storage.Lock.release r)
              ~f:(fun () ->
                let%bind () =
                  update t (fun registry ->
                    M.record_removal
                      registry
                      ~binding
                      ~operation
                      ~drain:Drained
                      ~revocation:Not_requested)
                in
                let%bind () =
                  revoke_removed t ~sw ~binding ~operation ~active revocation
                in
                cleanup_owned t ~binding))
      in
      match locks with
      | Ok () | Error Timed_out | Error Busy ->
        let%map current = status t ~binding in
        { disabled = true; cleanup = Status.cleanup current }
      | Error error -> Error error)
;;

let disable t ~sw ~clock ~maximum_wait ~binding ~revocation ~reason =
  disable_with_operation
    t
    ~sw
    ~clock
    ~maximum_wait
    ~binding
    ~operation:(t.new_operation ())
    ~mode:Fresh
    ~revocation
    ~reason
;;

(* The registry borrows native resources, but joins every operation it starts.
   Attempt G fences belong to caller switches and do not keep close waiting for
   an idle transport channel. A finite call limit rejects before side effects. *)
let status_impl = status
let status t ~binding = with_call t (fun () -> status_impl t ~binding)
let synchronize_impl = synchronize
let synchronize t = with_call t (fun () -> synchronize_impl t)
let begin_candidate_impl = begin_candidate

let begin_candidate t ~binding ~operation ~expectation =
  with_call t (fun () -> begin_candidate_impl t ~binding ~operation ~expectation)
;;

let cancel_candidate_impl = cancel_candidate

let cancel_candidate t candidate =
  with_call t (fun () -> cancel_candidate_impl t candidate)
;;

let commit_candidate_impl = commit_candidate

let commit_candidate ?authorize_commit t candidate verified =
  with_call t (fun () -> commit_candidate_impl ?authorize_commit t candidate verified)
;;

let commit_environment_candidate_impl = commit_environment_candidate

let commit_environment_candidate
      ?authorize_commit
      t
      candidate
      ~identity
      ~name
      ~configuration_revision
  =
  with_call t (fun () ->
    commit_environment_candidate_impl
      ?authorize_commit
      t
      candidate
      ~identity
      ~name
      ~configuration_revision)
;;

let refresh_impl = refresh

let refresh t ~sw ~clock ~maximum_wait ~binding ~renewal =
  with_call t (fun () -> refresh_impl t ~sw ~clock ~maximum_wait ~binding ~renewal)
;;

let admit_impl = admit

let admit t ~sw ~clock ~maximum_wait ~binding ~expected_owner ~expected_epoch ~renewal =
  with_call t (fun () ->
    admit_impl
      t
      ~sw
      ~clock
      ~maximum_wait
      ~binding
      ~expected_owner
      ~expected_epoch
      ~renewal)
;;

let disable_with_operation_impl = disable_with_operation

let disable_with_operation
      t
      ~sw
      ~clock
      ~maximum_wait
      ~binding
      ~operation
      ~mode
      ~revocation
      ~reason
  =
  with_call t (fun () ->
    disable_with_operation_impl
      t
      ~sw
      ~clock
      ~maximum_wait
      ~binding
      ~operation
      ~mode
      ~revocation
      ~reason)
;;

let disable_impl = disable

let disable t ~sw ~clock ~maximum_wait ~binding ~revocation ~reason =
  with_call t (fun () ->
    disable_impl t ~sw ~clock ~maximum_wait ~binding ~revocation ~reason)
;;

let reconcile_impl = reconcile
let reconcile t ~binding = with_call t (fun () -> reconcile_impl t ~binding)
let reconcile_operation_impl = reconcile_operation

let reconcile_operation t ~binding ~operation =
  with_call t (fun () -> reconcile_operation_impl t ~binding ~operation)
;;

let cancel_pending_candidate t ~binding ~operation =
  with_call t (fun () ->
    Eio.Switch.run (fun sw ->
      let open Result.Let_syntax in
      let%bind r = acquire t ~sw ~binding ~suffix:"R" ~mode:Exclusive in
      Exn.protect
        ~finally:(fun () -> Storage.Lock.release r)
        ~f:(fun () ->
          update t (fun registry ->
            M.cancel_pending_candidate registry ~binding ~operation))))
;;

let incarnation t =
  with_call t (fun () -> with_metadata t (fun registry -> Ok (M.incarnation registry)))
;;

module For_testing = struct
  let set_after_publication_hook t hook = t.after_publication <- hook
end
