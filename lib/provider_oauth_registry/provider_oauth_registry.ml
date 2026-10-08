open! Core
module O = Provider_oauth
module C = Credential_registry
module M = Credential_registry_model
module D = Openai.Responses_driver
module P = Provider_oauth_protocol.Presence

module Error = struct
  type t =
    | OAuth of O.Error.t
    | Registry of C.Error.t
    | Invalid_binding
    | Denied
    | Closed
  [@@deriving sexp_of]
end

type t =
  { policy : O.Policy.t
  ; refresh : O.Refresh.policy -> O.Verified.t -> O.Refresh.outcome
  }

type adapter = t

let create ~transport ~policy ~wall_clock =
  { policy
  ; refresh =
      (fun refresh_policy prior ->
        O.Refresh.exchange ~transport ~policy ~refresh_policy ~wall_clock prior)
  }
;;

let presence = function
  | P.Absent -> M.Presence.Absent
  | Null -> Null
  | Value value -> Value value
;;

let protocol_presence = function
  | M.Presence.Absent -> P.Absent
  | Null -> Null
  | Value value -> Value value
;;

let fixed_identity t identity =
  String.equal (M.Identity.provider identity) "openai"
  && String.equal (M.Identity.billing identity) "subscription"
  && Option.is_some (M.Identity.account identity)
  &&
  match M.Identity.method_ identity with
  | Api_key _ -> false
  | Oauth { issuer; client_registration; resource; _ } ->
    String.equal issuer (O.Policy.issuer t.policy)
    && String.equal client_registration (O.Policy.client_registration t.policy)
    && String.equal resource (O.Policy.resource t.policy)
;;

let registry_verified t ~identity ~refresh_policy verified =
  let open Result.Let_syntax in
  let%bind () =
    if
      fixed_identity t identity
      && Option.equal
           String.equal
           (M.Identity.account identity)
           (Some (O.Verified.account verified))
      &&
      match M.Identity.method_ identity with
      | Api_key _ -> false
      | Oauth { verified_subject; required_scopes; _ } ->
        String.equal verified_subject (O.Verified.subject verified)
        && List.for_all required_scopes ~f:(fun required ->
          List.mem (O.Verified.scopes verified) required ~equal:String.equal)
    then Ok ()
    else Error Error.Invalid_binding
  in
  let expiry = O.Verified.expires_at verified in
  let%bind () =
    if Float.is_finite expiry && Float.(expiry > 0. && expiry <= 9007199254740991.)
    then Ok ()
    else Error Error.Invalid_binding
  in
  (* JWT exp is an integral seconds declaration. Convert before multiplying to
     avoid rounding millisecond integers through float; never infer expires_in. *)
  let at_ms = Int64.(of_float expiry * 1000L) in
  let scopes_provenance =
    match O.Verified.scope_source verified with
    | Token_response -> M.Grant.Declared
    | Browser_request -> Qualified_request
    | Prior_exact -> Qualified_prior_exact
    | Qualified_access_token_claim -> Qualified_token_claim
  in
  let%bind grant =
    M.Grant.create
      ~identity
      ~scopes:(presence (O.Verified.scope_presence verified))
      ~expires_at_ms:(M.Presence.Value at_ms)
      ~refresh_policy
      ~effective:
        { M.Grant.scopes = O.Verified.scopes verified
        ; scopes_provenance
        ; expiry = Known { at_ms; provenance = Declared }
        ; unknown_expiry_policy = Reject_unknown
        }
    |> Result.map_error ~f:(fun error -> Error.Registry (C.Error.Model error))
  in
  let material =
    O.Verified.with_material verified ~f:(fun ~access ~refresh ->
      O.Verified.with_continuity verified ~f:(fun continuity ->
        C.Material.oauth
          ~access
          ~refresh:(presence refresh)
          ~continuity:(M.Presence.Value continuity)))
  in
  C.Verified.create ~identity ~grant:(Some grant) ~material
  |> Result.map_error ~f:(fun error -> Error.Registry error)
;;

let lease t admission ~identity ~profile =
  let open Result.Let_syntax in
  let%bind () =
    if
      fixed_identity t identity
      && M.Identity.equal (C.Admission.identity admission) identity
      && String.equal
           (D.Profile.endpoint profile)
           "https://chatgpt.com/backend-api/codex/responses"
      && Option.equal
           String.equal
           (D.Profile.account profile)
           (M.Identity.account identity)
    then Ok ()
    else Error D.Auth.Invalid_credential
  in
  let account = M.Identity.account identity |> Option.value_exn in
  let%bind lease =
    C.Admission.with_access admission ~f:(fun access ->
      Provider_secret_store.Secret.with_string access ~f:(fun token ->
        D.Auth.direct_codex token ~account))
  in
  let%bind lease =
    D.Auth.with_identity
      lease
      ~owner:(C.Admission.owner admission)
      ~generation:(C.Admission.epoch admission)
      ~check_current:(fun () ->
        C.Admission.check_current admission
        |> Result.map_error ~f:(fun _ -> D.Auth.Reauthorization_required))
  in
  match C.Admission.credential_revision admission with
  | None -> Ok lease
  | Some revision -> D.Auth.with_credential_revision lease revision
;;

let renewal t identity =
  if not (fixed_identity t identity)
  then None
  else
    Some
      (C.Renewal.create ~exchange:(fun ~sw ~identity:actual ~grant material ->
         Eio.Switch.check sw;
         if
           (not (M.Identity.equal actual identity))
           || not (M.Identity.equal (M.Grant.identity grant) identity)
         then C.Renewal.Definitely_not_submitted
         else (
           let existing =
             C.Material.with_oauth material ~f:(fun ~access ~refresh ~continuity ->
               match
                 M.Identity.method_ identity, M.Grant.(effective grant).expiry, continuity
               with
               | ( Oauth { issuer; client_registration; resource; verified_subject; _ }
                 , Known { at_ms; _ }
                 , Value continuity ) ->
                 O.Existing.of_registry
                   ~issuer
                   ~client_registration
                   ~resource
                   ~account:(M.Identity.account identity |> Option.value_exn)
                   ~subject:verified_subject
                   ~scopes:M.Grant.(effective grant).scopes
                   ~expires_at:(Int64.to_float at_ms /. 1000.)
                   ~access
                   ~refresh:(protocol_presence refresh)
                   ~continuity
                 |> Result.map_error ~f:(fun _ -> Error.Invalid_binding)
               | _ -> Error Error.Invalid_binding)
             |> Result.map_error ~f:(fun error -> Error.Registry error)
             |> Result.join
           in
           match existing with
           | Error _ -> C.Renewal.Definitely_not_submitted
           | Ok prior ->
             let refresh_policy =
               match M.Grant.refresh_policy grant with
               | Preserve_omitted -> O.Refresh.Preserve_omitted
               | Require_rotated -> Require_rotated
             in
             let outcome = t.refresh refresh_policy prior in
             Eio.Switch.check sw;
             (match outcome with
              | Definitely_not_submitted -> C.Renewal.Definitely_not_submitted
              | Authoritative_rejection -> Authoritative_rejection
              | Possibly_consumed -> Possibly_consumed
              | Verified verified ->
                (match
                   registry_verified
                     t
                     ~identity
                     ~refresh_policy:(M.Grant.refresh_policy grant)
                     verified
                 with
                 | Ok verified -> C.Renewal.Verified verified
                 | Error _ -> Possibly_consumed)))))
;;

module Acquisition = struct
  type state =
    | Pending
    | Publishing
    | Committed
    | Publication_uncertain
    | Closed of (unit, Error.t) result

  type t =
    { adapter : adapter
    ; registry : C.t
    ; candidate : C.Candidate.t
    ; host : M.Id.t
    ; expectation : M.Expectation.t
    ; refresh_policy : M.Grant.refresh_policy
    ; login : O.Login.t
    ; mutex : Eio.Mutex.t
    ; mutable state : state
    }

  let close t =
    match t.state with
    | Closed result -> result
    | Pending | Publishing | Committed | Publication_uncertain ->
      Eio.Cancel.protect (fun () ->
        (* Join before taking the state mutex so an await under complete cannot
         prevent owner close from stopping the network worker. Still retire the
         original candidate if the worker raised; preserve its exception. *)
        let primary =
          try
            O.Login.close t.login;
            None
          with
          | ex -> Some (ex, Stdlib.Printexc.get_raw_backtrace ())
        in
        let cleanup =
          try
            Ok
              (Eio.Mutex.use_ro t.mutex (fun () ->
                 match t.state with
                 | Committed -> Ok ()
                 | Publishing | Publication_uncertain ->
                   Error (Error.Registry C.Error.Publication_uncertain)
                 | Closed result -> result
                 | Pending ->
                   let result =
                     C.cancel_candidate t.registry t.candidate
                     |> Result.map_error ~f:(fun error -> Error.Registry error)
                   in
                   t.state <- Closed result;
                   result))
          with
          | ex -> Error (ex, Stdlib.Printexc.get_raw_backtrace ())
        in
        match primary, cleanup with
        | Some (ex, bt), _ | None, Error (ex, bt) ->
          Stdlib.Printexc.raise_with_backtrace ex bt
        | None, Ok result -> result)
  ;;

  let start
        adapter
        ~registry
        ~sw
        ~host
        ~binding
        ~operation
        ~expectation
        ~refresh_policy
        ~start
    =
    let open Result.Let_syntax in
    let%bind candidate =
      C.begin_candidate registry ~binding ~operation ~expectation
      |> Result.map_error ~f:(fun error -> Error.Registry error)
    in
    let cancel_original () =
      Eio.Cancel.protect (fun () -> C.cancel_candidate registry candidate)
    in
    match start ~sw with
    | Error error ->
      (match cancel_original () with
       | Ok () -> Error (Error.OAuth error)
       | Error cleanup -> Error (Error.Registry cleanup))
    | Ok (login, challenge) ->
      let t =
        { adapter
        ; registry
        ; candidate
        ; host
        ; expectation
        ; refresh_policy
        ; login
        ; mutex = Eio.Mutex.create ()
        ; state = Pending
        }
      in
      (try
         Eio.Switch.on_release sw (fun () ->
           Eio.Cancel.protect (fun () -> ignore (close t : (unit, Error.t) result)))
       with
       | ex ->
         let bt = Stdlib.Printexc.get_raw_backtrace () in
         (try
            Eio.Cancel.protect (fun () -> ignore (close t : (unit, Error.t) result))
          with
          | _ -> ());
         Stdlib.Printexc.raise_with_backtrace ex bt);
      Ok (t, challenge)
    | exception ex ->
      let bt = Stdlib.Printexc.get_raw_backtrace () in
      (try ignore (cancel_original () : (unit, C.Error.t) result) with
       | _ -> ());
      Stdlib.Printexc.raise_with_backtrace ex bt
  ;;

  let complete t ~authorize_commit =
    Eio.Mutex.use_ro t.mutex (fun () ->
      match t.state with
      | Committed | Publishing | Publication_uncertain | Closed _ -> Error Error.Closed
      | Pending ->
        let open Result.Let_syntax in
        let%bind verified =
          O.Login.await t.login |> Result.map_error ~f:(fun error -> Error.OAuth error)
        in
        let%bind required_scopes =
          match M.Expectation.oauth_required_scopes t.expectation with
          | Some scopes -> Ok scopes
          | None -> Error Error.Invalid_binding
        in
        let%bind identity =
          M.Identity.oauth
            ~host:t.host
            ~provider:"openai"
            ~billing:"subscription"
            ~issuer:(O.Policy.issuer t.adapter.policy)
            ~client_registration:(O.Policy.client_registration t.adapter.policy)
            ~resource:(O.Policy.resource t.adapter.policy)
            ~account:(O.Verified.account verified)
            ~verified_subject:(O.Verified.subject verified)
            ~required_scopes
          |> Result.map_error ~f:(fun error -> Error.Registry (C.Error.Model error))
        in
        let%bind () =
          if M.Expectation.accepts t.expectation identity
          then Ok ()
          else Error Error.Invalid_binding
        in
        let%bind verified =
          registry_verified t.adapter ~identity ~refresh_policy:t.refresh_policy verified
        in
        let%bind () = if authorize_commit () then Ok () else Error Error.Denied in
        t.state <- Publishing;
        (match C.commit_candidate ~authorize_commit t.registry t.candidate verified with
         | Ok () ->
           t.state <- Committed;
           Ok ()
         | Error C.Error.Authorization_denied ->
           t.state <- Pending;
           Error Error.Denied
         | Error C.Error.Publication_uncertain ->
           t.state <- Publication_uncertain;
           Error (Error.Registry C.Error.Publication_uncertain)
         | Error error ->
           t.state <- Pending;
           Error (Error.Registry error)))
  ;;
end
