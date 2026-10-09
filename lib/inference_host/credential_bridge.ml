open! Core
module M = Credential_registry_model
module C = Credential_registry
module D = Openai.Responses_driver
module H = Provider_profiles
module R = Inference.Request
module E = Inference_runtime.Preparation_error
module Compatible_profile = Compatible_profile

module Operation = struct
  type t =
    | Inference
    | Status
    | Configure
    | Remove
  [@@deriving equal, sexp_of]
end

module Error = struct
  type t =
    | Invalid_mapping
    | Invalid_environment
    | Invalid_credential
    | Missing_profile
    | Denied
    | Stale_authorization
    | Profile of H.Error.t
    | Lifecycle of C.Error.t
    | Preparation of E.t
  [@@deriving sexp_of]
end

module Mapping = struct
  type t =
    { profile : D.Profile.t
    ; revision : string
    ; configuration : H.Profile.t
    ; binding : M.Id.t
    ; identity : M.Identity.t
    }

  let create profile ~revision ~binding ~identity =
    let open Result.Let_syntax in
    let%bind method_ =
      if
        (not (String.equal (M.Identity.provider identity) "openai"))
        || not
             (Option.equal
                String.equal
                (M.Identity.account identity)
                (D.Profile.account profile))
      then Error Error.Invalid_mapping
      else (
        match M.Identity.method_ identity with
        | Api_key _ ->
          if String.equal (M.Identity.billing identity) "api"
          then Ok "api_key"
          else Error Error.Invalid_mapping
        | Oauth _ ->
          if String.equal (M.Identity.billing identity) "subscription"
          then Ok "oauth_subscription"
          else Error Error.Invalid_mapping)
    in
    let%bind auth_binding =
      R.Auth_binding.create
        ~method_
        ~credential_reference:(M.Id.to_string binding)
        ~limits:Document_schema.Limits.default
      |> Result.map_error ~f:(fun _ -> Error.Invalid_mapping)
    in
    let%map configuration =
      H.Profile.create profile ~revision ~binding:auth_binding
      |> Result.map_error ~f:(fun error -> Error.Profile error)
    in
    { profile; revision; configuration; binding; identity }
  ;;

  let profile t = D.Profile.id t.profile
  let binding t = t.binding
end

let lifecycle_error error = Error.Lifecycle error

module Environment = struct
  module Entry = struct
    type t =
      { binding : M.Id.t
      ; identity : M.Identity.t
      ; name : string
      ; configuration_revision : M.Id.t option
      ; resolve : sw:Eio.Switch.t -> (C.Environment.resolved, Error.t) Result.t
      ; status : unit -> C.Status.availability
      }

    let create ~binding ~identity ~name ~configuration_revision ~resolve ~status =
      let valid_name =
        String.length name > 0
        && String.length name <= 128
        && String.for_all name ~f:(function
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
          | _ -> false)
        &&
        match name.[0] with
        | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
        | _ -> false
      in
      match M.Identity.method_ identity with
      | Oauth _ -> Error Error.Invalid_environment
      | Api_key _ ->
        if not valid_name
        then Error Error.Invalid_environment
        else Ok { binding; identity; name; configuration_revision; resolve; status }
    ;;
  end

  type t = Entry.t String.Map.t

  let create entries =
    if List.length entries > 128
    then Error Error.Invalid_environment
    else
      List.fold_result entries ~init:String.Map.empty ~f:(fun map entry ->
        if Map.mem map (M.Id.to_string entry.Entry.binding)
        then Error Error.Invalid_environment
        else Ok (Map.set map ~key:(M.Id.to_string entry.binding) ~data:entry))
  ;;

  let port t =
    C.Environment.create
      ~resolve:(fun ~sw ~binding ~identity ~name ~expected_configuration_revision ->
        match Map.find t (M.Id.to_string binding) with
        | None -> Error C.Error.Binding_unavailable
        | Some (entry : Entry.t) ->
          if
            (not (M.Identity.equal entry.identity identity))
            || (not (String.equal entry.name name))
            || not
                 (Option.equal
                    M.Id.equal
                    entry.configuration_revision
                    expected_configuration_revision)
          then Error C.Error.Binding_unavailable
          else
            let open Result.Let_syntax in
            let%map resolved =
              entry.resolve ~sw
              |> Result.map_error ~f:(function
                | Error.Lifecycle error -> error
                | Invalid_credential -> C.Error.Missing_secret
                | Invalid_mapping
                | Invalid_environment
                | Missing_profile
                | Denied
                | Stale_authorization
                | Profile _
                | Preparation _ -> C.Error.Binding_unavailable)
            in
            resolved)
      ~status:(fun ~binding ~name ->
        match Map.find t (M.Id.to_string binding) with
        | Some entry when String.equal entry.name name -> entry.status ()
        | None | Some _ -> C.Status.Missing)
  ;;
end

module Status = struct
  type availability =
    | Configured
    | Unavailable of C.Status.availability
    | Disabled
  [@@deriving equal, sexp_of]

  type t =
    { profile : string
    ; availability : availability
    ; lifecycle : C.Status.t option
    }

  let profile t = t.profile
  let availability t = t.availability
  let lifecycle t = t.lifecycle
end

module OAuth = struct
  type t =
    { lease :
        C.Admission.t
        -> identity:M.Identity.t
        -> profile:D.Profile.t
        -> (D.Auth.lease, D.Auth.error) Result.t
    ; renewal : M.Identity.t -> C.Renewal.t option
    }

  let create ~lease ~renewal = { lease; renewal }
end

type entry =
  { mapping : Mapping.t
  ; mutable snapshot : C.Host_snapshot.binding option
  ; mutable installed : (string * int64) option
  }

type choice =
  { descriptor : Compatible_profile.t
  ; mutable derived_mapping : Mapping.t option
  ; mutable installed : (string * int64) option
  }

type projection_state =
  | Unchecked
  | Current
  | Publication_failed

type shared =
  { registry : C.t
  ; entries : entry String.Table.t
  ; choices : choice String.Table.t
  ; authorize : principal:string -> profile:string -> operation:Operation.t -> bool
  ; oauth : OAuth.t option
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; maximum_wait : Time_ns.Span.t
  ; mutable projection_state : projection_state
  }

type t =
  { shared : shared
  ; profiles : H.t
  }

let auth_error = function
  | C.Error.Authorization_denied -> D.Auth.Denied
  | C.Error.Model (Disabled | Stale_epoch | Stale_revision | Wrong_incarnation) ->
    D.Auth.Denied
  | Model Renewal_uncertain | Renewal_rejected -> Reauthorization_required
  | Model (Invalid_identity | Invalid_grant) -> Invalid_credential
  | Timed_out | Busy -> Timed_out
  | Model
      ( Invalid_document
      | Unsupported_schema
      | Missing_registry
      | Capacity
      | Epoch_exhausted
      | Stale_operation
      | Not_active )
  | Storage _
  | Secret_store _
  | Missing_secret
  | Binding_unavailable
  | Closed
  | Publication_uncertain
  | Revision_quarantined -> Missing
;;

let current shared (entry : entry) =
  if
    match shared.projection_state with
    | Current -> false
    | Unchecked | Publication_failed -> true
  then None
  else
    Option.filter entry.snapshot ~f:(fun snapshot ->
      Option.equal
        M.Identity.equal
        (C.Host_snapshot.identity snapshot)
        (Some entry.mapping.identity))
;;

let profile_availability shared (entry : entry) =
  match current shared entry with
  | None -> H.Status.Missing
  | Some snapshot ->
    (match C.Host_snapshot.availability snapshot with
     | Disabled -> H.Status.Disabled
     | Renewal_uncertain -> Reauthorization_required
     | Renewal_required ->
       (match M.Identity.method_ entry.mapping.identity, shared.oauth with
        | Oauth _, Some port when Option.is_some (port.renewal entry.mapping.identity) ->
          Available
        | Api_key _, _ | Oauth _, None | Oauth _, Some _ -> Reauthorization_required)
     | Missing -> Missing
     | Ready ->
       (match M.Identity.method_ entry.mapping.identity, shared.oauth with
        | Api_key _, _ | Oauth _, Some _ -> Available
        | Oauth _, None -> Missing))
;;

let owner_profile shared profile =
  match Hashtbl.find shared.choices profile with
  | None -> profile
  | Some choice -> Compatible_profile.credential_owner choice.descriptor
;;

let owner_entry shared profile =
  Hashtbl.find shared.entries (owner_profile shared profile)
;;

let authorized shared ~principal ~profile ~operation =
  shared.authorize ~principal ~profile ~operation
  && shared.authorize ~principal ~profile:(owner_profile shared profile) ~operation
;;

(* Only explicitly declared logical views sharing this credential owner join
   the immutable replay policy. Identity, capabilities and existing model
   declarations remain owned by the canonical driver profile. *)
let qualify_mapping mapping ~compatible_profiles =
  let canonical_profile = Mapping.profile mapping in
  let profiles =
    canonical_profile
    :: List.filter_map compatible_profiles ~f:(fun descriptor ->
      if String.equal (Compatible_profile.credential_owner descriptor) canonical_profile
      then Some (Compatible_profile.id descriptor)
      else None)
  in
  if List.length profiles = 1
  then Ok mapping
  else
    Openai.Responses_replay.with_compatible_profiles
      (D.Profile.replay_policy mapping.Mapping.profile)
      ~canonical_profile
      ~profiles
    |> Result.map_error ~f:(fun _ -> Error.Invalid_mapping)
    |> Result.bind ~f:(fun replay ->
      Mapping.create
        (D.Profile.with_replay_policy mapping.profile replay)
        ~revision:mapping.revision
        ~binding:mapping.binding
        ~identity:mapping.identity)
;;

let derive_choice descriptor mapping =
  let open Result.Let_syntax in
  let%bind profile =
    Compatible_profile.derive descriptor ~canonical:mapping.Mapping.profile
    |> Result.map_error ~f:(fun _ -> Error.Invalid_mapping)
  in
  let revision =
    `Array
      [ `String "compatible-profile-v1"
      ; `String mapping.revision
      ; `String (Compatible_profile.revision descriptor)
      ]
    |> Jsonaf.to_string
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
    |> fun digest -> "choice-v1-" ^ digest
  in
  Mapping.create profile ~revision ~binding:mapping.binding ~identity:mapping.identity
;;

let credentials shared ~sw identity =
  let open Result.Let_syntax in
  let%bind entry =
    match owner_entry shared (H.Credential_identity.profile identity) with
    | None -> Error D.Auth.Missing
    | Some entry -> Ok entry
  in
  let%bind snapshot =
    match current shared entry with
    | None -> Error D.Auth.Missing
    | Some snapshot -> Ok snapshot
  in
  let%bind () =
    if
      String.equal (C.Host_snapshot.owner snapshot) (H.Credential_identity.owner identity)
      && Int64.equal
           (C.Host_snapshot.epoch snapshot)
           (H.Credential_identity.generation identity)
    then Ok ()
    else Error D.Auth.Denied
  in
  let renewal =
    match M.Identity.method_ entry.mapping.identity, shared.oauth with
    | Api_key _, _ -> None
    | Oauth _, None -> None
    | Oauth _, Some port -> port.renewal entry.mapping.identity
  in
  let%bind admission =
    C.admit
      shared.registry
      ~sw
      ~clock:shared.clock
      ~maximum_wait:shared.maximum_wait
      ~binding:entry.mapping.binding
      ~expected_owner:(H.Credential_identity.owner identity)
      ~expected_epoch:(H.Credential_identity.generation identity)
      ~renewal
    |> Result.map_error ~f:auth_error
  in
  let%bind () =
    if M.Identity.equal (C.Admission.identity admission) entry.mapping.identity
    then Ok ()
    else Error D.Auth.Denied
  in
  let%bind lease =
    match M.Identity.method_ entry.mapping.identity with
    | Api_key _ ->
      C.Admission.with_access admission ~f:(fun access ->
        Provider_secret_store.Secret.with_string access ~f:D.Auth.bearer)
    | Oauth _ ->
      (match shared.oauth with
       | None -> Error D.Auth.Missing
       | Some port ->
         port.lease
           admission
           ~identity:entry.mapping.identity
           ~profile:
             (match
                Hashtbl.find shared.choices (H.Credential_identity.profile identity)
              with
              | Some { derived_mapping = Some mapping; _ } -> mapping.profile
              | Some { derived_mapping = None; _ } | None -> entry.mapping.profile))
  in
  let%bind lease =
    D.Auth.with_identity
      lease
      ~owner:(C.Admission.owner admission)
      ~generation:(C.Admission.epoch admission)
      ~check_current:(fun () ->
        match shared.projection_state with
        | Unchecked | Publication_failed -> Error D.Auth.Missing
        | Current -> C.Admission.check_current admission |> Result.map_error ~f:auth_error)
  in
  match C.Admission.credential_revision admission with
  | None ->
    if Option.is_none (D.Auth.credential_revision lease)
    then Ok lease
    else Error D.Auth.Invalid_credential
  | Some revision -> D.Auth.with_credential_revision lease revision
;;

let admit_projection shared =
  match shared.projection_state with
  | Publication_failed -> Error Error.Stale_authorization
  | Unchecked | Current -> Ok ()
;;

let synchronize t =
  let open Result.Let_syntax in
  let%bind () = admit_projection t.shared in
  let observed = C.synchronize t.shared.registry in
  (* Lifecycle synchronization acquires a lock and reads durable metadata, so
     another fiber can fail publication while this call is suspended. Preserve
     that shared failure even when lifecycle synchronization returns an error. *)
  let%bind () = admit_projection t.shared in
  let%bind snapshot =
    match observed with
    | Ok snapshot -> Ok snapshot
    | Error error ->
      t.shared.projection_state <- Unchecked;
      Error (lifecycle_error error)
  in
  t.shared.projection_state <- Unchecked;
  let snapshots = C.Host_snapshot.bindings snapshot in
  let result =
    Hashtbl.fold t.shared.entries ~init:(Ok ()) ~f:(fun ~key:_ ~data:entry result ->
      let%bind () = result in
      let selected =
        List.find snapshots ~f:(fun binding ->
          M.Id.equal (C.Host_snapshot.id binding) entry.mapping.binding)
      in
      entry.snapshot <- selected;
      match selected with
      | None ->
        (match entry.installed with
         | None -> Ok ()
         | Some _ ->
           H.disable t.profiles ~profile:(Mapping.profile entry.mapping)
           |> Result.map_error ~f:(fun error -> Error.Profile error))
      | Some binding ->
        let owner = C.Host_snapshot.owner binding in
        let epoch = C.Host_snapshot.epoch binding in
        let%bind () =
          match entry.installed with
          | None ->
            H.add t.profiles entry.mapping.configuration ~owner ~generation:epoch
            |> Result.map_error ~f:(fun error -> Error.Profile error)
          | Some (old_owner, old_epoch) ->
            if String.equal owner old_owner && Int64.equal epoch old_epoch
            then Ok ()
            else
              H.reauthorize
                t.profiles
                ~profile:(Mapping.profile entry.mapping)
                ~owner
                ~generation:epoch
              |> Result.map_error ~f:(fun error -> Error.Profile error)
        in
        entry.installed <- Some (owner, epoch);
        (match C.Host_snapshot.availability binding with
         | Disabled ->
           H.disable t.profiles ~profile:(Mapping.profile entry.mapping)
           |> Result.map_error ~f:(fun error -> Error.Profile error)
         | Ready | Missing | Renewal_required | Renewal_uncertain -> Ok ()))
  in
  let%bind () = result in
  let%map () =
    Hashtbl.fold
      t.shared.choices
      ~init:(Ok ())
      ~f:(fun ~key:profile ~data:choice result ->
        let%bind () = result in
        match owner_entry t.shared profile with
        | None -> Ok ()
        | Some owner ->
          let%bind mapping = derive_choice choice.descriptor owner.mapping in
          choice.derived_mapping <- Some mapping;
          (match owner.snapshot with
           | None ->
             (match choice.installed with
              | None -> Ok ()
              | Some _ ->
                H.disable t.profiles ~profile
                |> Result.map_error ~f:(fun e -> Error.Profile e))
           | Some snapshot ->
             let owner_id = C.Host_snapshot.owner snapshot in
             let epoch = C.Host_snapshot.epoch snapshot in
             let%bind () =
               (match choice.installed with
                | None ->
                  H.add t.profiles mapping.configuration ~owner:owner_id ~generation:epoch
                | Some (old_owner, old_epoch) ->
                  if String.equal owner_id old_owner && Int64.equal epoch old_epoch
                  then Ok ()
                  else H.reauthorize t.profiles ~profile ~owner:owner_id ~generation:epoch)
               |> Result.map_error ~f:(fun e -> Error.Profile e)
             in
             choice.installed <- Some (owner_id, epoch);
             (match C.Host_snapshot.availability snapshot with
              | Disabled ->
                H.disable t.profiles ~profile
                |> Result.map_error ~f:(fun e -> Error.Profile e)
              | Ready | Missing | Renewal_required | Renewal_uncertain -> Ok ())))
  in
  t.shared.projection_state <- Current
;;

let mappings t = Hashtbl.data t.shared.entries |> List.map ~f:(fun entry -> entry.mapping)

let publish_mapping t mapping =
  let open Result.Let_syntax in
  let%bind () = admit_projection t.shared in
  let%bind mapping =
    qualify_mapping
      mapping
      ~compatible_profiles:
        (Hashtbl.data t.shared.choices |> List.map ~f:(fun choice -> choice.descriptor))
  in
  let profile = Mapping.profile mapping in
  let existing = Hashtbl.find t.shared.entries profile in
  let%bind derived =
    Hashtbl.data t.shared.choices
    |> List.filter ~f:(fun choice ->
      String.equal (Compatible_profile.credential_owner choice.descriptor) profile)
    |> List.map ~f:(fun choice ->
      Result.map (derive_choice choice.descriptor mapping) ~f:(fun mapping ->
        choice, mapping))
    |> Result.all
  in
  let%bind () =
    if
      Hashtbl.mem t.shared.choices profile
      || (Option.is_none existing
          && Hashtbl.length t.shared.entries + Hashtbl.length t.shared.choices >= 128)
      || Hashtbl.exists t.shared.entries ~f:(fun entry ->
        (not (String.equal (Mapping.profile entry.mapping) profile))
        && M.Id.equal entry.mapping.binding mapping.Mapping.binding)
    then Error Error.Invalid_mapping
    else Ok ()
  in
  let observed = C.synchronize t.shared.registry in
  let%bind () = admit_projection t.shared in
  let%bind snapshot = Result.map_error observed ~f:lifecycle_error in
  let%bind selected =
    List.find (C.Host_snapshot.bindings snapshot) ~f:(fun item ->
      M.Id.equal (C.Host_snapshot.id item) mapping.binding)
    |> Result.of_option ~error:Error.Missing_profile
  in
  let%bind () =
    if
      Option.equal
        M.Identity.equal
        (C.Host_snapshot.identity selected)
        (Some mapping.identity)
    then Ok ()
    else Error Error.Invalid_mapping
  in
  let identical =
    Option.exists existing ~f:(fun entry ->
      M.Identity.equal entry.mapping.identity mapping.identity
      && M.Id.equal entry.mapping.binding mapping.binding
      && String.equal entry.mapping.revision mapping.revision
      && String.equal
           (D.Profile.endpoint entry.mapping.profile)
           (D.Profile.endpoint mapping.profile))
  in
  if identical
  then synchronize t
  else (
    let owner = C.Host_snapshot.owner selected in
    let epoch = C.Host_snapshot.epoch selected in
    let configurations =
      mapping.configuration
      :: List.map derived ~f:(fun (_, mapping) -> mapping.Mapping.configuration)
    in
    let%bind () =
      H.validate_replacements t.profiles configurations ~owner ~generation:epoch
      |> Result.map_error ~f:(fun e -> Error.Profile e)
    in
    (* Every derived configuration is validated before this point. Registry
       batch admission owns ID/capacity/owner/generation checks. Any unexpected
       publication failure leaves all shared authority callbacks unavailable
       until the bridge is reconstructed from trusted declarations. *)
    t.shared.projection_state <- Publication_failed;
    let%bind () =
      H.replace_many t.profiles configurations ~owner ~generation:epoch
      |> Result.map_error ~f:(fun e -> Error.Profile e)
    in
    Hashtbl.set
      t.shared.entries
      ~key:profile
      ~data:{ mapping; snapshot = Some selected; installed = Some (owner, epoch) };
    List.iter derived ~f:(fun (choice, mapping) ->
      choice.derived_mapping <- Some mapping;
      choice.installed <- Some (owner, epoch));
    let%bind () =
      match C.Host_snapshot.availability selected with
      | Ready | Missing | Renewal_required | Renewal_uncertain -> Ok ()
      | Disabled ->
        List.fold_result
          (profile
           :: List.map derived ~f:(fun (choice, _) ->
             Compatible_profile.id choice.descriptor))
          ~init:()
          ~f:(fun () profile ->
            H.disable t.profiles ~profile
            |> Result.map_error ~f:(fun e -> Error.Profile e))
    in
    t.shared.projection_state <- Current;
    Ok ())
;;

let create
      ?oauth
      ?(compatible_profiles = [])
      ?(approved_profiles = [])
      driver
      ~registry
      ~mappings
      ~authorize
      ~clock
      ~maximum_wait
      ~transport_policy
      ~limits
  =
  let clock = (clock :> Eio.Time.Mono.ty Eio.Time.Mono.t) in
  let open Result.Let_syntax in
  let%bind () =
    if Time_ns.Span.(maximum_wait < zero) then Error Error.Invalid_mapping else Ok ()
  in
  let%bind () =
    if List.length mappings + List.length compatible_profiles > 128
    then Error Error.Invalid_mapping
    else Ok ()
  in
  let approved_profiles =
    String.Set.of_list (approved_profiles @ List.map mappings ~f:Mapping.profile)
  in
  let%bind () =
    Compatible_profile.validate_set
      compatible_profiles
      ~credential_owners:(Set.to_list approved_profiles)
    |> Result.map_error ~f:(fun _ -> Error.Invalid_mapping)
  in
  let%bind mappings =
    List.map mappings ~f:(fun mapping -> qualify_mapping mapping ~compatible_profiles)
    |> Result.all
  in
  let entries : entry String.Table.t = String.Table.create () in
  let%bind () =
    List.fold_result mappings ~init:() ~f:(fun () mapping ->
      if
        Hashtbl.mem entries (Mapping.profile mapping)
        || Hashtbl.exists entries ~f:(fun entry ->
          M.Id.equal entry.mapping.binding mapping.Mapping.binding)
      then Error Error.Invalid_mapping
      else (
        Hashtbl.add_exn
          entries
          ~key:(Mapping.profile mapping)
          ~data:{ mapping; snapshot = None; installed = None };
        Ok ()))
  in
  let choices : choice String.Table.t = String.Table.create () in
  List.iter compatible_profiles ~f:(fun descriptor ->
    Hashtbl.add_exn
      choices
      ~key:(Compatible_profile.id descriptor)
      ~data:{ descriptor; derived_mapping = None; installed = None });
  let%bind () =
    List.fold_result compatible_profiles ~init:() ~f:(fun () descriptor ->
      match Hashtbl.find entries (Compatible_profile.credential_owner descriptor) with
      | None -> Ok ()
      | Some entry ->
        derive_choice descriptor entry.mapping |> Result.map ~f:(fun _ -> ()))
  in
  let shared =
    { registry
    ; entries
    ; choices
    ; authorize
    ; oauth
    ; clock
    ; maximum_wait
    ; projection_state = Unchecked
    }
  in
  let profiles =
    H.create
      ~transport_policy
      driver
      ~authorize:(fun ~principal ~profile ~account:_ ~binding:_ ->
        authorized shared ~principal ~profile ~operation:Operation.Inference)
      ~credentials:(credentials shared)
      ~status:(fun identity ->
        match owner_entry shared (H.Credential_identity.profile identity) with
        | None -> H.Status.Missing
        | Some entry -> profile_availability shared entry)
      ~limits
  in
  let t = { shared; profiles } in
  let%map () = synchronize t in
  t
;;

let with_response_limit t ~max_body_bytes =
  H.with_response_limit t.profiles ~max_body_bytes
  |> Result.map_error ~f:(fun error -> Error.Preparation error)
  |> Result.map ~f:(fun profiles -> { t with profiles })
;;

let entry t ~principal ~profile ~operation =
  let open Result.Let_syntax in
  let%bind () = admit_projection t.shared in
  if not (authorized t.shared ~principal ~profile ~operation)
  then Error Error.Denied
  else (
    match
      match operation with
      | Inference | Status -> owner_entry t.shared profile
      | Configure | Remove -> Hashtbl.find t.shared.entries profile
    with
    | None -> Error Error.Missing_profile
    | Some entry -> Ok entry)
;;

let check_choice_target t target =
  match Hashtbl.find t.shared.choices (R.Target.profile target) with
  | None -> Ok ()
  | Some { derived_mapping = None; _ } -> Error Error.Missing_profile
  | Some { derived_mapping = Some mapping; _ } ->
    if
      Option.equal
        String.equal
        (R.Target.profile_revision target)
        (Some mapping.Mapping.revision)
    then Ok ()
    else Error (Error.Profile H.Error.Incompatible_identity)
;;

let resolve t ~principal target =
  let open Result.Let_syntax in
  let%bind _ =
    entry t ~principal ~profile:(R.Target.profile target) ~operation:Inference
  in
  let%bind () = synchronize t in
  let%bind () = check_choice_target t target in
  H.resolve t.profiles ~principal target
  |> Result.map_error ~f:(fun error -> Error.Profile error)
;;

let capture t ~principal ~default_profile ~current ~model ~settings =
  let open Result.Let_syntax in
  let profile = Option.value_map current ~default:default_profile ~f:R.Target.profile in
  let%bind _ = entry t ~principal ~profile ~operation:Inference in
  let%bind () = synchronize t in
  let%bind () =
    match current with
    | None -> Ok ()
    | Some target ->
      let%bind () = check_choice_target t target in
      H.resolve t.profiles ~principal target
      |> Result.map ~f:(fun _ -> ())
      |> Result.map_error ~f:(fun error -> Error.Profile error)
  in
  H.capture t.profiles ~principal ~profile ~model ~settings
  |> Result.map_error ~f:(fun error -> Error.Profile error)
;;

let preparation_error = function
  | Error.Preparation error -> error
  | Profile (H.Error.Preparation error) -> error
  | Denied | Profile Denied -> E.Target_denied
  | Invalid_mapping | Invalid_environment | Invalid_credential -> Invalid_preparation
  | Profile Incompatible_identity -> Target_mismatch
  | Profile Reauthorization_required -> Reauthorization_required
  | Missing_profile | Stale_authorization | Lifecycle _
  | Profile (Missing_profile | Binding_unavailable | Disabled | Invalid_profile) ->
    Target_unavailable
;;

let resolver t ~principal target =
  resolve t ~principal target |> Result.map_error ~f:preparation_error
;;

let status t ~principal ~profile =
  let open Result.Let_syntax in
  let%bind entry = entry t ~principal ~profile ~operation:Status in
  let%bind () = synchronize t in
  let%map lifecycle =
    match C.status t.shared.registry ~binding:entry.mapping.binding with
    | Ok status -> Ok (Some status)
    | Error (C.Error.Model M.Error.Not_active) -> Ok None
    | Error error -> Error (lifecycle_error error)
  in
  let availability =
    match Option.map lifecycle ~f:C.Status.availability with
    | None -> Status.Unavailable C.Status.Missing
    | Some Disabled -> Status.Disabled
    | Some Available ->
      (match profile_availability t.shared entry with
       | H.Status.Available -> Configured
       | Missing -> Unavailable C.Status.Missing
       | Disabled -> Disabled
       | Reauthorization_required -> Unavailable Renewal_required)
    | Some
        (( Missing
         | Renewal_required
         | Renewal_uncertain
         | Secret_unavailable
         | Store_unavailable ) as unavailable) -> Unavailable unavailable
  in
  { Status.profile; availability; lifecycle }
;;

let api_entry t ~principal ~profile =
  let open Result.Let_syntax in
  let%bind entry = entry t ~principal ~profile ~operation:Configure in
  match M.Identity.method_ entry.mapping.identity with
  | Api_key _ -> Ok entry
  | Oauth _ -> Error Error.Invalid_mapping
;;

let finish_publication t ~binding ~operation result =
  let result =
    match result with
    | Ok () -> Ok ()
    | Error C.Error.Authorization_denied -> Error Error.Denied
    | Error C.Error.Publication_uncertain ->
      (match C.reconcile_operation t.shared.registry ~binding ~operation with
       | Ok M.Operation.Committed -> Ok ()
       | Ok (Pending | Rejected | Unavailable) ->
         Error (lifecycle_error C.Error.Publication_uncertain)
       | Error error -> Error (lifecycle_error error))
    | Error error -> Error (lifecycle_error error)
  in
  let synchronized = synchronize t in
  match result, synchronized with
  | Error error, _ | Ok _, Error error -> Error error
  | Ok (), Ok () -> Ok ()
;;

let commit_authorized t ~principal ~profile ~(captured : entry) ~authorize_commit () =
  authorize_commit ()
  &&
  match api_entry t ~principal ~profile with
  | Error _ -> false
  | Ok current ->
    M.Id.equal current.mapping.binding captured.mapping.binding
    && M.Identity.equal current.mapping.identity captured.mapping.identity
;;

let enroll ?(authorize_commit = fun () -> true) t ~principal ~profile ~operation ~sw ~read
  =
  let open Result.Let_syntax in
  let%bind entry = api_entry t ~principal ~profile in
  let%bind candidate =
    C.begin_candidate
      t.shared.registry
      ~binding:entry.mapping.binding
      ~operation
      ~expectation:(M.Expectation.exact entry.mapping.identity)
    |> Result.map_error ~f:lifecycle_error
  in
  let commit_started = ref false in
  let finished = ref false in
  Exn.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        if not !commit_started
        then
          ignore
            (C.cancel_candidate t.shared.registry candidate : (unit, C.Error.t) Result.t)
        else if not !finished
        then (
          (* A cancelled commit can have published. Never delete a possibly
             authoritative revision or assume cancellation rolled back metadata. *)
          ignore
            (C.reconcile_operation
               t.shared.registry
               ~binding:entry.mapping.binding
               ~operation
             : (M.Operation.result, C.Error.t) Result.t);
          ignore (synchronize t : (unit, Error.t) Result.t))))
    ~f:(fun () ->
      let%bind access = read ~sw in
      let%bind _ =
        Provider_secret_store.Secret.with_string access ~f:D.Auth.bearer
        |> Result.map_error ~f:(fun _ -> Error.Invalid_credential)
      in
      let%bind verified =
        C.Verified.create
          ~identity:entry.mapping.identity
          ~grant:None
          ~material:(C.Material.api_key access)
        |> Result.map_error ~f:lifecycle_error
      in
      let%bind current = api_entry t ~principal ~profile in
      let%bind () =
        if
          M.Id.equal current.mapping.binding entry.mapping.binding
          && M.Identity.equal current.mapping.identity entry.mapping.identity
        then Ok ()
        else Error Error.Stale_authorization
      in
      let%bind () = if authorize_commit () then Ok () else Error Error.Denied in
      commit_started := true;
      let result =
        finish_publication
          t
          ~binding:entry.mapping.binding
          ~operation
          (C.commit_candidate
             ~authorize_commit:
               (commit_authorized t ~principal ~profile ~captured:entry ~authorize_commit)
             t.shared.registry
             candidate
             verified)
      in
      finished := true;
      result)
;;

let configure_environment
      ?(authorize_commit = fun () -> true)
      t
      ~principal
      ~profile
      ~operation
      ~name
      ~configuration_revision
  =
  let open Result.Let_syntax in
  let%bind entry = api_entry t ~principal ~profile in
  let%bind candidate =
    C.begin_candidate
      t.shared.registry
      ~binding:entry.mapping.binding
      ~operation
      ~expectation:(M.Expectation.exact entry.mapping.identity)
    |> Result.map_error ~f:lifecycle_error
  in
  let commit_started = ref false in
  let finished = ref false in
  Exn.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        if not !commit_started
        then
          ignore
            (C.cancel_candidate t.shared.registry candidate : (unit, C.Error.t) Result.t)
        else if not !finished
        then (
          ignore
            (C.reconcile_operation
               t.shared.registry
               ~binding:entry.mapping.binding
               ~operation
             : (M.Operation.result, C.Error.t) Result.t);
          ignore (synchronize t : (unit, Error.t) Result.t))))
    ~f:(fun () ->
      let%bind current = api_entry t ~principal ~profile in
      let%bind () =
        if
          M.Id.equal current.mapping.binding entry.mapping.binding
          && M.Identity.equal current.mapping.identity entry.mapping.identity
        then Ok ()
        else Error Error.Stale_authorization
      in
      let%bind () = if authorize_commit () then Ok () else Error Error.Denied in
      commit_started := true;
      let result =
        finish_publication
          t
          ~binding:entry.mapping.binding
          ~operation
          (C.commit_environment_candidate
             ~authorize_commit:
               (commit_authorized t ~principal ~profile ~captured:entry ~authorize_commit)
             t.shared.registry
             candidate
             ~identity:entry.mapping.identity
             ~name
             ~configuration_revision)
      in
      finished := true;
      result)
;;

let remove t ~principal ~profile ~sw =
  let open Result.Let_syntax in
  let%bind entry = entry t ~principal ~profile ~operation:Remove in
  let%bind () = synchronize t in
  let reason =
    match Option.bind entry.snapshot ~f:C.Host_snapshot.source with
    | Some (M.Active.Environment_reference _) -> M.Snapshot.Environment_disabled
    | Some (Protected_revision _) | None -> Key_removed
  in
  let result =
    C.disable
      t.shared.registry
      ~sw
      ~clock:t.shared.clock
      ~maximum_wait:t.shared.maximum_wait
      ~binding:entry.mapping.binding
      ~revocation:None
      ~reason
    |> Result.map_error ~f:lifecycle_error
  in
  let synchronized = synchronize t in
  match result, synchronized with
  | Error error, _ | Ok _, Error error -> Error error
  | Ok removal, Ok () -> Ok removal
;;
