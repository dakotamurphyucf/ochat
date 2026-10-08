open! Core
module D = Openai.Responses_driver
module R = Inference.Request
module A = Openai.Inference_adapter
module P = History_entry.Payload.Presence

module Error = struct
  type t =
    | Missing_profile
    | Denied
    | Incompatible_identity
    | Binding_unavailable
    | Disabled
    | Reauthorization_required
    | Invalid_profile
    | Preparation of Inference_runtime.Preparation_error.t
  [@@deriving equal, sexp_of]
end

let valid_label value =
  Result.is_ok
    (Document_schema.Json.validate (`String value) ~limits:Document_schema.Limits.default)
  && not (String.is_empty (String.strip value))
;;

module Profile = struct
  type t =
    { profile : D.Profile.t
    ; revision : string
    ; binding : R.Auth_binding.t
    }

  let create profile ~revision ~binding =
    if
      (not (valid_label revision))
      || (not (valid_label (D.Profile.id profile)))
      || (not (Option.for_all (D.Profile.account profile) ~f:valid_label))
      || (not (valid_label (D.Profile.endpoint profile)))
      || Result.is_error
           (R.Auth_binding.of_json
              (R.Auth_binding.to_json binding)
              ~limits:Document_schema.Limits.default)
      || not
           (String.equal (R.Auth_binding.method_ binding) "api_key"
            || String.equal (R.Auth_binding.method_ binding) "oauth_subscription")
    then Error Error.Invalid_profile
    else Ok { profile; revision; binding }
  ;;

  let id t = D.Profile.id t.profile
end

module Credential_identity = struct
  type t =
    { profile : string
    ; account : string option
    ; binding : R.Auth_binding.t
    ; owner : string
    ; generation : int64
    }

  let profile t = t.profile
  let account t = t.account
  let binding t = t.binding
  let owner t = t.owner
  let generation t = t.generation

  let equal a b =
    String.equal a.profile b.profile
    && Option.equal String.equal a.account b.account
    && R.Auth_binding.equal a.binding b.binding
    && String.equal a.owner b.owner
    && Int64.equal a.generation b.generation
  ;;

  let sexp_of_t t =
    [%sexp
      { profile = (t.profile : string)
      ; account = (t.account : string option)
      ; method_ = (R.Auth_binding.method_ t.binding : string)
      ; credential_reference = (R.Auth_binding.credential_reference t.binding : string)
      ; owner = (t.owner : string)
      ; generation = (t.generation : int64)
      }]
  ;;
end

module Status = struct
  type availability =
    | Available
    | Missing
    | Disabled
    | Reauthorization_required
  [@@deriving equal, sexp_of]

  type t =
    { identity : Credential_identity.t
    ; availability : availability
    }

  let identity t = t.identity
  let availability t = t.availability

  let sexp_of_t t =
    [%sexp
      { identity = (t.identity : Credential_identity.t)
      ; availability = (t.availability : availability)
      }]
  ;;
end

type installed =
  { mutable configuration : Profile.t
  ; mutable owner : string
  ; mutable generation : int64
  ; mutable disabled : bool
  ; mutable revision_epoch : int
  ; mutable removed : bool
  }

type t =
  { driver : D.t
  ; entries : installed String.Table.t
  ; authorize :
      principal:string
      -> profile:string
      -> account:string option
      -> binding:R.Auth_binding.t
      -> bool
  ; credentials :
      sw:Eio.Switch.t -> Credential_identity.t -> (D.Auth.lease, D.Auth.error) Result.t
  ; status : Credential_identity.t -> Status.availability
  ; limits : Inference_runtime.Limits.t
  }

let create driver ~authorize ~credentials ~status ~limits =
  { driver; entries = String.Table.create (); authorize; credentials; status; limits }
;;

let identity entry =
  let configuration = entry.configuration in
  Credential_identity.
    { profile = Profile.id configuration
    ; account = D.Profile.account configuration.profile
    ; binding = configuration.binding
    ; owner = entry.owner
    ; generation = entry.generation
    }
;;

let find t profile =
  match Hashtbl.find t.entries profile with
  | None -> Error Error.Missing_profile
  | Some entry -> if entry.removed then Error Error.Missing_profile else Ok entry
;;

let add t configuration ~owner ~generation =
  if
    (not (valid_label owner))
    || Int64.(generation < 0L)
    || Hashtbl.mem t.entries (Profile.id configuration)
  then Error Error.Invalid_profile
  else (
    Hashtbl.set
      t.entries
      ~key:(Profile.id configuration)
      ~data:
        { configuration
        ; owner
        ; generation
        ; disabled = false
        ; revision_epoch = 0
        ; removed = false
        };
    Ok ())
;;

let remove t ~profile =
  Result.map (find t profile) ~f:(fun entry ->
    entry.removed <- true;
    entry.disabled <- true)
;;

let disable t ~profile =
  Result.map (find t profile) ~f:(fun entry -> entry.disabled <- true)
;;

let compatible a b =
  String.equal (Profile.id a) (Profile.id b)
  && Option.equal
       String.equal
       (D.Profile.account a.Profile.profile)
       (D.Profile.account b.Profile.profile)
  && String.equal (D.Profile.endpoint a.profile) (D.Profile.endpoint b.profile)
  && R.Auth_binding.equal a.binding b.binding
;;

let edit_profile t configuration =
  let open Result.Let_syntax in
  let%bind entry = find t (Profile.id configuration) in
  if not (compatible entry.configuration configuration)
  then Error Error.Incompatible_identity
  else if entry.revision_epoch = Int.max_value
  then Error Error.Invalid_profile
  else (
    entry.configuration <- configuration;
    entry.revision_epoch <- entry.revision_epoch + 1;
    Ok ())
;;

let reauthorize t ~profile ~owner ~generation =
  let open Result.Let_syntax in
  let%bind entry = find t profile in
  if (not (valid_label owner)) || Int64.(generation <= entry.generation)
  then Error Error.Invalid_profile
  else (
    entry.owner <- owner;
    entry.generation <- generation;
    entry.disabled <- false;
    Ok ())
;;

let authorized t ~principal entry =
  valid_label principal
  && t.authorize
       ~principal
       ~profile:(Profile.id entry.configuration)
       ~account:(D.Profile.account entry.configuration.profile)
       ~binding:entry.configuration.binding
;;

let admit t ~principal ~profile =
  let open Result.Let_syntax in
  let%bind entry = find t profile in
  if authorized t ~principal entry then Ok entry else Error Error.Denied
;;

let status t ~principal ~profile =
  let open Result.Let_syntax in
  let%map entry = admit t ~principal ~profile in
  let identity = identity entry in
  let availability = if entry.disabled then Status.Disabled else t.status identity in
  { Status.identity; availability }
;;

let available t entry =
  if entry.disabled
  then Error Error.Disabled
  else (
    match t.status (identity entry) with
    | Available -> Ok ()
    | Missing -> Error Error.Binding_unavailable
    | Disabled -> Error Error.Disabled
    | Reauthorization_required -> Error Error.Reauthorization_required)
;;

let capture t ~principal ~profile ~model ~settings =
  let open Result.Let_syntax in
  let%bind entry = admit t ~principal ~profile in
  let%bind () = available t entry in
  let configuration = entry.configuration in
  let%bind target =
    A.capture_target
      configuration.profile
      ~profile_revision:(Some configuration.revision)
      ~model
      ~settings
      ~limits:Document_schema.Limits.default
    |> Result.map_error ~f:(fun error -> Error.Preparation error)
  in
  R.Target.with_auth_binding
    target
    ~binding:(P.Value configuration.binding)
    ~limits:Document_schema.Limits.default
  |> Result.map_error ~f:(fun error ->
    Error.Preparation (Inference_runtime.Preparation_error.Invalid_request error))
;;

let matches entry target =
  let configuration = entry.configuration in
  String.equal (R.Target.adapter target) "openai.responses"
  && String.equal (R.Target.profile target) (Profile.id configuration)
  && Option.equal
       String.equal
       (R.Target.account target)
       (D.Profile.account configuration.profile)
  && String.equal (R.Target.endpoint target) (D.Profile.endpoint configuration.profile)
  &&
  match R.Target.auth_binding target with
  | Absent | Null -> false
  | Value binding -> R.Auth_binding.equal binding configuration.binding
;;

let resolve t ~principal target =
  let open Result.Let_syntax in
  let%bind entry = admit t ~principal ~profile:(R.Target.profile target) in
  let%bind () =
    match R.Target.auth_binding target with
    | Absent | Null -> Error Error.Binding_unavailable
    | Value _ -> if matches entry target then Ok () else Error Error.Incompatible_identity
  in
  let%bind () = available t entry in
  let configuration_epoch = entry.revision_epoch in
  let auth ~sw _profile =
    let check_admission () =
      if
        entry.removed
        || entry.disabled
        || (not (matches entry target))
        || not (authorized t ~principal entry)
      then Error D.Auth.Denied
      else if entry.revision_epoch <> configuration_epoch
      then Error D.Auth.Profile_changed
      else (
        match available t entry with
        | Ok () -> Ok ()
        | Error Error.Reauthorization_required -> Error D.Auth.Reauthorization_required
        | Error _ -> Error D.Auth.Missing)
    in
    let open Result.Let_syntax in
    let%bind () = check_admission () in
    let captured = identity entry in
    let check_current () =
      let%bind () = check_admission () in
      if Credential_identity.equal captured (identity entry)
      then Ok ()
      else Error D.Auth.Denied
    in
    let%bind lease = t.credentials ~sw captured in
    let%bind () = check_current () in
    D.Auth.with_identity
      lease
      ~owner:captured.owner
      ~generation:captured.generation
      ~check_current
  in
  (* Revision is captured-default provenance. Compatibility was checked above;
     preparation uses today's capabilities but never today's setting defaults. *)
  let%bind adapter =
    A.create
      ~auth_binding:(R.Target.auth_binding target)
      t.driver
      ~profile:entry.configuration.profile
      ~profile_revision:(R.Target.profile_revision target)
      ~auth
      ~limits:t.limits
    |> Result.map_error ~f:(fun error -> Error.Preparation error)
  in
  Inference_runtime.Context.create adapter ~target
  |> Result.map_error ~f:(fun error -> Error.Preparation error)
;;

let resolver t ~principal target =
  resolve t ~principal target
  |> Result.map_error ~f:(function
    | Error.Preparation error -> error
    | Missing_profile | Binding_unavailable | Disabled ->
      Inference_runtime.Preparation_error.Target_unavailable
    | Denied -> Inference_runtime.Preparation_error.Target_denied
    | Incompatible_identity -> Inference_runtime.Preparation_error.Target_mismatch
    | Reauthorization_required ->
      Inference_runtime.Preparation_error.Reauthorization_required
    | Invalid_profile -> Inference_runtime.Preparation_error.Invalid_preparation)
;;
