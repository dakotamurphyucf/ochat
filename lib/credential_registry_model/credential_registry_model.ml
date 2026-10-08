open! Core

module Error = struct
  type t =
    | Invalid_identity
    | Invalid_grant
    | Invalid_document
    | Unsupported_schema
    | Missing_registry
    | Wrong_incarnation
    | Capacity
    | Epoch_exhausted
    | Stale_epoch
    | Stale_revision
    | Stale_operation
    | Disabled
    | Renewal_uncertain
    | Not_active
  [@@deriving equal, sexp_of]
end

module Id = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let create value =
    if
      String.is_empty value
      || String.length value > 48
      || not
           (String.for_all value ~f:(function
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
              | _ -> false))
    then Error Error.Invalid_identity
    else Ok value
  ;;

  let to_string t = t
end

module Epoch = struct
  type t = int64 [@@deriving compare, equal, sexp_of]

  let of_int64 value =
    if Int64.(value < zero) then Error Error.Invalid_document else Ok value
  ;;

  let to_int64 t = t

  let next t =
    if Int64.equal t Int64.max_value
    then Error Error.Epoch_exhausted
    else Ok (Int64.succ t)
  ;;
end

module Presence = struct
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving sexp_of]
end

let label ?(maximum = 256) value =
  (not (String.is_empty value))
  && String.length value <= maximum
  && String.for_all value ~f:(fun c ->
    let n = Char.to_int c in
    n >= 32 && n <> 127)
;;

let endpoint value =
  if not (label ~maximum:2048 value)
  then false
  else (
    let uri = Uri.of_string value in
    match Uri.scheme uri, Uri.host uri with
    | Some scheme, Some host ->
      (String.equal scheme "https" || String.equal scheme "http")
      && (not (String.is_empty host))
      && Option.is_none (Uri.userinfo uri)
      && Option.is_none (Uri.fragment uri)
      && List.is_empty (Uri.query uri)
    | _ -> false)
;;

let scopes values =
  List.length values <= 64
  && List.for_all values ~f:label
  && Option.is_none (List.find_a_dup values ~compare:String.compare)
;;

module Identity = struct
  type method_ =
    | Api_key of { key_reference : Id.t }
    | Oauth of
        { issuer : string
        ; client_registration : string
        ; resource : string
        ; verified_subject : string
        ; required_scopes : string list
        }
  [@@deriving equal, sexp_of]

  type t =
    { host : Id.t
    ; provider : string
    ; billing : string
    ; account : string option
    ; method_ : method_
    }
  [@@deriving equal, sexp_of]

  let common ~provider ~billing ~account =
    label provider && label billing && Option.for_all account ~f:label
  ;;

  let api_key ~host ~provider ~billing ~account ~key_reference =
    if not (common ~provider ~billing ~account)
    then Error Error.Invalid_identity
    else Ok { host; provider; billing; account; method_ = Api_key { key_reference } }
  ;;

  let oauth
        ~host
        ~provider
        ~billing
        ~issuer
        ~client_registration
        ~resource
        ~account
        ~verified_subject
        ~required_scopes
    =
    if
      (not (common ~provider ~billing ~account:(Some account)))
      || not
           (endpoint issuer
            && endpoint resource
            && label client_registration
            && label verified_subject
            && scopes required_scopes)
    then Error Error.Invalid_identity
    else
      Ok
        { host
        ; provider
        ; billing
        ; account = Some account
        ; method_ =
            Oauth
              { issuer
              ; client_registration
              ; resource
              ; verified_subject
              ; required_scopes = List.sort required_scopes ~compare:String.compare
              }
        }
  ;;

  let host t = t.host
  let provider t = t.provider
  let billing t = t.billing
  let account t = t.account
  let method_ t = t.method_
end

module Expectation = struct
  type t =
    | Exact of Identity.t
    | Acquisition of
        { host : Id.t
        ; provider : string
        ; billing : string
        ; issuer : string
        ; client_registration : string
        ; resource : string
        ; account : string option
        ; required_scopes : string list
        }

  let exact identity = Exact identity

  let oauth_acquisition
        ~host
        ~provider
        ~billing
        ~issuer
        ~client_registration
        ~resource
        ~account
        ~required_scopes
    =
    if
      (not (Identity.common ~provider ~billing ~account))
      || not
           (endpoint issuer
            && endpoint resource
            && label client_registration
            && scopes required_scopes)
    then Error Error.Invalid_identity
    else
      Ok
        (Acquisition
           { host
           ; provider
           ; billing
           ; issuer
           ; client_registration
           ; resource
           ; account
           ; required_scopes = List.sort required_scopes ~compare:String.compare
           })
  ;;

  let host = function
    | Exact identity -> Identity.host identity
    | Acquisition expectation -> expectation.host
  ;;

  let oauth_required_scopes = function
    | Acquisition expected -> Some expected.required_scopes
    | Exact identity ->
      (match Identity.method_ identity with
       | Oauth { required_scopes; _ } -> Some required_scopes
       | Api_key _ -> None)
  ;;

  let accepts t (identity : Identity.t) =
    match t, identity.method_ with
    | Exact expected, _ -> Identity.equal expected identity
    | Acquisition expected, Oauth actual ->
      Id.equal expected.host identity.host
      && String.equal expected.provider identity.provider
      && String.equal expected.billing identity.billing
      && String.equal expected.issuer actual.issuer
      && String.equal expected.client_registration actual.client_registration
      && String.equal expected.resource actual.resource
      && Option.for_all expected.account ~f:(fun account ->
        Option.equal String.equal (Some account) identity.account)
      && List.equal String.equal expected.required_scopes actual.required_scopes
    | Acquisition _, Api_key _ -> false
  ;;
end

module Grant = struct
  type refresh_policy =
    | Preserve_omitted
    | Require_rotated
  [@@deriving sexp_of]

  type provenance =
    | Declared
    | Qualified_prior_exact
    | Qualified_request
    | Qualified_token_claim
  [@@deriving equal, sexp_of]

  type expiry =
    | Unknown
    | Known of
        { at_ms : int64
        ; provenance : provenance
        }
  [@@deriving sexp_of]

  type unknown_expiry_policy =
    | Reject_unknown
    | Allow_qualified_unknown
  [@@deriving sexp_of]

  type effective =
    { scopes : string list
    ; scopes_provenance : provenance
    ; expiry : expiry
    ; unknown_expiry_policy : unknown_expiry_policy
    }
  [@@deriving sexp_of]

  type t =
    { identity : Identity.t
    ; scopes : string list Presence.t
    ; expires_at_ms : int64 Presence.t
    ; refresh_policy : refresh_policy
    ; effective : effective
    }
  [@@deriving sexp_of]

  let create
        ~identity
        ~scopes:granted
        ~expires_at_ms
        ~refresh_policy
        ~(effective : effective)
    =
    let effective_scopes = List.sort effective.scopes ~compare:String.compare in
    let valid_scopes =
      scopes effective.scopes
      && (match identity.Identity.method_ with
          | Oauth expected ->
            List.for_all
              expected.required_scopes
              ~f:(List.mem effective_scopes ~equal:String.equal)
          | Api_key _ -> false)
      &&
      match granted with
      | Presence.Value granted ->
        scopes granted
        && equal_provenance effective.scopes_provenance Declared
        && List.equal
             String.equal
             (List.sort granted ~compare:String.compare)
             effective_scopes
      | Absent -> not (equal_provenance effective.scopes_provenance Declared)
      | Null ->
        (not (equal_provenance effective.scopes_provenance Declared))
        && not (equal_provenance effective.scopes_provenance Qualified_token_claim)
    in
    let valid_expiry =
      (match effective.expiry with
       | Known value ->
         Int64.(value.at_ms >= zero)
         && not (equal_provenance value.provenance Qualified_token_claim)
       | Unknown -> true)
      &&
      match expires_at_ms, effective.expiry with
      | Presence.Value raw, Known value ->
        Int64.equal raw value.at_ms && equal_provenance value.provenance Declared
      | (Absent | Null), Known value -> not (equal_provenance value.provenance Declared)
      | (Absent | Null), Unknown -> true
      | Value _, Unknown -> false
    in
    if not (valid_scopes && valid_expiry)
    then Error Error.Invalid_grant
    else
      Ok
        { identity
        ; scopes = granted
        ; expires_at_ms
        ; refresh_policy
        ; effective = { effective with scopes = effective_scopes }
        }
  ;;

  let identity t = t.identity
  let scopes t = t.scopes
  let expires_at_ms t = t.expires_at_ms
  let refresh_policy t = t.refresh_policy
  let effective t = t.effective
end

module Active = struct
  type source =
    | Protected_revision of Id.t
    | Environment_reference of
        { name : string
        ; configuration_revision : Id.t option
        }
  [@@deriving sexp_of]

  type t =
    { identity : Identity.t
    ; epoch : Epoch.t
    ; source : source
    ; grant : Grant.t option
    ; raw : Jsonaf.t
    }
  [@@deriving sexp_of]

  let identity t = t.identity
  let epoch t = t.epoch
  let source t = t.source
  let grant t = t.grant
end

module Expected = struct
  type t =
    { epoch : Epoch.t
    ; revision : Id.t option
    ; operation : Id.t
    }
  [@@deriving sexp_of]

  let epoch t = t.epoch
  let revision t = t.revision
  let operation t = t.operation
end

module Snapshot = struct
  type refresh =
    | Idle
    | Possibly_sent of Id.t
    | Renewal_uncertain of Id.t
  [@@deriving sexp_of]

  type disabled_reason =
    | Logout
    | Key_removed
    | Environment_disabled
    | Renewal_rejected
  [@@deriving sexp_of]

  type t =
    { active : Active.t option
    ; epoch : Epoch.t
    ; disabled : disabled_reason option
    ; refresh : refresh
    }
  [@@deriving sexp_of]

  let active t = t.active
  let epoch t = t.epoch
  let disabled t = t.disabled
  let refresh t = t.refresh
end

module Removal = struct
  type drain =
    | Pending
    | Drained
  [@@deriving sexp_of]

  type revocation =
    | Not_requested
    | Unavailable
    | Possibly_sent
    | Revoked
  [@@deriving sexp_of]

  type t =
    { operation : Id.t
    ; drain : drain
    ; revocation : revocation
    ; raw : Jsonaf.t
    }

  let operation t = t.operation
  let drain t = t.drain
  let revocation t = t.revocation
end

module Cleanup = struct
  type deletion =
    | Pending
    | Removed_durability_unknown
    | Confirmed_removed
    | Quarantined
  [@@deriving sexp_of]

  type retired =
    { revision : Id.t
    ; owned_by : Id.t
    ; deletion : deletion
    ; raw : Jsonaf.t
    }

  let revision t = t.revision
  let owned_by t = t.owned_by
  let deletion t = t.deletion
end

module Operation = struct
  type result =
    | Pending
    | Committed
    | Rejected
    | Unavailable
  [@@deriving sexp_of]

  type t =
    { id : Id.t
    ; result : result
    ; raw : Jsonaf.t
    }

  let id t = t.id
  let result t = t.result
end

type candidate =
  { expected : Expected.t
  ; expectation : Expectation.t
  ; staged : Id.t option
  ; raw : Jsonaf.t
  }

type binding =
  { id : Id.t
  ; snapshot : Snapshot.t
  ; candidate : candidate option
  ; staged_refresh : Id.t option
  ; removal : Removal.t option
  ; revocation_history : Removal.t list
  ; retired : Cleanup.retired list
  ; operations : Operation.t list
  ; raw : Jsonaf.t
  }

type t =
  { incarnation : Id.t
  ; host : Id.t
  ; bindings : binding list
  ; document : Document_schema.Document.t
  }

let maximum_bindings = 128
let maximum_operations = 64
let maximum_retired = 64
let kind = "ochat.provider.credential_registry"
let version = 1

let limits =
  Document_schema.Limits.create
    ~max_bytes:(1024 * 1024)
    ~max_depth:64
    ~max_fields:20000
    ~max_nodes:50000
  |> function
  | Ok limits -> limits
  | Error _ -> failwith "credential registry fixed limits are invalid"
;;

(* Raw document fields stay attached to their owners. An absent nullable field
   remains absent when its semantic value is unchanged; known edits replace only
   their own member. Unknown required semantics are rejected at admission. *)
let overlay raw fields =
  match raw with
  | `Object original ->
    let updated =
      List.fold fields ~init:original ~f:(fun current (name, value) ->
        match List.Assoc.find original name ~equal:String.equal, value with
        | None, `Null -> current
        | _ -> List.Assoc.add current ~equal:String.equal name value)
    in
    `Object updated
  | _ -> `Object fields
;;

let rec preserve_members raw value =
  match raw, value with
  | `Object _, `Object values ->
    overlay
      raw
      (List.map values ~f:(fun (name, value) ->
         let previous =
           match Document_schema.Json.field raw ~name with
           | Value value -> value
           | Absent | Null -> `Null
         in
         name, preserve_members previous value))
  | _, value -> value
;;

let string value = `String value
let number value = `Number (Int64.to_string value)
let optional encode = Option.value_map ~default:`Null ~f:encode

let fields = function
  | `Object fields -> Ok fields
  | _ -> Error Error.Invalid_document
;;

let get fields name =
  Result.of_option
    (List.Assoc.find fields name ~equal:String.equal)
    ~error:Error.Invalid_document
;;

let as_string = function
  | `String value -> Ok value
  | _ -> Error Error.Invalid_document
;;

let as_id json = Result.bind (as_string json) ~f:Id.create

let as_int64 = function
  | `Number value ->
    (try Ok (Int64.of_string value) with
     | _ -> Error Error.Invalid_document)
  | _ -> Error Error.Invalid_document
;;

let decode_field fields name decode = Result.bind (get fields name) ~f:decode

let decode_optional fields name decode =
  match List.Assoc.find fields name ~equal:String.equal with
  | None | Some `Null -> Ok None
  | Some json -> Result.map (decode json) ~f:Option.some
;;

let decode_list decode = function
  | `Array values -> Result.all (List.map values ~f:decode)
  | _ -> Error Error.Invalid_document
;;

let identity_to_json (t : Identity.t) =
  let common =
    [ "host", string t.host
    ; "provider", string t.provider
    ; "billing", string t.billing
    ; "account", optional string t.account
    ]
  in
  `Object
    (common
     @
     match t.method_ with
     | Api_key { key_reference } ->
       [ "method", string "api_key"; "key_reference", string key_reference ]
     | Oauth grant ->
       [ "method", string "oauth_subscription"
       ; "issuer", string grant.issuer
       ; "client_registration", string grant.client_registration
       ; "resource", string grant.resource
       ; "verified_subject", string grant.verified_subject
       ; "required_scopes", `Array (List.map grant.required_scopes ~f:string)
       ])
;;

let identity_of_json json =
  let open Result.Let_syntax in
  let%bind fields = fields json in
  let%bind host = decode_field fields "host" as_id in
  let%bind provider = decode_field fields "provider" as_string in
  let%bind billing = decode_field fields "billing" as_string in
  let%bind account = decode_optional fields "account" as_string in
  let%bind method_ = decode_field fields "method" as_string in
  match method_ with
  | "api_key" ->
    let%bind key_reference = decode_field fields "key_reference" as_id in
    Identity.api_key ~host ~provider ~billing ~account ~key_reference
  | "oauth_subscription" ->
    let%bind issuer = decode_field fields "issuer" as_string in
    let%bind client_registration = decode_field fields "client_registration" as_string in
    let%bind resource = decode_field fields "resource" as_string in
    let%bind verified_subject = decode_field fields "verified_subject" as_string in
    let%bind required_scopes =
      decode_field fields "required_scopes" (decode_list as_string)
    in
    let%bind account = Result.of_option account ~error:Error.Invalid_identity in
    Identity.oauth
      ~host
      ~provider
      ~billing
      ~issuer
      ~client_registration
      ~resource
      ~account
      ~verified_subject
      ~required_scopes
  | _ -> Error Error.Invalid_identity
;;

let expectation_to_json = function
  | Expectation.Exact identity ->
    `Object [ "kind", string "exact"; "identity", identity_to_json identity ]
  | Acquisition value ->
    `Object
      [ "kind", string "acquisition"
      ; "host", string value.host
      ; "provider", string value.provider
      ; "billing", string value.billing
      ; "issuer", string value.issuer
      ; "client_registration", string value.client_registration
      ; "resource", string value.resource
      ; "account", optional string value.account
      ; "required_scopes", `Array (List.map value.required_scopes ~f:string)
      ]
;;

let expectation_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind kind = decode_field f "kind" as_string in
  match kind with
  | "exact" ->
    decode_field f "identity" identity_of_json |> Result.map ~f:Expectation.exact
  | "acquisition" ->
    let%bind host = decode_field f "host" as_id in
    let%bind provider = decode_field f "provider" as_string in
    let%bind billing = decode_field f "billing" as_string in
    let%bind issuer = decode_field f "issuer" as_string in
    let%bind client_registration = decode_field f "client_registration" as_string in
    let%bind resource = decode_field f "resource" as_string in
    let%bind account = decode_optional f "account" as_string in
    let%bind required_scopes = decode_field f "required_scopes" (decode_list as_string) in
    Expectation.oauth_acquisition
      ~host
      ~provider
      ~billing
      ~issuer
      ~client_registration
      ~resource
      ~account
      ~required_scopes
  | _ -> Error Error.Invalid_document
;;

let presence_field name encode = function
  | Presence.Absent -> []
  | Null -> [ name, `Null ]
  | Value value -> [ name, encode value ]
;;

let decode_presence f name decode =
  match List.Assoc.find f name ~equal:String.equal with
  | None -> Ok Presence.Absent
  | Some `Null -> Ok Presence.Null
  | Some value -> Result.map (decode value) ~f:(fun value -> Presence.Value value)
;;

let provenance_to_string = function
  | Grant.Declared -> "declared"
  | Qualified_prior_exact -> "qualified_prior_exact"
  | Qualified_request -> "qualified_request"
  | Qualified_token_claim -> "qualified_token_claim"
;;

let provenance_of_json json =
  Result.bind (as_string json) ~f:(function
    | "declared" -> Ok Grant.Declared
    | "qualified_prior_exact" -> Ok Grant.Qualified_prior_exact
    | "qualified_request" -> Ok Grant.Qualified_request
    | "qualified_token_claim" -> Ok Grant.Qualified_token_claim
    | _ -> Error Error.Invalid_grant)
;;

let effective_to_json (effective : Grant.effective) =
  `Object
    [ "scopes", `Array (List.map effective.scopes ~f:string)
    ; "scopes_provenance", string (provenance_to_string effective.scopes_provenance)
    ; ( "expiry"
      , match effective.expiry with
        | Unknown -> `Object [ "kind", string "unknown" ]
        | Known value ->
          `Object
            [ "kind", string "known"
            ; "at_ms", number value.at_ms
            ; "provenance", string (provenance_to_string value.provenance)
            ] )
    ; ( "unknown_expiry_policy"
      , string
          (match effective.unknown_expiry_policy with
           | Reject_unknown -> "reject_unknown"
           | Allow_qualified_unknown -> "allow_qualified_unknown") )
    ]
;;

let effective_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind scopes = decode_field f "scopes" (decode_list as_string) in
  let%bind scopes_provenance = decode_field f "scopes_provenance" provenance_of_json in
  let%bind expiry_json = get f "expiry" in
  let%bind expiry_fields = fields expiry_json in
  let%bind kind = decode_field expiry_fields "kind" as_string in
  let%bind expiry =
    match kind with
    | "unknown" -> Ok Grant.Unknown
    | "known" ->
      let%bind at_ms = decode_field expiry_fields "at_ms" as_int64 in
      let%map provenance = decode_field expiry_fields "provenance" provenance_of_json in
      Grant.Known { at_ms; provenance }
    | _ -> Error Error.Invalid_grant
  in
  let%bind policy = decode_field f "unknown_expiry_policy" as_string in
  let%map unknown_expiry_policy =
    match policy with
    | "reject_unknown" -> Ok Grant.Reject_unknown
    | "allow_qualified_unknown" -> Ok Grant.Allow_qualified_unknown
    | _ -> Error Error.Invalid_grant
  in
  { Grant.scopes; scopes_provenance; expiry; unknown_expiry_policy }
;;

let grant_to_json ?(raw = `Object []) (grant : Grant.t) =
  let current =
    [ ( "identity"
      , preserve_members
          (match Document_schema.Json.field raw ~name:"identity" with
           | Value value -> value
           | Absent | Null -> `Object [])
          (identity_to_json grant.identity) )
    ; ( "effective"
      , preserve_members
          (match Document_schema.Json.field raw ~name:"effective" with
           | Value value -> value
           | Absent | Null -> `Object [])
          (effective_to_json grant.effective) )
    ; ( "refresh_policy"
      , string
          (match grant.refresh_policy with
           | Preserve_omitted -> "preserve_omitted"
           | Require_rotated -> "require_rotated") )
    ]
    @ presence_field
        "scopes"
        (fun values -> `Array (List.map values ~f:string))
        grant.scopes
    @ presence_field "expires_at_ms" number grant.expires_at_ms
  in
  let absent =
    (match grant.scopes with
     | Presence.Absent -> [ "scopes" ]
     | Null | Value _ -> [])
    @
    match grant.expires_at_ms with
    | Presence.Absent -> [ "expires_at_ms" ]
    | Null | Value _ -> []
  in
  let raw =
    match raw with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (List.mem absent name ~equal:String.equal)))
    | _ -> raw
  in
  overlay raw current
;;

let grant_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind identity = decode_field f "identity" identity_of_json in
  let%bind scopes = decode_presence f "scopes" (decode_list as_string) in
  let%bind expires_at_ms = decode_presence f "expires_at_ms" as_int64 in
  let%bind policy = decode_field f "refresh_policy" as_string in
  let%bind refresh_policy =
    match policy with
    | "preserve_omitted" -> Ok Grant.Preserve_omitted
    | "require_rotated" -> Ok Grant.Require_rotated
    | _ -> Error Error.Invalid_grant
  in
  let%bind effective = decode_field f "effective" effective_of_json in
  Grant.create ~identity ~scopes ~expires_at_ms ~refresh_policy ~effective
;;

let source_to_json = function
  | Active.Protected_revision revision ->
    `Object [ "kind", string "protected_revision"; "revision", string revision ]
  | Environment_reference { name; configuration_revision } ->
    `Object
      [ "kind", string "environment_reference"
      ; "name", string name
      ; "configuration_revision", optional string configuration_revision
      ]
;;

let environment_name name =
  String.length name <= 256
  && (not (String.is_empty name))
  && String.for_alli name ~f:(fun index c ->
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | '0' .. '9' -> index > 0
    | _ -> false)
;;

let source_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind kind = decode_field f "kind" as_string in
  match kind with
  | "protected_revision" ->
    decode_field f "revision" as_id
    |> Result.map ~f:(fun id -> Active.Protected_revision id)
  | "environment_reference" ->
    let%bind name = decode_field f "name" as_string in
    let%bind configuration_revision = decode_optional f "configuration_revision" as_id in
    if environment_name name
    then Ok (Active.Environment_reference { name; configuration_revision })
    else Error Error.Invalid_identity
  | _ -> Error Error.Invalid_document
;;

let raw_field raw name =
  match Document_schema.Json.field raw ~name with
  | Value value -> value
  | Absent | Null -> `Object []
;;

let active_to_json (active : Active.t) =
  overlay
    active.raw
    [ ( "identity"
      , overlay
          (raw_field active.raw "identity")
          (match identity_to_json active.identity with
           | `Object fields -> fields
           | _ -> assert false) )
    ; "epoch", number active.epoch
    ; ( "source"
      , overlay
          (raw_field active.raw "source")
          (match source_to_json active.source with
           | `Object fields -> fields
           | _ -> assert false) )
    ; "grant", optional (grant_to_json ~raw:(raw_field active.raw "grant")) active.grant
    ]
;;

let active_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind identity = decode_field f "identity" identity_of_json in
  let%bind epoch =
    decode_field f "epoch" (fun json -> Result.bind (as_int64 json) ~f:Epoch.of_int64)
  in
  let%bind source = decode_field f "source" source_of_json in
  let%bind grant = decode_optional f "grant" grant_of_json in
  let valid =
    match identity.Identity.method_, source, grant with
    | Api_key _, Protected_revision _, None -> true
    | Api_key _, Environment_reference { name; _ }, None -> environment_name name
    | Oauth _, Protected_revision _, Some grant ->
      Identity.equal identity (Grant.identity grant)
    | _ -> false
  in
  if valid
  then Ok { Active.identity; epoch; source; grant; raw = json }
  else Error Error.Invalid_grant
;;

let expected_to_json (expected : Expected.t) =
  `Object
    [ "epoch", number expected.epoch
    ; "revision", optional string expected.revision
    ; "operation", string expected.operation
    ]
;;

let expected_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind epoch =
    decode_field f "epoch" (fun json -> Result.bind (as_int64 json) ~f:Epoch.of_int64)
  in
  let%bind revision = decode_optional f "revision" as_id in
  let%map operation = decode_field f "operation" as_id in
  { Expected.epoch; revision; operation }
;;

let candidate_to_json (candidate : candidate) =
  overlay
    candidate.raw
    [ ( "expected"
      , preserve_members
          (raw_field candidate.raw "expected")
          (expected_to_json candidate.expected) )
    ; ( "expectation"
      , preserve_members
          (raw_field candidate.raw "expectation")
          (expectation_to_json candidate.expectation) )
    ; "staged", optional string candidate.staged
    ]
;;

let candidate_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind expected = decode_field f "expected" expected_of_json in
  let%bind expectation = decode_field f "expectation" expectation_of_json in
  let%map staged = decode_optional f "staged" as_id in
  { expected; expectation; staged; raw = json }
;;

let operation_result_to_string = function
  | Operation.Pending -> "pending"
  | Committed -> "committed"
  | Rejected -> "rejected"
  | Unavailable -> "unavailable"
;;

let operation_to_json (operation : Operation.t) =
  overlay
    operation.raw
    [ "id", string operation.id
    ; "result", string (operation_result_to_string operation.result)
    ]
;;

let operation_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind id = decode_field f "id" as_id in
  let%bind value = decode_field f "result" as_string in
  let%map result =
    match value with
    | "pending" -> Ok Operation.Pending
    | "committed" -> Ok Operation.Committed
    | "rejected" -> Ok Operation.Rejected
    | "unavailable" -> Ok Operation.Unavailable
    | _ -> Error Error.Invalid_document
  in
  { Operation.id; result; raw = json }
;;

let deletion_to_string = function
  | Cleanup.Pending -> "pending"
  | Removed_durability_unknown -> "removed_durability_unknown"
  | Confirmed_removed -> "confirmed_removed"
  | Quarantined -> "quarantined"
;;

let retired_to_json (retired : Cleanup.retired) =
  overlay
    retired.raw
    [ "revision", string retired.revision
    ; "owned_by", string retired.owned_by
    ; "deletion", string (deletion_to_string retired.deletion)
    ]
;;

let retired_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind revision = decode_field f "revision" as_id in
  let%bind owned_by = decode_field f "owned_by" as_id in
  let%bind value = decode_field f "deletion" as_string in
  let%map deletion =
    match value with
    | "pending" -> Ok Cleanup.Pending
    | "removed_durability_unknown" -> Ok Cleanup.Removed_durability_unknown
    | "confirmed_removed" -> Ok Cleanup.Confirmed_removed
    | "quarantined" -> Ok Cleanup.Quarantined
    | _ -> Error Error.Invalid_document
  in
  { Cleanup.revision; owned_by; deletion; raw = json }
;;

let disabled_to_string = function
  | Snapshot.Logout -> "logout"
  | Key_removed -> "key_removed"
  | Environment_disabled -> "environment_disabled"
  | Renewal_rejected -> "renewal_rejected"
;;

let disabled_of_json json =
  Result.bind (as_string json) ~f:(function
    | "logout" -> Ok Snapshot.Logout
    | "key_removed" -> Ok Snapshot.Key_removed
    | "environment_disabled" -> Ok Snapshot.Environment_disabled
    | "renewal_rejected" -> Ok Snapshot.Renewal_rejected
    | _ -> Error Error.Invalid_document)
;;

let refresh_to_json = function
  | Snapshot.Idle -> `Object [ "kind", string "idle" ]
  | Possibly_sent operation ->
    `Object [ "kind", string "possibly_sent"; "operation", string operation ]
  | Renewal_uncertain operation ->
    `Object [ "kind", string "renewal_uncertain"; "operation", string operation ]
;;

let refresh_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind kind = decode_field f "kind" as_string in
  match kind with
  | "idle" -> Ok Snapshot.Idle
  | "possibly_sent" | "renewal_uncertain" ->
    let%map operation = decode_field f "operation" as_id in
    if String.equal kind "possibly_sent"
    then Snapshot.Possibly_sent operation
    else Renewal_uncertain operation
  | _ -> Error Error.Invalid_document
;;

let removal_to_json (t : Removal.t) =
  overlay
    t.raw
    [ "operation", string t.operation
    ; ( "drain"
      , string
          (match t.drain with
           | Pending -> "pending"
           | Drained -> "drained") )
    ; ( "revocation"
      , string
          (match t.revocation with
           | Not_requested -> "not_requested"
           | Unavailable -> "unavailable"
           | Possibly_sent -> "possibly_sent"
           | Revoked -> "revoked") )
    ]
;;

let removal_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind operation = decode_field f "operation" as_id in
  let%bind drain =
    decode_field f "drain" (fun v ->
      Result.bind (as_string v) ~f:(function
        | "pending" -> Ok Removal.Pending
        | "drained" -> Ok Removal.Drained
        | _ -> Error Error.Invalid_document))
  in
  let%map revocation =
    decode_field f "revocation" (fun v ->
      Result.bind (as_string v) ~f:(function
        | "not_requested" -> Ok Removal.Not_requested
        | "unavailable" -> Ok Removal.Unavailable
        | "possibly_sent" -> Ok Removal.Possibly_sent
        | "revoked" -> Ok Removal.Revoked
        | _ -> Error Error.Invalid_document))
  in
  { Removal.operation; drain; revocation; raw = json }
;;

let binding_to_json (binding : binding) =
  overlay
    binding.raw
    [ "id", string binding.id
    ; "epoch", number binding.snapshot.epoch
    ; "active", optional active_to_json binding.snapshot.active
    ; ( "disabled"
      , optional
          (fun value -> string (disabled_to_string value))
          binding.snapshot.disabled )
    ; ( "refresh"
      , overlay
          (raw_field binding.raw "refresh")
          (match refresh_to_json binding.snapshot.refresh with
           | `Object fields -> fields
           | _ -> assert false) )
    ; "candidate", optional candidate_to_json binding.candidate
    ; "staged_refresh", optional string binding.staged_refresh
    ; "removal", optional removal_to_json binding.removal
    ; ( "revocation_history"
      , `Array (List.map binding.revocation_history ~f:removal_to_json) )
    ; "retired", `Array (List.map binding.retired ~f:retired_to_json)
    ; "operations", `Array (List.map binding.operations ~f:operation_to_json)
    ]
;;

let occupied_revision_count ~active_revision ~candidate ~refresh ~retired =
  let refresh_operation =
    match refresh with
    | Snapshot.Idle -> None
    | Possibly_sent operation | Renewal_uncertain operation -> Some operation
  in
  List.filter_opt
    [ active_revision
    ; Option.map candidate ~f:(fun candidate -> candidate.expected.operation)
    ; refresh_operation
    ]
  @ List.map retired ~f:Cleanup.revision
  |> List.dedup_and_sort ~compare:Id.compare
  |> List.length
;;

let binding_of_json json =
  let open Result.Let_syntax in
  let%bind f = fields json in
  let%bind id = decode_field f "id" as_id in
  let%bind epoch =
    decode_field f "epoch" (fun json -> Result.bind (as_int64 json) ~f:Epoch.of_int64)
  in
  let%bind active = decode_optional f "active" active_of_json in
  let%bind disabled = decode_optional f "disabled" disabled_of_json in
  let%bind refresh = decode_field f "refresh" refresh_of_json in
  let%bind candidate = decode_optional f "candidate" candidate_of_json in
  let%bind staged_refresh = decode_optional f "staged_refresh" as_id in
  let%bind removal = decode_optional f "removal" removal_of_json in
  let%bind revocation_history =
    match List.Assoc.find f "revocation_history" ~equal:String.equal with
    | None -> Ok []
    | Some value -> decode_list removal_of_json value
  in
  let%bind retired = decode_field f "retired" (decode_list retired_of_json) in
  let%bind operations = decode_field f "operations" (decode_list operation_of_json) in
  let active_revision =
    Option.bind active ~f:(fun active ->
      match active.Active.source with
      | Protected_revision revision -> Some revision
      | Environment_reference _ -> None)
  in
  let pending operation =
    List.exists operations ~f:(fun item ->
      Id.equal operation item.Operation.id
      &&
      match item.result with
      | Pending -> true
      | Committed | Rejected | Unavailable -> false)
  in
  let candidate_valid =
    Option.for_all candidate ~f:(fun candidate ->
      Epoch.equal candidate.expected.epoch epoch
      && Option.equal Id.equal candidate.expected.revision active_revision
      && pending candidate.expected.operation
      && Option.for_all candidate.staged ~f:(Id.equal candidate.expected.operation))
  in
  let refresh_valid =
    match refresh with
    | Snapshot.Idle -> Option.is_none staged_refresh
    | Possibly_sent operation | Renewal_uncertain operation ->
      Option.is_none disabled
      && Option.exists active ~f:(fun active ->
        match active.Active.identity.method_, active.grant with
        | Identity.Oauth _, Some _ -> true
        | _ -> false)
      && pending operation
      && Option.for_all staged_refresh ~f:(Id.equal operation)
  in
  if
    (not (candidate_valid && refresh_valid))
    || List.exists retired ~f:(fun item ->
      (not (Id.equal item.Cleanup.revision item.owned_by))
      || Option.equal Id.equal active_revision (Some item.revision))
    || occupied_revision_count ~active_revision ~candidate ~refresh ~retired
       > maximum_retired
    || List.length operations > maximum_operations
    || Option.is_some
         (List.find_a_dup (List.map operations ~f:Operation.id) ~compare:Id.compare)
    || Option.is_some
         (List.find_a_dup (List.map retired ~f:Cleanup.revision) ~compare:Id.compare)
    || List.length revocation_history > 16
    || (Option.is_some active && List.length revocation_history >= 16)
    || Option.is_some
         (List.find_a_dup
            (List.map revocation_history ~f:Removal.operation)
            ~compare:Id.compare)
    || List.exists revocation_history ~f:(fun record ->
      match record.Removal.revocation with
      | Possibly_sent -> false
      | _ -> true)
    || Option.exists removal ~f:(fun removal ->
      (Option.is_none disabled
       &&
       match removal.Removal.drain with
       | Pending -> true
       | Drained -> false)
      || not
           (List.exists operations ~f:(fun op ->
              Id.equal op.Operation.id removal.Removal.operation
              &&
              match op.result with
              | Committed -> true
              | _ -> false)))
    || (Option.is_some disabled && Option.is_some active)
    || Option.exists active ~f:(fun active -> not (Epoch.equal epoch active.Active.epoch))
  then Error Error.Invalid_document
  else
    Ok
      { id
      ; snapshot = { Snapshot.active; epoch; disabled; refresh }
      ; candidate
      ; staged_refresh
      ; removal
      ; revocation_history
      ; retired
      ; operations
      ; raw = json
      }
;;

let revision_reservations (binding : binding) =
  List.filter_opt
    [ Option.bind binding.snapshot.active ~f:(fun active ->
        match active.Active.source with
        | Protected_revision revision -> Some revision
        | Environment_reference _ -> None)
    ; Option.map binding.candidate ~f:(fun candidate -> candidate.expected.operation)
    ; (match binding.snapshot.refresh with
       | Idle -> None
       | Possibly_sent operation -> Some operation
       | Renewal_uncertain operation ->
         if
           List.exists binding.retired ~f:(fun retired ->
             Id.equal retired.Cleanup.revision operation
             &&
             match retired.deletion with
             | Quarantined -> true
             | _ -> false)
         then None
         else Some operation)
    ]
  @ List.map binding.retired ~f:Cleanup.revision
;;

let globally_unique_revisions bindings =
  let revisions = List.concat_map bindings ~f:revision_reservations in
  Option.is_none (List.find_a_dup revisions ~compare:Id.compare)
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind () =
    Document_schema.Document.validate document ~limits
    |> Result.map_error ~f:(fun _ -> Error.Invalid_document)
  in
  if
    (not (String.equal (Document_schema.Document.kind document) kind))
    || Document_schema.Document.version document <> version
    || not (List.is_empty (Document_schema.Document.required_semantics document))
  then Error Error.Unsupported_schema
  else (
    let%bind f = fields (Document_schema.Document.payload document) in
    let%bind incarnation = decode_field f "incarnation" as_id in
    let%bind host = decode_field f "host" as_id in
    let%bind bindings = decode_field f "bindings" (decode_list binding_of_json) in
    if
      (not (globally_unique_revisions bindings))
      || List.length bindings > maximum_bindings
      || Option.is_some
           (List.find_a_dup
              (List.map bindings ~f:(fun binding -> binding.id))
              ~compare:Id.compare)
      || List.exists bindings ~f:(fun binding ->
        Option.exists binding.candidate ~f:(fun candidate ->
          not (Id.equal host (Expectation.host candidate.expectation))))
      || List.exists bindings ~f:(fun binding ->
        Option.exists binding.snapshot.active ~f:(fun active ->
          not (Id.equal host (Identity.host active.identity))))
    then Error Error.Invalid_document
    else Ok { incarnation; host; bindings; document })
;;

let to_document t =
  let payload =
    overlay
      (Document_schema.Document.payload t.document)
      [ "incarnation", string t.incarnation
      ; "host", string t.host
      ; "bindings", `Array (List.map t.bindings ~f:binding_to_json)
      ]
  in
  let json = overlay (Document_schema.Document.json t.document) [ "payload", payload ] in
  Document_schema.Document.inspect ~limits json
  |> Result.map_error ~f:(fun _ -> Error.Capacity)
;;

let initialize ~incarnation ~host =
  Document_schema.Document.create
    ~limits
    ~kind
    ~version
    ~payload:
      (`Object
          [ "incarnation", string incarnation
          ; "host", string host
          ; "bindings", `Array []
          ])
  |> Result.map_error ~f:(fun _ -> Error.Invalid_document)
  |> Result.bind ~f:of_document
;;

let incarnation t = t.incarnation
let host t = t.host
let binding_ids t = List.map t.bindings ~f:(fun binding -> binding.id)

let binding t id =
  Result.of_option
    (List.find t.bindings ~f:(fun binding -> Id.equal binding.id id))
    ~error:Error.Not_active
;;

let find t ~binding:id = Result.map (binding t id) ~f:(fun binding -> binding.snapshot)
let retired t ~binding:id = Result.map (binding t id) ~f:(fun binding -> binding.retired)

let operation t ~binding:id ~operation =
  Result.bind (binding t id) ~f:(fun binding ->
    Result.of_option
      (List.find binding.operations ~f:(fun item -> Id.equal operation item.Operation.id))
      ~error:Error.Stale_operation)
;;

let active_revision (snapshot : Snapshot.t) =
  Option.bind snapshot.active ~f:(fun active ->
    match active.Active.source with
    | Protected_revision revision -> Some revision
    | Environment_reference _ -> None)
;;

let expected_for (binding : binding) operation =
  { Expected.epoch = binding.snapshot.epoch
  ; revision = active_revision binding.snapshot
  ; operation
  }
;;

let check_expected (binding : binding) (expected : Expected.t) =
  if not (Epoch.equal binding.snapshot.epoch expected.epoch)
  then Error Error.Stale_epoch
  else if not (Option.equal Id.equal (active_revision binding.snapshot) expected.revision)
  then Error Error.Stale_revision
  else Ok ()
;;

let finish t (updated : binding) =
  let occupied =
    occupied_revision_count
      ~active_revision:(active_revision updated.snapshot)
      ~candidate:updated.candidate
      ~refresh:updated.snapshot.refresh
      ~retired:updated.retired
  in
  if occupied > maximum_retired
  then Error Error.Capacity
  else (
    let bindings =
      if List.exists t.bindings ~f:(fun binding -> Id.equal binding.id updated.id)
      then
        List.map t.bindings ~f:(fun binding ->
          if Id.equal binding.id updated.id then updated else binding)
      else updated :: t.bindings
    in
    if not (globally_unique_revisions bindings)
    then Error Error.Stale_revision
    else (
      let updated = { t with bindings } in
      Result.bind (to_document updated) ~f:of_document))
;;

let fresh_binding id =
  { id
  ; snapshot = { Snapshot.active = None; epoch = 0L; disabled = None; refresh = Idle }
  ; candidate = None
  ; staged_refresh = None
  ; retired = []
  ; removal = None
  ; revocation_history = []
  ; operations = []
  ; raw = `Object []
  }
;;

let begin_operation (binding : binding) id =
  if List.exists binding.operations ~f:(fun item -> Id.equal item.Operation.id id)
  then Error Error.Stale_operation
  else (
    let operations =
      if List.length binding.operations < maximum_operations
      then binding.operations
      else (
        (* Only terminal receipts may age out. Missing historical proof remains
           unavailable; unresolved admission is never silently forgotten. *)
        let rec remove_last_terminal = function
          | [] -> None
          | (head : Operation.t) :: tail ->
            (match remove_last_terminal tail with
             | Some tail -> Some (head :: tail)
             | None ->
               (match head.result with
                | Pending -> None
                | Committed | Rejected | Unavailable ->
                  let reserved =
                    Option.exists binding.removal ~f:(fun removal ->
                      Id.equal removal.Removal.operation head.id)
                    || List.exists binding.revocation_history ~f:(fun removal ->
                      Id.equal removal.Removal.operation head.id)
                  in
                  if reserved then None else Some tail))
        in
        Option.value (remove_last_terminal binding.operations) ~default:binding.operations)
    in
    if List.length operations >= maximum_operations
    then Error Error.Capacity
    else
      Ok
        { binding with
          operations = { Operation.id; result = Pending; raw = `Object [] } :: operations
        })
;;

let terminal (binding : binding) operation result =
  { binding with
    operations =
      List.map binding.operations ~f:(fun item ->
        if Id.equal item.Operation.id operation then { item with result } else item)
  }
;;

let retire (binding : binding) ~revision ~owned_by =
  if
    List.exists binding.retired ~f:(fun retired ->
      Id.equal retired.Cleanup.revision revision)
  then Ok binding
  else if List.length binding.retired >= maximum_retired
  then Error Error.Capacity
  else
    Ok
      { binding with
        retired =
          { Cleanup.revision; owned_by; deletion = Pending; raw = `Object [] }
          :: binding.retired
      }
;;

let begin_candidate t ~binding:id ~operation ~expectation =
  let open Result.Let_syntax in
  let%bind current =
    match List.find t.bindings ~f:(fun binding -> Id.equal binding.id id) with
    | Some current -> Ok current
    | None ->
      if List.length t.bindings >= maximum_bindings
      then Error Error.Capacity
      else Ok (fresh_binding id)
  in
  if not (Id.equal t.host (Expectation.host expectation))
  then Error Error.Invalid_identity
  else if Option.is_some current.candidate
  then Error Error.Stale_operation
  else (
    let%bind () =
      let pending =
        List.length current.revocation_history
        +
        if
          Option.exists current.removal ~f:(fun r ->
            match r.Removal.revocation with
            | Possibly_sent -> true
            | _ -> false)
        then 1
        else 0
      in
      if pending >= 16 then Error Error.Capacity else Ok ()
    in
    let%bind current = begin_operation current operation in
    let expected = expected_for current operation in
    let candidate = { expected; expectation; staged = None; raw = `Object [] } in
    let%map t = finish t { current with candidate = Some candidate } in
    t, expected)
;;

let current_candidate current expected =
  let open Result.Let_syntax in
  let%bind () = check_expected current expected in
  match current.candidate with
  | Some candidate when Id.equal candidate.expected.operation expected.Expected.operation
    -> Ok candidate
  | Some _ | None -> Error Error.Stale_operation
;;

let discard_candidate t ~binding:id ~expected =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind candidate = current_candidate current expected in
  let%bind current =
    match candidate.staged with
    | None -> Ok current
    | Some revision -> retire current ~revision ~owned_by:expected.operation
  in
  finish
    t
    (terminal { current with candidate = None } expected.operation Operation.Rejected)
;;

let stage_candidate t ~binding:id ~expected ~revision =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind candidate = current_candidate current expected in
  if Option.is_some candidate.staged || not (Id.equal revision expected.operation)
  then Error Error.Stale_operation
  else if List.length current.retired >= maximum_retired
  then Error Error.Capacity
  else
    finish t { current with candidate = Some { candidate with staged = Some revision } }
;;

let valid_active identity source grant epoch =
  active_of_json
    (active_to_json { Active.identity; source; grant; epoch; raw = `Object [] })
;;

let commit_candidate t ~binding:id ~expected ~identity ~source ~grant =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind candidate = current_candidate current expected in
  let staged_matches =
    match source with
    | Active.Protected_revision revision ->
      Option.equal Id.equal candidate.staged (Some revision)
    | Environment_reference _ -> Option.is_none candidate.staged
  in
  if
    Option.exists current.removal ~f:(fun removal ->
      match removal.Removal.drain with
      | Pending -> true
      | Drained -> not (List.is_empty current.retired))
  then Error Error.Stale_operation
  else if
    not
      (Id.equal t.host (Identity.host identity)
       && Expectation.accepts candidate.expectation identity
       && staged_matches)
  then Error Error.Invalid_identity
  else (
    let%bind epoch = Epoch.next current.snapshot.epoch in
    let%bind active = valid_active identity source grant epoch in
    let%bind current =
      match active_revision current.snapshot with
      | None -> Ok current
      | Some revision -> retire current ~revision ~owned_by:revision
    in
    let%bind current =
      match current.staged_refresh with
      | None -> Ok current
      | Some revision -> retire current ~revision ~owned_by:revision
    in
    let current =
      match current.snapshot.refresh with
      | Idle -> current
      | Possibly_sent operation | Renewal_uncertain operation ->
        terminal current operation Operation.Rejected
    in
    let snapshot =
      { Snapshot.active = Some active; epoch; disabled = None; refresh = Idle }
    in
    finish
      t
      (terminal
         { current with snapshot; candidate = None; staged_refresh = None }
         expected.operation
         Operation.Committed))
;;

let begin_refresh t ~binding:id ~operation =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  if Option.is_some current.snapshot.disabled
  then Error Error.Disabled
  else (
    match current.snapshot.active, current.snapshot.refresh with
    | None, _ -> Error Error.Not_active
    | _, (Possibly_sent _ | Renewal_uncertain _) -> Error Error.Renewal_uncertain
    | Some active, Idle ->
      (match active.Active.source, active.identity.method_ with
       | Protected_revision _, Identity.Oauth _ ->
         let%bind current = begin_operation current operation in
         let expected = expected_for current operation in
         let%map t =
           finish
             t
             { current with
               snapshot = { current.snapshot with refresh = Possibly_sent operation }
             }
         in
         t, expected
       | _ -> Error Error.Invalid_grant))
;;

let check_refresh current expected =
  let open Result.Let_syntax in
  let%bind () = check_expected current expected in
  if Option.is_some current.snapshot.disabled
  then Error Error.Disabled
  else (
    match current.snapshot.refresh with
    | (Snapshot.Possibly_sent operation | Renewal_uncertain operation)
      when Id.equal operation expected.Expected.operation -> Ok ()
    | Idle | Possibly_sent _ | Renewal_uncertain _ -> Error Error.Stale_operation)
;;

let clear_definitely_unsent_refresh t ~binding:id ~expected =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind () = check_refresh current expected in
  if Option.is_some current.staged_refresh
  then Error Error.Stale_operation
  else
    finish
      t
      (terminal
         { current with snapshot = { current.snapshot with refresh = Idle } }
         expected.operation
         Operation.Rejected)
;;

let mark_renewal_uncertain t ~binding:id ~expected =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind () = check_refresh current expected in
  finish
    t
    { current with
      snapshot = { current.snapshot with refresh = Renewal_uncertain expected.operation }
    }
;;

let stage_refresh t ~binding:id ~expected ~revision =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind () = check_refresh current expected in
  if Option.is_some current.staged_refresh || not (Id.equal revision expected.operation)
  then Error Error.Stale_operation
  else if List.length current.retired >= maximum_retired
  then Error Error.Capacity
  else finish t { current with staged_refresh = Some revision }
;;

let commit_refresh t ~binding:id ~expected ~revision ~grant =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind () = check_refresh current expected in
  let%bind active = Result.of_option current.snapshot.active ~error:Error.Not_active in
  if
    (not (Option.equal Id.equal current.staged_refresh (Some revision)))
    || not (Identity.equal active.identity (Grant.identity grant))
  then Error Error.Invalid_grant
  else (
    let%bind current =
      match active_revision current.snapshot with
      | None -> Error Error.Not_active
      | Some old -> retire current ~revision:old ~owned_by:old
    in
    let active =
      { active with source = Protected_revision revision; grant = Some grant }
    in
    let snapshot = { current.snapshot with active = Some active; refresh = Idle } in
    finish
      t
      (terminal
         { current with snapshot; staged_refresh = None }
         expected.operation
         Operation.Committed))
;;

let disable t ~binding:id ~operation ~reason =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let current =
    match current.removal with
    | Some removal
      when match removal.Removal.revocation with
           | Possibly_sent -> true
           | _ -> false ->
      { current with revocation_history = removal :: current.revocation_history }
    | Some _ | None -> current
  in
  let%bind current = begin_operation current operation in
  let%bind epoch = Epoch.next current.snapshot.epoch in
  let revisions =
    List.filter_opt
      [ active_revision current.snapshot
      ; Option.bind current.candidate ~f:(fun candidate -> candidate.staged)
      ; current.staged_refresh
      ]
    |> List.dedup_and_sort ~compare:Id.compare
  in
  let%bind current =
    List.fold revisions ~init:(Ok current) ~f:(fun acc revision ->
      Result.bind acc ~f:(fun current -> retire current ~revision ~owned_by:revision))
  in
  let operations =
    List.map current.operations ~f:(fun item ->
      if
        Operation.(
          match item.result with
          | Pending -> true
          | _ -> false)
      then { item with result = Operation.Rejected }
      else item)
  in
  let snapshot =
    { Snapshot.active = None; epoch; disabled = Some reason; refresh = Idle }
  in
  finish
    t
    (terminal
       { current with
         snapshot
       ; candidate = None
       ; staged_refresh = None
       ; operations
       ; removal =
           Some
             { Removal.operation
             ; drain = Pending
             ; revocation = Not_requested
             ; raw = `Object []
             }
       }
       operation
       Operation.Committed)
;;

let record_cleanup t ~binding:id ~operation ~revision ~deletion =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let live =
    List.exists t.bindings ~f:(fun binding ->
      Option.equal Id.equal (active_revision binding.snapshot) (Some revision)
      || Option.exists binding.candidate ~f:(fun candidate ->
        Option.equal Id.equal candidate.staged (Some revision))
      || Option.equal Id.equal binding.staged_refresh (Some revision))
  in
  if live
  then Error Error.Stale_revision
  else (
    match
      List.find current.retired ~f:(fun item -> Id.equal item.Cleanup.revision revision)
    with
    | None -> Error Error.Stale_operation
    | Some item when not (Id.equal item.owned_by operation) -> Error Error.Stale_operation
    | Some _ ->
      let retired =
        match deletion with
        | Cleanup.Confirmed_removed ->
          List.filter current.retired ~f:(fun item ->
            not (Id.equal item.Cleanup.revision revision))
        | Pending | Removed_durability_unknown | Quarantined ->
          List.map current.retired ~f:(fun item ->
            if Id.equal item.Cleanup.revision revision
            then { item with deletion }
            else item)
      in
      finish t { current with retired })
;;

let removal t ~binding:id = Result.map (binding t id) ~f:(fun binding -> binding.removal)

let record_removal
      t
      ~binding:id
      ~operation
      ~(drain : Removal.drain)
      ~(revocation : Removal.revocation)
  =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  match current.removal with
  | None -> Error Error.Stale_operation
  | Some removal when not (Id.equal removal.operation operation) ->
    Error Error.Stale_operation
  | Some removal ->
    let drain_valid =
      match removal.drain, drain with
      | Drained, Pending -> false
      | _ -> true
    in
    let revocation_valid =
      match removal.revocation, revocation with
      | Revoked, Revoked
      | Possibly_sent, (Possibly_sent | Revoked | Unavailable)
      | (Not_requested | Unavailable), _ -> true
      | _ -> false
    in
    if not (drain_valid && revocation_valid)
    then Error Error.Stale_operation
    else finish t { current with removal = Some { removal with drain; revocation } }
;;

let candidate_pending t ~binding:id =
  Result.map (binding t id) ~f:(fun binding -> Option.is_some binding.candidate)
;;

let revocation_history t ~binding:id =
  Result.map (binding t id) ~f:(fun binding -> binding.revocation_history)
;;

let quarantine_refresh t ~binding:id ~expected =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  let%bind () = check_refresh current expected in
  let%bind revision =
    Result.of_option current.staged_refresh ~error:Error.Stale_operation
  in
  let%bind current = retire current ~revision ~owned_by:expected.operation in
  let retired =
    List.map current.retired ~f:(fun item ->
      if Id.equal item.Cleanup.revision revision
      then { item with deletion = Cleanup.Quarantined }
      else item)
  in
  finish
    t
    { current with
      retired
    ; staged_refresh = None
    ; snapshot = { current.snapshot with refresh = Renewal_uncertain expected.operation }
    }
;;

let pending_candidate_operation t ~binding:id =
  Result.map (binding t id) ~f:(fun binding ->
    Option.map binding.candidate ~f:(fun candidate -> candidate.expected.operation))
;;

let cancel_pending_candidate t ~binding:id ~operation =
  let open Result.Let_syntax in
  let%bind current = binding t id in
  match current.candidate with
  | Some candidate when Id.equal candidate.expected.operation operation ->
    discard_candidate t ~binding:id ~expected:candidate.expected
  | Some _ | None -> Error Error.Stale_operation
;;
