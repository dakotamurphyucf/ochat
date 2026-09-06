open! Core
module Persisted = Session.Shell_state.Manifest_grant
module M = Chatmd_shell_spec.Manifest

type source =
  { canonical_source_root : string
  ; source_sha256 : string
  ; repository_identity : string option
  }

type bindings =
  { user_id : string option
  ; host_id : string option
  }

type grant = Persisted.persisted [@@deriving bin_io, sexp]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

let wall_clock_now_ns () =
  Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_int64
;;

let source_material manifest =
  Manifest_signature.payload
    ~manifest
    ~issuer:"session-user"
    ~audience:[]
    ~issued_at_unix:0L
    ~expires_at_unix:None
;;

let same_optional_binding expected actual =
  Option.for_all expected ~f:(fun expected ->
    Option.exists actual ~f:(String.equal expected))
;;

let active now grant =
  Option.is_none grant.Persisted.revoked_at_ns
  && Option.for_all grant.expires_at_ns ~f:(fun expiry -> Int64.(expiry >= now))
;;

let matches ~now_ns ~session_id ~(source : source) ~(bindings : bindings) ~manifest grant =
  let material = source_material manifest in
  active now_ns grant
  && String.equal grant.Persisted.manifest_sha256 manifest.M.sha256
  && String.equal grant.canonical_source_root source.canonical_source_root
  && String.equal grant.source_sha256 source.source_sha256
  && Option.equal String.equal grant.repository_identity source.repository_identity
  && Int.equal grant.schema_version 1
  && List.equal [%equal: string * string] grant.builtin_versions material.builtin_versions
  && List.equal
       [%equal: string * string]
       grant.imported_source_sha256
       material.imported_source_sha256
  && Option.equal String.equal grant.session_id (Some session_id)
  && same_optional_binding grant.user_id bindings.user_id
  && same_optional_binding grant.host_id bindings.host_id
;;

let fresh_id manifest now =
  String.concat [ manifest.M.sha256; Int64.to_string now; Int.to_string (Random.bits ()) ]
  |> Md5.digest_string
  |> Md5.to_hex
;;

let make_grant ~now_ns ~session_id ~(source : source) ~(bindings : bindings) manifest =
  let material = source_material manifest in
  Persisted.
    { grant_id = fresh_id manifest now_ns
    ; manifest_sha256 = manifest.M.sha256
    ; canonical_source_root = source.canonical_source_root
    ; repository_identity = source.repository_identity
    ; source_sha256 = source.source_sha256
    ; signer = None
    ; issuer = Some "session-user"
    ; audience = []
    ; schema_version = 1
    ; builtin_versions = material.builtin_versions
    ; imported_source_sha256 = material.imported_source_sha256
    ; session_id = Some session_id
    ; user_id = bindings.user_id
    ; host_id = bindings.host_id
    ; created_at_ns = now_ns
    ; expires_at_ns = None
    ; revoked_at_ns = None
    ; revocation_reason = None
    }
;;

let authorizer ~load ~remember ~now_ns ~session_id ~source ~bindings ~fallback =
  let mutex = Eio.Mutex.create () in
  fun request ->
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
      let manifest = request.Manifest_authorizer.manifest in
      let now_ns = now_ns () in
      match load () with
      | Error error -> Manifest_authorizer.Reject error.message
      | Ok grants ->
        let exists =
          List.exists grants ~f:(fun grant ->
            matches ~now_ns ~session_id ~source ~bindings ~manifest grant)
        in
        if exists
        then Manifest_authorizer.Authorize_once
        else (
          match fallback request with
          | Manifest_authorizer.Reject _ as response -> response
          | Authorize_once ->
            let grant = make_grant ~now_ns ~session_id ~source ~bindings manifest in
            (match remember grant with
             | Ok () -> Authorize_once
             | Error error ->
               Reject ("failed to persist exact manifest authorization: " ^ error.message))))
;;

let session_authorizer ~session ~persist ~source ~bindings ~fallback =
  fun request ->
  let manifest = request.Manifest_authorizer.manifest in
  let session_id = !session.Session.id in
  let now_ns = wall_clock_now_ns () in
  let exists =
    List.exists !session.shell_state.manifest_grants ~f:(fun grant ->
      matches ~now_ns ~session_id ~source ~bindings ~manifest grant)
  in
  if exists
  then Manifest_authorizer.Authorize_once
  else (
    match fallback request with
    | Manifest_authorizer.Reject _ as response -> response
    | Authorize_once ->
      let grant = make_grant ~now_ns ~session_id ~source ~bindings manifest in
      let shell_state =
        { !session.shell_state with
          manifest_grants = grant :: !session.shell_state.manifest_grants
        }
      in
      let updated = { !session with shell_state } in
      (match persist updated with
       | Ok () ->
         session := updated;
         Authorize_once
       | Error message ->
         Reject ("failed to persist exact manifest authorization: " ^ message)))
;;

let revoke ~session ~persist ~grant_id ~reason =
  if
    not
      (List.exists !session.Session.shell_state.manifest_grants ~f:(fun grant ->
         String.equal grant.Persisted.grant_id grant_id))
  then Error ("unknown manifest grant: " ^ grant_id)
  else (
    let revoked_at_ns = Some (wall_clock_now_ns ()) in
    let manifest_grants =
      List.map !session.Session.shell_state.manifest_grants ~f:(fun grant ->
        if String.equal grant.Persisted.grant_id grant_id
        then { grant with revoked_at_ns; revocation_reason = reason }
        else grant)
    in
    let shell_state = { !session.shell_state with manifest_grants } in
    let updated = { !session with shell_state } in
    Result.map (persist updated) ~f:(fun () -> session := updated))
;;
