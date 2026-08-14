open! Core
module Persisted = Session.Shell_state.Approval_grant
module Scope = Session.Shell_state.Approval_scope
module Request_kind = Session.Shell_state.Request_kind

type grant = Persisted.persisted [@@deriving bin_io, sexp]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

type bindings =
  { user_id : string option
  ; host_id : string option
  }
[@@deriving sexp, compare, equal]

type backend =
  { read : unit -> (grant list, error) result
  ; mutate : 'a. (grant list -> (grant list * 'a, error) result) -> ('a, error) result
  }

type t =
  { mutex : Eio.Mutex.t
  ; backend : backend
  ; bindings : bindings
  }

let error code message = Error { code; message }

let protect code f =
  try Ok (f ()) with
  | exn -> error code (Exn.to_string exn)
;;

let with_lock t f = Eio.Mutex.use_rw ~protect:true t.mutex f

let memory_backend initial =
  let grants = ref initial in
  { read = (fun () -> Ok !grants)
  ; mutate =
      (fun update ->
        Result.map (update !grants) ~f:(fun (updated, result) ->
          grants := updated;
          result))
  }
;;

let memory ?(initial = []) ~bindings () =
  { mutex = Eio.Mutex.create (); backend = memory_backend initial; bindings }
;;

let session ~session ~persist ~bindings =
  let read () = Ok (!session).Session.shell_state.approval_grants in
  let mutate update =
    Result.bind (read ()) ~f:(fun grants ->
      Result.bind (update grants) ~f:(fun (approval_grants, result) ->
        let shell_state = { (!session).shell_state with approval_grants } in
        let updated = { !session with shell_state } in
        match persist updated with
        | Error message -> error "shell.approval_session_write_failed" message
        | Ok () ->
          session := updated;
          Ok result))
  in
  { mutex = Eio.Mutex.create (); backend = { read; mutate }; bindings }
;;

module Durable_file = struct
  module Data = struct
    type t =
      { version : int
      ; grants : grant list
      ; payload_hmac_sha256 : string option
      }
    [@@deriving bin_io, sexp]
  end

  let version = 1

  let payload grants = Sexp.to_string_mach ([%sexp_of: grant list] grants)

  let mac integrity_key grants =
    Option.map integrity_key ~f:(fun key ->
      Digestif.SHA256.(hmac_string ~key (payload grants) |> to_hex))
  ;;

  let verify ~integrity_key data =
    if not (Int.equal data.Data.version version)
    then error "shell.approval_file_version" "unsupported approval-store version"
    else if Option.equal String.equal data.payload_hmac_sha256 (mac integrity_key data.grants)
    then Ok data.grants
    else error "shell.approval_file_integrity" "approval-store integrity check failed"
  ;;

  let read ~fs ~path ~integrity_key () =
    let file = Eio.Path.(fs / path) in
    if not (Eio.Path.is_file file)
    then Ok []
    else
      protect "shell.approval_file_read_failed" (fun () ->
        Bin_prot_utils_eio.read_bin_prot (module Data) file
        |> verify ~integrity_key)
      |> Result.join
  ;;

  let encoded ~integrity_key grants =
    let data = Data.{ version; grants; payload_hmac_sha256 = mac integrity_key grants } in
    Bin_prot.Utils.bin_dump ~header:true Data.bin_writer_t data |> Bigstring.to_string
  ;;

  let write ~env ~fs ~path ~integrity_key ~durable grants =
    protect "shell.approval_file_write_failed" (fun () ->
      Io.mkdir ~exists_ok:true ~dir:fs (Filename.dirname path);
      let pid = Core_unix.getpid () |> Pid.to_int in
      let suffix = sprintf ".tmp.%d.%d" pid (Random.bits ()) in
      let temporary = Eio.Path.(fs / (path ^ suffix)) in
      let destination = Eio.Path.(fs / path) in
      Fun.protect
        ~finally:(fun () ->
          try if Eio.Path.is_file temporary then Eio.Path.unlink temporary with
          | _ -> ())
        (fun () ->
           Eio.Switch.run (fun sw ->
             let flow =
               Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) temporary
             in
             Eio.Flow.copy_string (encoded ~integrity_key grants) flow;
             if durable then Eio.File.sync flow);
           Eio.Path.rename temporary destination))
  ;;

  let acquire_lock ~env ~fs lock_path =
    let clock = Eio.Stdenv.clock env in
    let rec loop remaining =
      match
        Or_error.try_with (fun () ->
          Eio.Path.save ~create:(`Exclusive 0o600) lock_path "")
      with
      | Ok () -> Ok ()
      | Error _ when remaining > 0 ->
        Eio.Time.sleep clock 0.01;
        loop (remaining - 1)
      | Error error ->
        Error
          { code = "shell.approval_file_locked"
          ; message = Error.to_string_hum error
          }
    in
    loop 200
  ;;

  let mutate ~env ~fs ~path ~integrity_key ~durable update =
    let lock_path = Eio.Path.(fs / (path ^ ".lock")) in
    Result.bind (acquire_lock ~env ~fs lock_path) ~f:(fun () ->
      Fun.protect
        ~finally:(fun () ->
          try Eio.Path.unlink lock_path with
          | _ -> ())
        (fun () ->
           Result.bind (read ~fs ~path ~integrity_key ()) ~f:(fun grants ->
             Result.bind (update grants) ~f:(fun (updated, result) ->
               Result.map
                 (write ~env ~fs ~path ~integrity_key ~durable updated)
                 ~f:(fun () -> result)))))
  ;;
end

let durable_file ~env ~path ?integrity_key ?(durable = true) ~bindings () =
  let fs = Eio.Stdenv.fs env in
  let read = Durable_file.read ~fs ~path ~integrity_key in
  Result.map (read ()) ~f:(fun _ ->
    let mutate
      : 'a.
        (grant list -> (grant list * 'a, error) result) -> ('a, error) result
      = fun update ->
        Durable_file.mutate ~env ~fs ~path ~integrity_key ~durable update
    in
    { mutex = Eio.Mutex.create (); backend = { read; mutate }; bindings })
;;

let request_kind = function
  | Shell_access.Context.Structured -> Request_kind.Structured
  | Script_file -> Script_file
  | Raw_shell -> Raw_shell
;;

let ns_since_epoch time =
  Time_ns.to_int63_ns_since_epoch time |> Int63.to_int64
;;

let ns_of_unix_seconds seconds =
  Time_ns.Span.of_sec seconds
  |> Time_ns.of_span_since_epoch
  |> ns_since_epoch
;;

let active ~now (grant : grant) =
  Option.is_none grant.Persisted.revoked_at_ns
  && Option.for_all grant.expires_at_ns ~f:(fun expiry ->
    Int64.(expiry >= ns_since_epoch now))
;;

let same_binding expected actual =
  Option.for_all expected ~f:(fun expected ->
    Option.exists actual ~f:(String.equal expected))
;;

let same_identity
      (grant : grant)
      (identity : Shell_access.Approval.identity)
  =
  String.equal grant.manifest_sha256 identity.Shell_access.Approval.manifest_sha256
  && String.equal grant.runtime_id identity.runtime_id
  && Poly.equal grant.request_kind (request_kind identity.request_kind)
  && String.equal grant.executable_sha256 identity.executable_sha256
  && String.equal grant.cwd_sha256 identity.cwd_sha256
  && String.equal grant.environment_sha256 identity.environment_sha256
  && Option.equal String.equal grant.stdin_sha256 identity.stdin_sha256
  && Int.equal grant.stdin_bytes identity.stdin_bytes
  && Option.equal String.equal grant.script_sha256 identity.script_sha256
;;

let scope_matches
      (grant : grant)
      ~session_id
      (identity : Shell_access.Approval.identity)
  =
  match grant.Persisted.scope with
  | Scope.Exact_session ->
    Option.equal String.equal grant.session_id session_id
    && String.equal grant.command_sha256 identity.Shell_access.Approval.command_hash
  | Prefix_session { prefix } ->
    Option.equal String.equal grant.session_id session_id
    && List.is_prefix identity.argv ~prefix ~equal:String.equal
  | Durable_exact ->
    String.equal grant.command_sha256 identity.command_hash
;;

let matches
      t
      ~now
      ~session_id
      (identity : Shell_access.Approval.identity)
      (grant : grant)
  =
  active ~now grant
  && same_binding grant.user_id t.bindings.user_id
  && same_binding grant.host_id t.bindings.host_id
  && same_identity grant identity
  && scope_matches grant ~session_id identity
;;

let lookup t ~now ~session_id identity =
  with_lock t (fun () ->
    t.backend.mutate (fun grants ->
      match List.find grants ~f:(matches t ~now ~session_id identity) with
      | None -> Ok (grants, None)
      | Some matched ->
        let last_used_at_ns = Some (ns_since_epoch now) in
        let updated =
          List.map grants ~f:(fun grant ->
            if String.equal grant.grant_id matched.grant_id
            then { grant with last_used_at_ns }
            else grant)
        in
        Ok (updated, Some { matched with last_used_at_ns })))
;;

let scope_and_expiry = function
  | Shell_access.Approval.Once -> None
  | Exact_session { expires_at } ->
    Some (Scope.Exact_session, Option.map expires_at ~f:ns_of_unix_seconds, None)
  | Prefix_session { prefix; expires_at } ->
    Some (Scope.Prefix_session { prefix }, Option.map expires_at ~f:ns_of_unix_seconds, Some prefix)
  | Durable_exact { expires_at } ->
    Some (Scope.Durable_exact, Option.map expires_at ~f:ns_of_unix_seconds, None)
;;

let fresh_grant_id identity now =
  String.concat
    [ identity.Shell_access.Approval.command_hash
    ; Int64.to_string (ns_since_epoch now)
    ; Int.to_string (Random.bits ())
    ]
  |> Md5.digest_string
  |> Md5.to_hex
;;

let reviewer = function
  | None -> Session.Shell_state.Reviewer.{ source = "unknown"; reviewer_id = None }
  | Some metadata ->
    { source = metadata.Shell_access.Approval.reviewer_kind
    ; reviewer_id = Some metadata.reviewer_id
    }
;;

let make_grant t ~now ~session_id identity scope metadata =
  Option.map (scope_and_expiry scope) ~f:(fun (scope, expires_at_ns, argv_prefix) ->
    Persisted.
      { grant_id = fresh_grant_id identity now
      ; manifest_sha256 = identity.manifest_sha256
      ; runtime_id = identity.runtime_id
      ; request_kind = request_kind identity.request_kind
      ; command_sha256 = identity.command_hash
      ; executable_sha256 = identity.executable_sha256
      ; argv = identity.argv
      ; argv_prefix
      ; cwd_sha256 = identity.cwd_sha256
      ; environment_sha256 = identity.environment_sha256
      ; stdin_sha256 = identity.stdin_sha256
      ; stdin_bytes = identity.stdin_bytes
      ; script_sha256 = identity.script_sha256
      ; scope
      ; session_id
      ; user_id = t.bindings.user_id
      ; host_id = t.bindings.host_id
      ; created_at_ns = ns_since_epoch now
      ; expires_at_ns
      ; last_used_at_ns = None
      ; reviewer = reviewer metadata
      ; revoked_at_ns = None
      ; revocation_reason = None
      })
;;

let remember t ~now ~session_id identity scope metadata =
  match make_grant t ~now ~session_id identity scope metadata with
  | None -> Ok ()
  | Some grant ->
    with_lock t (fun () ->
      t.backend.mutate (fun grants -> Ok (grant :: grants, ())))
;;

let revoke t ~now ~grant_id ~reason =
  with_lock t (fun () ->
    t.backend.mutate (fun grants ->
      if not (List.exists grants ~f:(fun grant -> String.equal grant.grant_id grant_id))
      then error "shell.approval_grant_not_found" ("unknown grant: " ^ grant_id)
      else
        let revoked_at_ns = Some (ns_since_epoch now) in
        let updated =
          List.map grants ~f:(fun grant ->
            if String.equal grant.grant_id grant_id
            then { grant with revoked_at_ns; revocation_reason = reason }
            else grant)
        in
        Ok (updated, ())))
;;

let list t = with_lock t t.backend.read

let executor_store t =
  Shell_access.Approval.create_store
    ~lookup:(fun ~now ~session_id identity ->
      lookup t ~now:(Time_ns.of_span_since_epoch (Time_ns.Span.of_sec now)) ~session_id identity
      |> Result.map ~f:Option.is_some
      |> Result.map_error ~f:(fun error -> error.message))
    ~remember:(fun ~session_id identity scope metadata ->
      remember t ~now:(Time_ns.now ()) ~session_id identity scope metadata
      |> Result.map_error ~f:(fun error -> error.message))
    ()
;;
