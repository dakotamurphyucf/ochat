open! Core

type t =
  { uid : int
  ; gid : int
  ; pid : int option
  }
[@@deriving compare, equal, sexp]

external raw_peer_credentials
  :  Caml_unix.file_descr
  -> int * int * int
  = "ochat_unix_peer_credentials"

external effective_uid : unit -> int = "ochat_unix_effective_uid"

let authentication_error message =
  Agent_protocol.Error.create Unauthenticated ~message ~retryable:false ()
;;

let of_flow flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> Error (authentication_error "Unix peer credentials are unavailable")
  | Some fd ->
    Result.try_with (fun () ->
      Eio_unix.Fd.use_exn "unix-peer-credentials" fd raw_peer_credentials)
    |> Result.map ~f:(fun (uid, gid, pid) ->
      { uid; gid; pid = Option.some_if (pid >= 0) pid })
    |> Result.map_error ~f:(fun exn ->
      authentication_error ("failed to read Unix peer credentials: " ^ Exn.to_string exn))
;;

let principal_id uid =
  Digestif.SHA256.(digest_string ("unix-peer:v1:" ^ Int.to_string uid) |> to_hex)
  |> fun digest -> Agent_protocol.Id.Principal.of_string ("pri_" ^ digest)
;;

let attributes credentials =
  [ "unix.gid", Int.to_string credentials.gid; "unix.uid", Int.to_string credentials.uid ]
  @ Option.value_map credentials.pid ~default:[] ~f:(fun pid ->
    [ "unix.pid", Int.to_string pid ])
;;

let authenticate_same_user ~scopes flow =
  let open Result.Let_syntax in
  let%bind credentials = of_flow flow in
  if credentials.uid <> effective_uid ()
  then Error (authentication_error "Unix peer UID is not authorized")
  else (
    let%bind id = principal_id credentials.uid in
    Agent_protocol.Principal.create
      ~id
      ~authentication_kind:"unix.peer"
      ~scopes
      ~attributes:(attributes credentials))
;;
