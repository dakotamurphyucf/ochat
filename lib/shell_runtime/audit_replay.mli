open! Core

(** Read-only validation and reconstruction of shell JSONL audit logs. No
    function in this module can execute a command. *)

type event =
  { schema_version : string
  ; sequence : int64
  ; timestamp : float
  ; session_id : string option
  ; runtime_id : string
  ; manifest_sha256 : string
  ; request_id : string
  ; plan_id : string option
  ; name : string
  ; fields : (string * Jsonaf.t) list
  ; previous_event_sha256 : string option
  ; event_sha256 : string option
  }
[@@deriving sexp]

type request =
  { request_id : string
  ; runtime_id : string
  ; manifest_sha256 : string
  ; command : string option
  ; command_sha256 : string option
  ; cwd_sha256 : string option
  ; request_kind : string option
  ; effects : string list
  ; policy_action : string option
  ; approval_answer : string option
  ; interceptors : string list
  ; backend : string option
  ; stdout_bytes : int
  ; stderr_bytes : int
  ; exit_kind : string option
  ; exit_code : int option
  ; signal : int option
  ; rejected_reason : string option
  ; completed : bool
  ; events : event list
  }
[@@deriving sexp]

type error =
  { code : string
  ; line : int option
  ; message : string
  }
[@@deriving sexp, compare, equal]

val load
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> path:string
  -> (event list, error list) result

(** [load_rotated ~fs ~path] loads numbered rotations from oldest to newest,
    followed by [path]. It caps discovery at 1,024 files. *)
val load_rotated
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> path:string
  -> (event list, error list) result

(** Checks sequence continuity and every present integrity-chain digest. *)
val validate : event list -> (unit, error list) result

val requests : event list -> request list
val request : event list -> request_id:string -> (request, error) result
val render_event : event -> string
val render_request : request -> string
