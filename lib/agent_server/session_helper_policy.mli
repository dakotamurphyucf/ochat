(** Operator-owned configuration for the optional external session helper.
    This does not register tools or enable extension support. *)
type t =
  { tool_name : string
  ; executable : string
  ; executable_sha256 : string
  ; arguments : string list
  ; operations : string list
  ; read_roots : string list
  ; environment : string list
  ; private_paths : string list
  ; max_request_bytes : int
  ; max_response_bytes : int
  ; max_requests : int
  }
[@@deriving compare, equal, sexp]

(** Capture canonical paths and produce the existing caller-scoped grant.
    [protected_paths] includes host credentials, configuration, store and control
    endpoints. Explicit roots must contain only data the helper may read. Known
    protected paths may not overlap explicit or implicit sandbox read roots.
    Every invocation rechecks path identities and its exact final execution
    context. No environment values are inherited by this configuration. *)
val grants
  :  env:Eio_unix.Stdenv.base
  -> protected_paths:string list
  -> t list
  -> (Agent_session.Session_management_channel.grant list, string) result
