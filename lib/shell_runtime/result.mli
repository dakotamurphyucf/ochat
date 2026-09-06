open! Core

(** Stable shell result values returned by ChatMD shell tools. *)

type status =
  | Exited of int
  | Signaled of int
[@@deriving sexp, compare, equal, jsonaf]

type command =
  { argv : string list
  ; executable_sha256 : string
  ; status : status
  ; intercepted_by : string option
  }
[@@deriving sexp, compare, equal, jsonaf]

type t =
  { request_id : string
  ; status : status
  ; stdout : string
  ; stderr : string
  ; stdout_truncated : bool
  ; stderr_truncated : bool
  ; backend : string
  ; runtime_id : string
  ; manifest_sha256 : string
  ; commands : command list
  }
[@@deriving sexp, compare, equal, jsonaf]

type error =
  { code : string
  ; message : string
  ; permission_request : Shell_access.Approval.request option
  }

val of_executor
  :  runtime_id:string
  -> manifest_sha256:string
  -> Shell_access.Executor.result
  -> t

val error_of_executor : Shell_access.Executor.error -> error
