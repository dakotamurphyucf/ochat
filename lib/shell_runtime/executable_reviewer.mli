open! Core

type t

val create : Hook_worker.t -> t

val review
  :  t
  -> Shell_access.Approval.request
  -> (Shell_access.Approval.response option, Hook_worker.error) result
