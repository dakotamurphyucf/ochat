open! Core

val filter
  :  Hook_worker.t
  -> secret_filter:Shell_access.Secret_filter.t
  -> Shell_access.Audit.envelope
  -> (Hook_protocol.action, Hook_worker.error) result
