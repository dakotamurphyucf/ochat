open! Core

let filter worker ~secret_filter envelope =
  Hook_worker.invoke worker (Hook_payload.audit_event ~secret_filter envelope)
;;
