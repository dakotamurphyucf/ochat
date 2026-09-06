open! Core

(** Loads redacted, immutable Shell Security management data through Eio. *)

val load_audit
  :  env:Eio_unix.Stdenv.base
  -> session_id:string
  -> (Shell_security_page_state.audit_page, string) result
