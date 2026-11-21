(** The private key for protected native approvals under a Session_store.
    The caller holds the original Store's exclusive lock throughout loading,
    creation and use. Its raw key never leaves this module. *)
type t

val load_or_create
  :  env:Eio_unix.Stdenv.base
  -> root:Data_root.t
  -> (t, Store_error.t) result

val commit
  :  t
  -> owner:Agent_protocol.Session_ref.t
  -> generation:int
  -> profile_revision:string
  -> tool_name:string
  -> tool_revision:string
  -> resource_fingerprint:string
  -> input:Jsonaf.t
  -> string
