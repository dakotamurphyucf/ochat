(** Private stable identity for an explicitly trusted process-local operator.
    The caller supplies the already exclusively owned store. This is not a
    credential, execution grant or mapping for network principals. Root ownership
    is established by the store lock and current OS-user/private-directory policy.
    Child lookup and atomic publication retain the opened, validated directory
    capability; record validation and bounded decoding use the same file FD.
    The OS account and configured root anchor are trusted. This is not protection
    against an adversary already controlling that same OS account. *)
val load_or_create
  :  env:Eio_unix.Stdenv.base
  -> Session_store.t
  -> (Agent_protocol.Id.Principal.t, Store_error.t) result

(** Read-only preflight before host composition. An absent root may be created
    privately by the store; existing roots must prove native local ownership. *)
val validate_root
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> (unit, Store_error.t) result
