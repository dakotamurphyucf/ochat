open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model

(** Host command→original lifecycle operation backlink, not credential authority.
    Shared by standalone and daemon; generic83 response receipts remain separate.
    No challenge result or secret arguments are accepted. *)
module Error : sig
  type t =
    | Corrupt
    | Full
    | Conflict
    | Busy
    | Storage of Private_storage.Error.t
  [@@deriving sexp_of]
end

module Intent : sig
  type t

  val operation : t -> M.Id.t
  val committed : t -> P.Command_receipt.committed option
end

type admission =
  | Fresh of Intent.t
  | Existing of Intent.t

type t

val create
  :  Private_storage.Directory.t
  -> host:M.Id.t
  -> maximum_records:int
  -> (t, Error.t) Result.t

val begin_
  :  t
  -> principal:P.Id.Principal.t
  -> key:P.Idempotency_key.t
  -> method_name:string
  -> params:Jsonaf.t
  -> operation:M.Id.t
  -> (admission, Error.t) Result.t

val lookup
  :  t
  -> principal:P.Id.Principal.t
  -> key:P.Idempotency_key.t
  -> method_name:string
  -> params:Jsonaf.t
  -> (Intent.t option, Error.t) Result.t

val complete : t -> Intent.t -> P.Command_receipt.committed -> (unit, Error.t) Result.t

(** Metadata-only original operation proof for trusted bootstrap recovery.
    Never enrolls, completes an intent, authorizes a caller or synthesizes an ID. *)
val has_operation
  :  t
  -> method_name:string
  -> operation:M.Id.t
  -> (bool, Error.t) Result.t
