open! Core

(** Immutable host configuration choices. No endpoint, account, binding or
    credential is declared here. A choice inherits the verified canonical
    owner's identity, capabilities and qualified transport policies. *)
module Error : sig
  type t =
    | Invalid_descriptor
    | Invalid_document
  [@@deriving equal, sexp_of]
end

type t

val create
  :  id:string
  -> credential_owner:string
  -> revision:string
  -> defaults:Openai.Responses_driver.Setting.t list
  -> (t, Error.t) Result.t

val id : t -> string
val credential_owner : t -> string
val revision : t -> string

(** Call only after the canonical mapping has been verified. The canonical
    profile ID must equal credential_owner. No profile is guessed for an
    unmapped OAuth template. *)
val derive
  :  t
  -> canonical:Openai.Responses_driver.Profile.t
  -> (Openai.Responses_driver.Profile.t, Error.t) Result.t

(** Strict authored configuration: at most128 unique choices, bounded structural
    JSON, defaults as a named setting object. Unknown identity/policy fields,
    nondefault setting provenance, duplicate names/IDs and self references reject.
    Complete canonical-owner validation belongs to bridge composition. *)
val of_json : Jsonaf.t -> (t list, Error.t) Result.t

(** Decode bounded authored text before constructing a JSON tree. *)
val of_string : string -> (t list, Error.t) Result.t

(** Validate one complete immutable host declaration set. Owners are canonical
    approved IDs (including unmapped templates); no chains, ID collisions or
    more than128 total owner/choice IDs. *)
val validate_set : t list -> credential_owners:string list -> (unit, Error.t) Result.t
