(** Opaque identifiers used by the Ochat agent protocol. *)

module Generator : sig
  type t

  (** [create ~bytes] creates an identifier generator backed by [bytes].
      [bytes length] must return exactly [length] bytes. *)
  val create : bytes:(int -> string) -> t

  (** [secure] uses the process cryptographic random generator. *)
  val secure : t
end

module type S = sig
  type t [@@deriving compare, hash, sexp]

  (** [create ()] creates a cryptographically random identifier. *)
  val create : unit -> t

  (** [create_with generator] creates an identifier using [generator]. *)
  val create_with : Generator.t -> t

  (** [of_string value] validates and parses [value]. *)
  val of_string : string -> (t, Error.t) result

  (** [to_string t] returns the opaque wire representation of [t]. *)
  val to_string : t -> string

  (** [to_json t] encodes [t] as a JSON string. *)
  val to_json : t -> Jsonaf.t

  (** [of_json json] decodes and validates a JSON string identifier. *)
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Server : S
module Session : S
module Attachment : S
module Operation : S
module Event_cursor : S
module Transaction : S
module Job : S
module Invocation : S
module Subscription : S
module Delivery : S
module Capability : S
module Schedule : S
module Permission : S
module Grant : S
module Workspace_definition : S
module Workspace_instance : S
module Prompt_definition : S
module Prompt_revision : S
module Principal : S
module Blob : S
module Idempotency_record : S
