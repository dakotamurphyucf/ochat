open! Core

(** Nonsecret named daemon target. Credential references are filenames, never
    provider profiles or bearer values. A profile does not grant authority. *)
type t

val create
  :  home:string option
  -> name:string
  -> endpoint:string
  -> expected_server:Agent_protocol.Id.Server.t option
  -> daemon_credential_file:string option
  -> (t, Agent_protocol.Error.t) result

val name : t -> string
val description : t -> string
val expected_server : t -> Agent_protocol.Id.Server.t option

(** Open an owned connection, initialize it and validate pinned host identity
    before returning it. Failure closes the opened connection. *)
val connect
  :  t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> notification_capacity:int
  -> (Agent_client.Connection.t, Agent_protocol.Error.t) result

(** Versioned nonsecret profile document. Required-null optional fields retain
    absence/null distinctions at admission; unknown fields are preserved by
    callers retaining the original document. Unsupported kinds/versions fail. *)
val to_document : t -> (Document_schema.Document.t, Agent_protocol.Error.t) result

val of_document
  :  home:string option
  -> Document_schema.Document.t
  -> (t, Agent_protocol.Error.t) result

(** Explicit nonsecret profile file operations through Eio. Save atomically
    replaces the selected file and preserves restored unknown fields.
    Paths must be absolute; credentials are neither loaded nor stored here. *)
val load
  :  home:string option
  -> env:Eio_unix.Stdenv.base
  -> path:string
  -> (t, Agent_protocol.Error.t) result

val save
  :  t
  -> env:Eio_unix.Stdenv.base
  -> path:string
  -> (unit, Agent_protocol.Error.t) result
