open! Core

module Destination : sig
  (** Reject the same source coordinate even when its file has been removed.
      Resolve existing parents and existing file aliases; a missing parent uses
      an absolute lexical coordinate. This is a read-only frontend guard, not a
      filesystem reservation. Atomic output installation remains the caller's
      responsibility. Filesystem failures and cancellation propagate. *)
  val conflicts_with
    :  env:Eio_unix.Stdenv.base
    -> source_file:string
    -> output_file:string
    -> bool
end

(** Explicit edited-ChatMD adoption into a fresh neutral transcript. Authored
    conversion, not restoration of captured provider payloads or promotion of
    rendered/public history. The executable definition remains separate. *)
val load
  :  Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> file:string
  -> checkpoint:(unit -> unit)
  -> (Agent_protocol.Session_seed.Transcript.t, Agent_protocol.Error.t) Result.t

(** One bounded source read, one shared owned grammar invocation and one existing
    Converter/Responses_history conversion. Bounds: 8 MiB source, 100000 tokens,
    depth 128, 100000 history entries, 2 million cooperative work steps and 16 MiB
    neutral input. Fresh host IDs replace presentation IDs; opaque call IDs and
    metadata recognized by the existing converter stay payloads. Unavailable or
    unsupported provider capture is not reconstructed. Reject executable and
    configuration declarations, imports,
    documents, embedded agents, local images and unsupported top-level opaque
    markup before conversion. Remote image URIs remain data; no network fetch.
    Caller supplies genuine context; no initializer/tool/inference is invoked.
    Existing parser [Failure] diagnostics (malformed lexing, invalid message
    attributes and declared parser/input/admission limits) become Invalid_request.
    This catch covers only parser admission, never Converter or neutral lowering.
    Original callback/cancellation failures, including Failure, preserve their
    backtrace; other exception constructors propagate. *)
