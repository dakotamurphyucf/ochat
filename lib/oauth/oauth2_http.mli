open Core

(** Tiny one-shot HTTP helpers built on {!Piaf}.

    The helpers are opinionated towards OAuth&nbsp;2.0 use-cases where nearly
    every request/response pair is JSON-encoded and fits comfortably in
    memory.

    All functions run inside an {!Eio} fibre – pass the [env] you receive
    from {!Eio_main.run} and a fresh {!Eio.Switch.t} that scopes network
    resources.

    Returned values follow the common [`(Jsonaf.t, string) Result.t`] pattern.
    The [Error] branch contains a message from {!Piaf.Error.to_string} or –
    for JSON decoding failures – {!Exn.to_string}.
*)

(** [get_json ~env ~sw url] issues an HTTP {b GET} request to [url] and
    decodes the response body as JSON.  Transport-level problems such as TLS
    failures or timeouts are propagated via the [Error] case. *)
val get_json
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> (Jsonaf.t, string) Result.t

(** [post_form ~env ~sw url params] submits [params] as an
    [application/x-www-form-urlencoded] body and decodes the JSON response.

    Each [(k, v)] pair is percent-encoded individually.  A JSON decoding
    failure is {b raised} rather than wrapped in [Error] for backward
    compatibility. *)
val post_form
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> (string * string) list
  -> (Jsonaf.t, string) Result.t

(** [post_json ~env ~sw url payload] sends [payload] as
    [Content-Type: application/json] and parses the server’s JSON reply.
    Parsing errors are captured and returned as [Error]. *)
val post_json
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> string
  -> Jsonaf.t
  -> (Jsonaf.t, string) Result.t
