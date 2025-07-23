(** IO convenience helpers built on top of {{!module:Eio}Eio}.

    This module gathers a set of small but often-needed utilities that
    are scattered throughout the ChatGPT code-base:

    • Filesystem helpers ([save_doc], [append_doc], etc.) to treat paths as
      first-class capabilities, in the spirit of Eio.
    • Simple logging helpers (both file-based and console).
    • Small wrappers around {{!module:Cohttp_eio}Cohttp_eio} for HTTP
      interactions in {{!module:Net}Net}.
    • A minimal worker pool abstraction in {{!module:Task_pool}} that
      demonstrates how to combine [Eio.Stream] with [Eio.Domain_manager].
    • Reference echo-server / client implementations useful for tests.
    • A helper to embed local images as Base-64 data-URIs.

    All functions expose explicit capabilities instead of relying on
    ambient authority, making them easy to reason about when writing
    concurrent or sandboxed code. *)

open Eio

(** Append a sub-path.

    [base / name] is a convenience alias for
    [Eio.Path.( / ) base name] which joins [name] onto the capability
    [base].  Only the string component of the path is modified; the
    underlying directory capability is preserved. *)
val ( / ) : ([> Fs.dir_ty ] as 'a) Path.t -> string -> 'a Path.t

(** Run [f ()] and wrap the outcome in [Result.t].

    • Returns [Ok (f ())] on success.
    • Returns [Error msg] if [f] raises, where [msg] is obtained via
      [Fmt.str "%a" Eio.Exn.pp ex].

    This helper is useful at API boundaries to convert exceptions into
    a plain-data error channel. *)
val to_res : (unit -> 'a) -> ('a, string) result

(** Append a line to a log file.

    [log ~dir ?file msg] opens (or creates) [file] under [dir] with
    permissions [0o600] and appends [msg] at the end, without adding a
    newline.  The operation is atomic with respect to the flow that the
    underlying [Eio.Path.with_open_out] returns.

    Default [file] value is ["./logs.txt"]. *)
val log : dir:Eio.Fs.dir_ty Eio.Path.t -> ?file:string -> string -> unit

(** Write [msg] to [stdout].

    This is a thin wrapper over [Eio.Flow.copy_string] that exists only
    so callers do not need to open [Eio] directly. *)
val console_log : stdout:[> Flow.sink_ty ] Resource.t -> string -> unit

(** Overwrite a file with [contents].

    [save_doc ~dir file contents] is a convenience wrapper around
    [Eio.Path.save ~create:(`Or_truncate 0o777)].  The whole string is
    written in one go and the file is truncated beforehand if it
    exists.

    Note that permissions [0o777] mimic the behaviour of the original
    code but may be too permissive for security-sensitive contexts. *)
val save_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string -> unit

(** Append [contents] to an existing file.

    Behaviour is the same as {!save_doc} except that data is added at
    the end of the file instead of overwriting it.  A new file is
    created with mode [0o777] when missing. *)
val append_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string -> unit

(** Read a whole file into a string. *)
val load_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string

(** Delete a file if it exists. *)
val delete_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> unit

(** Create a sub-directory.

    [mkdir ~dir sub] is equivalent to
    [Eio.Path.mkdirs ~perm:0o700 (dir / sub)].

    When [exists_ok] is [true] (default: [false]) no exception is raised
    if the directory is already present. *)
val mkdir : ?exists_ok:bool -> dir:Eio.Fs.dir_ty Eio.Path.t -> string -> unit

(** List entries of a directory. *)
val directory : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string list

(** Test whether a path is a directory. *)
val is_dir : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> bool

(** Open [dir] and invoke [f] with an [Eio.Path.t] rooted at it. *)
val with_dir : dir:[> Fs.dir_ty ] Path.t -> ([ `Close | `Dir ] Path.t -> 'a) -> 'a

(*────────────────────────  Data-dir helpers  ──────────────────────────*)

(** Guarantee that the hidden [\.chatmd] data-directory exists.

    [ensure_chatmd_dir ~cwd] returns [cwd /.chatmd], creating the
    directory (permissions 0o700) on the first call.  Subsequent calls
    are idempotent. *)
val ensure_chatmd_dir : cwd:([> Fs.dir_ty ] as 'a) Path.t -> 'a Path.t

module Net : sig
  (** URL helpers *)
  val get_host : string -> string
  (** Extract the host component from an URL (defaulting to "" for
      schemeless paths). *)

  (** Extract the path component of an URL. *)
  val get_path : string -> string

  (** Pre-instantiated TLS client configuration with a no-op
      authenticator.  Useful for quick prototyping; DO NOT use in
      production. *)
  val tls_config : Tls.Config.client

  (** An empty Cohttp header value. *)
  val empty_headers : Http.Header.t

  (** A typed description of how to consume a Cohttp response. *)
  type _ response =
    | Raw : (Http.Response.t * Cohttp_eio.Body.t -> 'a) -> 'a response
    | Default : string response

  (** Perform an HTTPS POST request.

      [post ty ~net ~host ~headers ~path body] opens a TLS connection to
      [host], sends [body] to [path] and decodes the response according
      to [ty]. *)
  val post
    :  'a response
    -> net:_ Eio.Net.t
    -> host:string
    -> headers:Http.Header.t
    -> path:string
    -> string
    -> 'a

  (** Perform an HTTPS GET request. *)
  val get
    :  'a response
    -> net:_ Eio.Net.t
    -> host:string
    -> ?headers:Http.Header.t
    -> string
    -> 'a

  (** Download a remote resource into a local file. *)
  val download_file
    :  _ Eio.Net.t
    -> string
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> filename:string
    -> unit
end

module type Task_pool_config = sig
  (** Type of tasks submitted to the pool. *)
  type input

  (** Result produced by a worker. *)
  type output

  (** Domain manager used to spawn domains. *)
  val dm : Domain_manager.ty Resource.t

  (** In-memory queue transporting work and result promises. *)
  val stream : (input * output Eio.Promise.u) Eio.Stream.t

  (** Switch that supervises the worker lifecycle. *)
  val sw : Eio.Switch.t

  (** Pure function run by each worker.  Must be thread-safe. *)
  val handler : input -> output
end

(** A minimal domain-aware worker pool.

    {b Usage}

    {[
      module Pool = Io.Task_pool (struct
        type input  = int
        type output = int
        let dm     = domain_mgr
        let stream = Eio.Stream.create 0
        let sw     = switch
        let handler x = x * x
      end)

      let () = Pool.spawn "square" in
      assert (Pool.submit 11 = 121)
    ]} *)
module Task_pool : functor (C : Task_pool_config) -> sig
  (** Start a new worker domain.  The function returns immediately. *)
  val spawn : string -> unit

  (** Submit a task and block until its result is available. *)
  val submit : C.input -> C.output
end

(** Convenience entry-point for CLI binaries.

    [run_main main] is equivalent to [Eio_main.run] but also initialises
    the default Mirage-crypto RNG (required by [tls-eio]) before
    delegating to [main]. *)
val run_main : (Eio_unix.Stdenv.base -> 'a) -> 'a

module Server : sig
  (** Simple line-based echo server (demonstration purposes only). *)

  open Eio

  (** Same as {!Eio.traceln} but namespaced. *)
  val traceln : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

  (** Handle a single client until they disconnect.  Called internally
      by {!run}. *)
  val handle_client : [> `Flow | `R | `W ] Resource.t -> [< Eio.Net.Sockaddr.t ] -> unit

  (** Accept clients forever. *)
  val run : _ Eio.Net.listening_socket -> 'a
end

module Client : sig
  (** Matching client implementation for {!Server}. *)

  val traceln : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

  (** Send three "Hello from client" lines and print the replies. *)
  val run
    :  net:_ Eio.Net.t
    -> clock:_ Eio.Time.clock
    -> addr:Eio.Net.Sockaddr.stream
    -> unit
end

module Run_server : sig
  (** Helper to spin up a local server and two test clients. *)

  val main : net:_ Eio.Net.t -> clock:_ Eio.Time.clock -> unit
  val run : unit -> unit
end

module Base64 : sig
  (** Encode a local file into a "data:" URI. *)

  (** [file_to_data_uri ~dir file] loads [file] from [dir], Base-64
      encodes its contents and prefixes the result with the proper MIME
      type deduced from the extension. *)
  val file_to_data_uri : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string
end
