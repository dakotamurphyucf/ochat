(** Shared execution context used by the {!Chat_response} sub-modules.

    The module provides a thin, immutable record bundling together:

    • [env]   – An Eio standard environment (usually
      {!Eio_unix.Stdenv.base}) or a restricted object that exposes at
      least the {e structural} methods we rely on (currently [#net]).

    • [dir]   – The root directory that should be considered “current”
      when resolving relative paths inside prompts or tool invocations.
      This is often {!Eio.Stdenv.fs}, but callers may provide a
      sandboxed subtree if they want to restrict file-system access.

    • [cache] – A {!Cache.t} instance used to memoise network fetches
      and nested agent calls.

    The record is designed to travel through all helper functions so
    that they can call {[Ctx.net ctx]} or {[Ctx.dir ctx]} instead of
    threading long parameter lists.

    The polymorphic ['env] parameter is kept abstract in order to avoid
    a hard dependency on a concrete environment type.  Any object that
    fulfils the structural requirements will compile.  In practice this
    means the value is either the one supplied by [Eio_main.run] or a
    test double that provides compatible stubs.

    {1 API at a glance}

    {v
      (* Constructors *)
      let ctx = Ctx.create ~env ~dir ~cache in
      let ctx' = Ctx.of_env ~env ~cache (* dir = Eio.Stdenv.fs env *)

      (* Accessors *)
      let net   = Ctx.net   ctx in  (* env#net *)
      let dir   = Ctx.dir   ctx in  (* Eio.Path.t *)
      let cache = Ctx.cache ctx in
    v}

    No function performs blocking IO; accessing a field is O(1).
    Thread-safety follows the rules of the underlying structures:
    the context itself is immutable but the [cache] it carries is
    mutable and not synchronised.  Each fibre should therefore use its
    own context or ensure external synchronisation when sharing a cache.
*)

type 'env t =
  { env : 'env
  ; dir : Eio.Fs.dir_ty Eio.Path.t
    (** Root directory used by {!Fetch} helpers for reading local files. *)
  ; cache : Cache.t
    (** Shared TTL-LRU store for memoising agent answers and HTTP fetches. *)
  }

(** [create ~env ~dir ~cache] builds a fresh context from its parts. *)
let create ~env ~dir ~cache = { env; dir; cache }

(** [of_env ~env ~cache] is a shorthand for
    {[create ~env ~dir:(Eio.Stdenv.fs env) ~cache]}. *)
let of_env ~env ~cache = { env; dir = Eio.Stdenv.fs env; cache }

(** [net t] exposes the network namespace ([env#net]). *)
let net t = t.env#net

(** Raw access to the encapsulated environment.  Use with care – prefer
    the specialised helpers provided by other modules when possible. *)
let env t = t.env

(** Filesystem root for local IO operations. *)
let dir t = t.dir

(** Shared cache instance carried by the context. *)
let cache t = t.cache
