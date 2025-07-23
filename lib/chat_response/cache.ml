(** In-memory and on-disk cache for agent executions.

    The [Cache] module provides a small **TTL-based LRU cache** that maps an
    {!Prompt.Chat_markdown.agent_content} value – effectively the *complete
    specification* of an `<agent/>` element (its URL and any inline user
    items) – to the textual response returned by running that agent.

    The cache lives entirely in memory while the program executes but can be
    serialised to / read from disk using {!save} and {!load}.  Persistence is
    binary, deterministic, and compact thanks to {!Bin_prot}.

    {1 Design}

    • LRU policy with a *time-to-live* (TTL) per entry – implemented by
      {!Ttl_lru_cache}.  
    • Key is {!Prompt.Chat_markdown.agent_content}.  
    • Value is the **verbatim assistant answer** (a [string]).

    {1 High-level API}

    {ul
    {- {!create} constructs an empty cache with a bounded size.}
    {- {!find_or_add} returns a cached value or computes and stores a fresh one.}
    {- {!load}/{!save} transparently handle on-disk persistence between runs.}}

    Example: reading a cache at start-up and persisting it on exit
    {[
      let cache_file = Path.(cwd / "cache.bin") in
      let cache = Cache.load ~file:cache_file ~max_size:1000 () in

      (* … use [cache] during the program … *)

      Cache.save ~file:cache_file cache
    ]}
*)

open Core
module CM = Prompt.Chat_markdown

(*--- 0-a. Key + underlying LRU implementation -----------------------*)
module Key = struct
  type t = CM.agent_content [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module LRU = Ttl_lru_cache.Make (Key)

(* Abstract type exposed to callers.  Values are backed by a TTL-LRU
     storing the textual response of an agent prompt keyed by the
     agent specification (its url + inline user items). *)
type t = string LRU.t

(*--- 0-b. Persistence helpers ---------------------------------------*)
type persistent_form =
  { max_size : int
  ; items : (Key.t * string LRU.entry) list
  }
[@@deriving bin_io]

(** [create ~max_size ()] returns a fresh in-memory cache that can hold at most
    [max_size] entries.  When inserting a new item and the cache is full, the
    *least recently used* entry is evicted.

    Entries are still subject to their individual TTL even if the cache is not
    full. *)
let create ~max_size () = LRU.create ~max_size ()

(** Internal: convert an in-memory cache into the serialisable form that is
    written on disk.  Exposed only for unit tests. *)
let to_persistent lru = { max_size = LRU.max_size lru; items = LRU.to_alist lru }

(** Internal: inverse of {!to_persistent}. *)
let of_persistent pf =
  let cache = create ~max_size:pf.max_size () in
  List.iter pf.items ~f:(fun (k, v) -> LRU.set cache ~key:k ~data:v);
  cache
;;

(* Binary serialisation on disk – deterministic and compact. *)
(** [write_file ~file cache] serialises [cache] using [Bin_prot] and writes it
    atomically to [file].  The function truncates any existing file. *)
let write_file ~file cache =
  Bin_prot_utils_eio.write_bin_prot'
    file
    [%bin_writer: persistent_form]
    (to_persistent cache)
;;

(** [read_file ~file] loads a cache previously written with {!write_file}. *)
let read_file ~file =
  of_persistent (Bin_prot_utils_eio.read_bin_prot' file [%bin_reader: persistent_form])
;;

(*--- 0-c. Public API --------------------------------------------------*)
(** [find_or_add t key ~ttl ~default] returns the value associated with [key]
    if it is present *and* younger than [ttl].  Otherwise it evaluates
    [default ()], stores the result under [key] together with the supplied
    [ttl], and returns it.

    This wrapper is a direct re-export of {!Ttl_lru_cache.find_or_add} so that
    callers do not need to reference the underlying implementation module. *)
let find_or_add t key ~ttl ~default = LRU.find_or_add t key ~ttl ~default

(* Convenience wrappers for loading / saving the cache on disk that are
     aware of a fallback [~max_size] when no cache file is present.  This
     removes boiler-plate from callers such as the driver functions. *)

(** [load ~file ~max_size ()] returns a cache backed by the on-disk file
    [file].  If the file does not exist the function returns a fresh
    cache built with {!create ~max_size}. *)
let load ~file ~max_size () =
  if Eio.Path.is_file file then read_file ~file else create ~max_size ()
;;

(** [save ~file cache] writes [cache] to disk using {!write_file}. *)
let save ~file t = write_file ~file t
