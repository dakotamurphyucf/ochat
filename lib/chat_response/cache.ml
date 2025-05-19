open Core
module CM = Prompt_template.Chat_markdown

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

let create ~max_size () = LRU.create ~max_size ()
let to_persistent lru = { max_size = LRU.max_size lru; items = LRU.to_alist lru }

let of_persistent pf =
  let cache = create ~max_size:pf.max_size () in
  List.iter pf.items ~f:(fun (k, v) -> LRU.set cache ~key:k ~data:v);
  cache
;;

(* Binary serialisation on disk – deterministic and compact. *)
let write_file ~file cache =
  Bin_prot_utils_eio.write_bin_prot'
    file
    [%bin_writer: persistent_form]
    (to_persistent cache)
;;

let read_file ~file =
  of_persistent (Bin_prot_utils_eio.read_bin_prot' file [%bin_reader: persistent_form])
;;

(*--- 0-c. Public API --------------------------------------------------*)
let find_or_add t key ~ttl ~default = LRU.find_or_add t key ~ttl ~default

(* Convenience wrappers for loading / saving the cache on disk that are
     aware of a fallback [~max_size] when no cache file is present.  This
     removes boiler-plate from callers such as the driver functions. *)

let load ~file ~max_size () =
  if Eio.Path.is_file file then read_file ~file else create ~max_size ()
;;

let save ~file t = write_file ~file t
