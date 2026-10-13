open Core

(** A per-engine bounded cache for an immutable {!Highlight_theme.t}. The
    resolver may be shared between domains: its cache is an atomic immutable
    snapshot, and a missed compare-and-set never delays rendering. At most
    4096 entries and 256 KiB of encoded keys are retained per resolver, plus
    O(entry count) map and attribute overhead. An oversized or saturated key
    is resolved without caching. *)
type t

val create : theme:Highlight_theme.t -> t

(** Scope order and duplicates do not affect lookup. The key encoding keeps
    distinct scope groups distinct even if they contain dots or separators. *)
val attr_of_scopes : t -> scopes:string list -> Notty.A.t
