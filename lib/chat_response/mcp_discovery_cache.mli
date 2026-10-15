(** One discovery cache per connected MCP declaration, never process-global.
    Its loader closes over exactly one authenticated client and tool filter.
    Different declarations, credentials, transports or runtimes cannot share
    entries or invalidation. The owning runtime switch bounds client/listener
    lifetime. Expiry uses the host's Eio clock; failed/cancelled loads are not
    cached, and their exceptions propagate only after the mutex is unlocked so
    subsequent discovery and invalidation remain usable. *)
type 'a t

val create : now:(unit -> float) -> load:(unit -> 'a) -> 'a t
val get : 'a t -> 'a

(** Like [get], but call [before_load] under this cache's mutex immediately
    before an actual loader call (including an invalidated retry). A cache hit
    does not call it. The check is borrowed for this call only and is never
    retained by the cache, so a construction-time guard cannot become an
    invocation-time authority. A rejected check leaves the cache unchanged;
    loader exceptions keep their original backtrace after unlocking. *)
val get_checked : 'a t -> before_load:(unit -> (unit, 'e) result) -> ('a, 'e) result

val invalidate : 'a t -> unit
