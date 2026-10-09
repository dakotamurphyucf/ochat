(** One ephemeral, already-authorized canonical state snapshot. It owns no IO or
    execution resources and must not outlive a search request. The disposable
    cache receives only safe Search_entry projections. *)
type t

val create
  :  server_id:Agent_protocol.Id.Server.t
  -> Agent_session.Session_state.t
  -> (t, Agent_protocol.Error.t) result

val session : t -> Agent_protocol.Session_ref.t
val generation : t -> int
val revision : t -> int64
val length : t -> int

(** O(bounded canonical source length), excluding the initial authored prefix.
    This lookup grants no visibility; project and reauthorize before disclosure. *)
val index_of_id : t -> Agent_protocol.History.Id.t -> int option

module Window : sig
  type t = private
    { entries : Search_entry.t option list
    ; next_index : int
    ; scanned_entries : int
    ; scanned_bytes : int
    ; reached_end : bool
    }
end

(** Examine at most 512 canonical positions, counting excluded entries too.
    The initial authored prefix is never projected. Total source positions are
    bounded to 65536; seeking is O(source length), projection O(examined bytes).
    Each admitted raw payload is bounded to 2 MiB before semantic decoding.
    max_bytes is a remaining per-page work budget (1–8 MiB); an entry which fits
    the source limit but not the remaining budget is left for the next page.
    Malformed/oversized source is an explicit error, never an empty shard.
    Scope/revision keys do not authorize reuse: caller revalidates before hits. *)
val project
  :  t
  -> principal:Agent_protocol.Principal.t
  -> cache:Search_cache.t
  -> offset:int
  -> limit:int
  -> max_bytes:int
  -> (Window.t, Agent_protocol.Error.t) result
