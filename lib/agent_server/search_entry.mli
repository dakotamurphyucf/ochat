(** Rebuildable readable projection of one already authorized public entry.
    Retains only canonical user/assistant text/refusal strings and host identity;
    never retains a provider payload, image URI, annotation or hidden body.
    Initial authored prefix exclusion belongs to the owning session reader. *)
type t

module Part : sig
  type t = private
    { index : int
    ; text : string
    }
end

(** At most 4096 original parts and 1 MiB of selected UTF-8 text per entry.
    Every selected part is validated before admission. An excluded entry returns
    None; malformed/oversized selected content is an explicit error. *)
val of_public
  :  Agent_protocol.Public_history.t
  -> (t option, Agent_protocol.Error.t) result

val history_id : t -> Agent_protocol.History.Id.t
val content_revision : t -> Agent_protocol.History.Content_revision.t
val parts : t -> Part.t list

(** Conservative cache accounting: selected string bytes plus bounded per-part
    and per-entry bookkeeping. This is a budget unit, not an exact heap census. *)
val accounted_bytes : t -> int

(** Join selected parts with newlines into a 2048-byte UTF-8 prefix. Truncation
    is explicit; no unbounded intermediate concatenation is allocated. *)
val navigation_context
  :  t
  -> (Agent_protocol.Search_navigation.Entry.t, Agent_protocol.Error.t) result
