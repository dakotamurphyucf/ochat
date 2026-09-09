(** Conservative substring reference scan for a bounded set of typed candidate
    blob IDs. Callers must validate authoritative storage before treating absence
    as proof; this scanner grants no deletion authority on its own. *)
type t

val create : Agent_protocol.Id.Blob.t list -> (t, Agent_protocol.Error.t) result

(** Start a distinct root. Previously found references remain, while the partial
    token suffix is reset so unrelated files cannot form a synthetic reference. *)
val begin_root : t -> unit

(** Feed successive byte chunks from one validated root. IDs split across chunks
    are retained. [ignore] may suppress a blob's reference to itself while scanning
    its own metadata/data; it must be consistent across chunks from that root.
    Embedded IDs and prefixes are deliberately retained conservatively. *)
val feed : ?ignore:Agent_protocol.Id.Blob.t -> t -> string -> unit

val referenced : t -> Agent_protocol.Id.Blob.t -> bool
val references : t -> Agent_protocol.Id.Blob.t list
