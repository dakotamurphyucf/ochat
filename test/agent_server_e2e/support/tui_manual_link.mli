(** Eio-owned byte relay for explicitly requested manual connection faults.
    Neither transcripts nor credentials are parsed or retained. *)
type t

(** [start ~sw ~env ~listen_path ~upstream] binds a new socket and relays accepted
    flows to [upstream]. The caller owns the private listener directory. Existing
    nodes are never unlinked or reused. Switch cleanup closes all owned flows. *)
val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> listen_path:string
  -> upstream:string
  -> t

(** [cut t] closes active relayed connections and rejects new connections until
    resumed. Connections racing a cut cannot forward after a later resume. *)
val cut : t -> unit

(** [resume t] accepts new connections without reviving old flows. *)
val resume : t -> unit

val is_available : t -> bool

(** [start_tcp ~sw ~env ~upstream_port] binds an ephemeral IPv4 loopback port
    and forwards to the specified loopback port. Returns the relay and its bound
    port without releasing the listener. HTTP is relayed unchanged: cut affects
    all flows on this relay, including both POST connections and SSE streams. *)
val start_tcp
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> upstream_port:int
  -> t * int

(** [connection_count t] counts accepted upstream connection attempts and active
    relays. Closed relays disappear after their Eio fibers finish cleanup. *)
val connection_count : t -> int
