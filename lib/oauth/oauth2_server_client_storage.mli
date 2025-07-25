open! Core
module Client = Oauth2_server_types.Client

(** In-memory storage for OAuth 2.0 *Dynamic Client Registration* records.

    The table lives for the duration of the process only; no state is
    persisted to disk.  It is therefore suitable for unit tests,
    integration tests, prototypes and single-instance deployments where
    durability is not a requirement.

    {1 Behaviour}

    • Each entry is keyed by the server-generated [client_id].
    • Calls that mutate the table are thread-safe because
      {!Core.Hashtbl} is safe for concurrent reads and writes as long as
      no function passed to accessors mutates the table recursively.  We
      never do that here.
    • A set of deterministic development credentials is pre-populated at
      start-up.  They can be overridden via the environment variables
      [MCP_DEV_CLIENT_ID] and [MCP_DEV_CLIENT_SECRET].
*)

(** [register ?client_name ?redirect_uris ?confidential ()] inserts and
    returns a freshly generated client.

    – When [?confidential = false] (default) the function creates a **public
      client**: no secret is stored, and the returned record contains
      [client_secret = None].  This corresponds to the PKCE-only model
      recommended for native and JavaScript apps.
    – When [?confidential = true] a random secret is generated with
      {!Mirage_crypto_rng.generate} / {!Base64.encode_string} and stored
      alongside the client.  The secret **never expires** because
      [client_secret_expires_at] is set to [None].

    A monotonic Unix timestamp (seconds) is recorded in
    [client_id_issued_at].  Multiple calls never raise – identifiers are
    generated with cryptographic randomness and are therefore
    statistically unique.

    Returns the complete {!Client.t} record so callers can persist or
    serialise it.

    Example creating a *public* client:
    {[
      let open Oauth2 in
      let client = Oauth2_server_client_storage.register ~client_name:"CLI" () in
      Option.is_none client.client_secret  (* = true *)
    ]}
*)
val register
  :  ?client_name:string
  -> ?redirect_uris:string list
  -> ?confidential:bool
  -> unit
  -> Client.t

(** [find client_id] returns the stored client for [client_id] or
    [None] if the identifier is unknown.  The lookup is O(1). *)
val find : string -> Client.t option

(** [validate_secret ~client_id ~client_secret] checks whether credentials
    are valid.

    The result is [true] iff a matching client exists and one of the
    following holds:
    • the stored entry is public ([client_secret = None]) and the caller
      also supplied [None]; or
    • the stored secret exactly equals the supplied value.

    Any other combination (missing client, mismatching secret, supplying a
    secret for a public client, …) yields [false]. *)
val validate_secret : client_id:string -> client_secret:string option -> bool

(** [insert_fixed ~client_id ~client_secret ?client_name ?redirect_uris ()]
    stores a deterministic **confidential client**.

    Useful for development, automated tests or smoke set-ups where client
    credentials must remain stable across restarts.  If an entry with the
    same [client_id] already exists it is silently replaced.

    The returned record is identical to what {!register} would return in
    confidential mode, except that the identifier and secret come from the
    caller.

    Example:
    {[
      let client =
        Oauth2_server_client_storage.insert_fixed
          ~client_id:"dev-client"
          ~client_secret:"dev-secret"
          ()
      in
      assert (client.client_secret = Some "dev-secret")
    ]}
*)
val insert_fixed
  :  client_id:string
  -> client_secret:string
  -> ?client_name:string
  -> ?redirect_uris:string list
  -> unit
  -> Client.t

(** [pp_table fmt ()] prints a human-readable dump of the current table –
    one client per line – useful for debugging and REPL sessions.  Secrets
    are shown verbatim, public clients display the Unicode symbol [∅]. *)
val pp_table : Format.formatter -> unit -> unit
