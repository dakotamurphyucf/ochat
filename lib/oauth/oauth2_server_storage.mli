open! Core
module Token = Oauth2_server_types.Token

(** In-memory OAuth 2.0 access-token store.

    This module underpins the token-introspection logic of a *minimal* OAuth
    2.0 Authorisation Server.  It maintains an in-process
    [`Hashtbl`](https://ocaml.org/p/core/latest/doc/core/Core/Hashtbl.html)
    mapping from the raw [access_token] string to its absolute expiration
    time.

    The store offers two constant-time operations:

    • {!insert} – register a freshly issued token
    • {!find_valid} – validate a token and lazily evict it once expired

    Because everything lives in a single OCaml process the implementation is
    suitable for unit tests, demos or single-node deployments only.  It is
    *not* thread-safe and provides no persistence or replication.
*)

(** [insert token] adds [token.access_token] to the store and records its
    expiry ([token.expires_in] seconds from the current clock).  Existing
    entries with the same key are overwritten. *)
val insert : Token.t -> unit

(** [find_valid access_token] returns [true] iff [access_token] is present and
    not expired.  If the token has expired it is removed and the function
    returns [false]. *)
val find_valid : string -> bool

(** [pp_table fmt ()] pretty-prints the current table for debugging.  Do not
    rely on the exact output format. *)
val pp_table : Format.formatter -> unit -> unit
