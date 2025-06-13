open Core

module Token : sig
  (** Minimal access-token structure that the server returns. *)

  type t =
    { access_token : string [@key "access_token"]
    ; token_type : string [@key "token_type"]
    ; expires_in : int [@key "expires_in"]
    ; obtained_at : float [@key "obtained_at"]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

module Metadata : sig
  (** Portion of the OAuth 2.0 Authorization-Server Metadata document that we
      actually need for the happy-path client. *)

  type t =
    { issuer : string [@key "issuer"]
    ; authorization_endpoint : string [@key "authorization_endpoint"]
    ; token_endpoint : string [@key "token_endpoint"]
    ; registration_endpoint : string option
          [@key "registration_endpoint" "registration_endpoint"]
    }
  [@@deriving sexp, bin_io, jsonaf]
end

(** {1 Dynamic Client Registration types} *)

module Client : sig
  (** Minimal representation of a registered OAuth client.
      We only store what is required for RFC 7591 happy-path: a generated
      [client_id] and, for confidential clients, an optional
      [client_secret].  For completeness we also track [client_name] and
      the timestamp when the ID was issued so that we can expire
      credentials if desired later on. *)

  type t =
    { client_id : string [@key "client_id"]
    ; client_secret : string option [@key "client_secret"]
    ; client_name : string option [@key "client_name"] [@jsonaf.option]
    ; redirect_uris : string list option [@key "redirect_uris"] [@jsonaf.option]
    ; client_id_issued_at : int option [@key "client_id_issued_at"] [@jsonaf.option]
    ; client_secret_expires_at : int option
          [@key "client_secret_expires_at"] [@jsonaf.option]
    }
  [@@deriving sexp, bin_io, jsonaf]
end
