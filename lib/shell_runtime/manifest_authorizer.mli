open! Core

(** Bootstrap authorization for a complete canonical shell manifest. *)

type request =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; summary : string
  }

type response =
  | Authorize_once
  | Reject of string

type grant = { manifest_sha256 : string } [@@deriving sexp, compare, equal]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

type t = request -> response

(** [authorize t manifest] requests a one-use grant bound to [manifest]. *)
val authorize : t -> Chatmd_shell_spec.Manifest.t -> (grant, error) result

(** [verify grant manifest] checks that [grant] authorizes exactly [manifest]. *)
val verify : grant -> Chatmd_shell_spec.Manifest.t -> (unit, error) result

(** [assume_authorized] authorizes every manifest. Use only in tests and
    explicitly trusted embedding environments. *)
val assume_authorized : t

(** [deny] rejects every manifest. *)
val deny : t
