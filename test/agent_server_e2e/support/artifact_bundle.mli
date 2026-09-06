open Core

(** Sanitized text artifacts retained for failed E2E scenarios. *)

type t

(** [create ~fs ~root ~secrets] creates a writer rooted at [root].
    Every exact secret and its Base64 encoding is redacted before persistence. *)
val create : fs:Eio.Fs.dir_ty Eio.Path.t -> root:string -> secrets:string list -> t

(** [register_secret t secret] adds [secret] and its Base64 encoding to all
    subsequent redaction and validation. *)
val register_secret : t -> string -> unit

(** Sanitize text destined for diagnostics as well as saved artifacts. *)
val redact : t -> string -> string

(** [write_text t ~name ~contents] writes one redacted regular file. [name]
    must be a nonempty basename other than [.] or [..]. *)
val write_text : t -> name:string -> contents:string -> (unit, Error.t) result

(** [validate_redaction t] verifies that no configured secret variant appears
    in a regular file directly beneath the bundle root. *)
val validate_redaction : t -> (unit, Error.t) result

(** [preserve t ~destination] validates and copies direct regular-file
    artifacts into a new private directory at [destination]. *)
val preserve : t -> destination:string -> (unit, Error.t) result
