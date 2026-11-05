(** Bounded immutable command handoff DATA under its actual Invocation owner.
    No decoder creates custody. Historical absence keeps legacy behavior. *)
module Phase : sig
  type t =
    | Staged
    | Accepted of Timestamp.t
    | Dispatch_claimed of
        { accepted_at : Timestamp.t
        ; claimed_at : Timestamp.t
        }
    | Discarded of string
    | Rejected of Error.t
  [@@deriving equal, sexp_of]
end

type t [@@deriving equal, sexp]

val persistence_semantics : string

val create
  :  request:Goal_resume_request.t
  -> principal_id:Id.Principal.t
  -> (t, Error.t) result

val request : t -> Goal_resume_request.t
val principal_id : t -> Id.Principal.t
val phase : t -> Phase.t
val accept : t -> now:Timestamp.t -> (t, Error.t) result
val claim : t -> now:Timestamp.t -> (t, Error.t) result
val discard : t -> reason:string -> (t, Error.t) result
val reject : t -> Error.t -> (t, Error.t) result
val validate_update : t -> incoming:t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Every validated intent reserves 3072 phase bytes within its 8192-byte carrier.
    Retained phase encodings are bounded to 2048 bytes. [discard] and [reject]
    preserve a bounded reason/error; oversized error details become an explicit
    summary preserving code and retryability. Callers retain the original primary
    error separately. Claim preserves the exact accepted timestamp; wallclock
    ordering is not an invariant. *)
