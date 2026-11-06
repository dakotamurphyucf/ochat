(** Bounded native initial-episode handoff DATA. Decoding supplies no custody,
    admission ACK or execution permission. *)
module Phase : sig
  type t =
    | Staged
    | Accepted of Timestamp.t
    | Dispatch_claimed of
        { accepted_at : Timestamp.t
        ; claimed_at : Timestamp.t
        }
    | Admitted of
        { accepted_at : Timestamp.t
        ; claimed_at : Timestamp.t
        ; admission : Run_receipt.t
        }
    | Discarded of string
    | Rejected of Error.t
  [@@deriving equal, sexp_of]
end

type t [@@deriving equal, sexp_of]

val persistence_semantics : string

val create
  :  request:Goal_native_start_request.t
  -> principal_id:Id.Principal.t
  -> (t, Error.t) result

val request : t -> Goal_native_start_request.t
val principal_id : t -> Id.Principal.t
val phase : t -> Phase.t
val accept : t -> now:Timestamp.t -> (t, Error.t) result
val claim : t -> now:Timestamp.t -> (t, Error.t) result

(** Only Dispatch_claimed -> Admitted, retaining both exact timestamps. Checks
    Admission kind, command digest, source, key, principal, initial run revision 0
    and positive Session revision. The Actor must separately prove the actual
    preparation/Run/link and commit the association in the admission candidate.
    Wallclock ordering is not required. Admitted cannot discard/reject/rearm;
    later activation failure is separate Attempt/Status data. *)
val admit : t -> admission:Run_receipt.t -> (t, Error.t) result

val discard : t -> reason:string -> (t, Error.t) result
val reject : t -> Error.t -> (t, Error.t) result
val validate_update : t -> incoming:t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Real Input remains bounded by its codec. The intent has 12288 bytes beyond
    that input bound and depth 66, with 6144 phase bytes reserved on every entry;
    actual phases are at most 4096 bytes/depth 12. Discard/reject bound retained
    diagnostics, preserving error code/retryability in an explicit summary when
    necessary. Complete containing document limits remain authoritative. *)
