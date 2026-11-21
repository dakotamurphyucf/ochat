(** Read one ordinary user or assistant text part from an exact recovery
    occurrence. Offsets count UTF-8 bytes in the original part. *)
val method_name : string

val feature : string

type t = private
  { citation : Recovery_window.Citation.t
  ; preparation_revision : int64 option
  ; part : int
  ; start : int
  ; stop : int
  ; max_bytes : int
  }

(** Effective citations require their original nonnegative preparation revision.
    The half-open range is bounded by the maximum admitted text part size. *)
val create
  :  Recovery_window.Citation.t
  -> ?preparation_revision:int64
  -> part:int
  -> start:int
  -> stop:int
  -> ?max_bytes:int
  -> unit
  -> (t, Protocol_error.t) result

val session : t -> Session_ref.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result

module Reply : sig
  type request := t

  type t = private
    { text : string
    ; next_offset : int option
    }

  val of_slice : History_text.Slice.t -> t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result

  (** Require the bytes to reach either the requested stop or the exact next
      offset. A truncated response must advance within its selected range. *)
  val validate : t -> request:request -> (unit, Protocol_error.t) result
end
