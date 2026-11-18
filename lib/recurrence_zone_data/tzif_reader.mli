open Core

module Version : sig
  type t =
    | V2
    | V3
  [@@deriving equal]
end

module Local_type : sig
  type t =
    { offset_seconds : int
    ; is_dst : bool
    ; designation : string
    }
  [@@deriving equal]
end

module Transition : sig
  type t =
    { at : int64
    ; local_type : Local_type.t
    }
end

(** Private validated structural representation. All second values are signed
    POSIX seconds since epoch; leap-bearing input is rejected. Raw input is not
    retained. The compatibility block is validated but not used for projection. *)
type t =
  { version : Version.t
  ; initial : Local_type.t
  ; transitions : Transition.t array
  ; footer : string
  }

val maximum_bytes : int
val maximum_transitions : int
val maximum_types : int
val maximum_designation_bytes : int
val maximum_footer_bytes : int
val of_string : string -> (t, Zone_data_error.t) Result.t
