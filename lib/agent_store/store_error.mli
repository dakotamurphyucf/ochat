(** Typed failures produced by the durable agent store. *)

type t =
  | Locked of string option
  | Missing of string
  | Schema_too_new of int
  | Migration_required of int
  | Corrupt of string
  | Io of
      { operation : string
      ; path : string
      ; message : string
      }
[@@deriving sexp]

(** [of_exn ~operation ~path exn] redacts an exception into a store I/O error. *)
val of_exn : operation:string -> path:string -> exn -> t

(** [to_protocol_error t] maps a store failure to a stable transport error. *)
val to_protocol_error : t -> Agent_protocol.Error.t
