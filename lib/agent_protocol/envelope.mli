(** Ochat JSON-RPC-style transport envelopes. *)

module Request_id : sig
  type t [@@deriving compare, sexp]

  (** [of_json json] accepts a string or number request identifier. *)
  val of_json : Jsonaf.t -> (t, Error.t) result

  (** [to_json t] returns the exact JSON identifier represented by [t]. *)
  val to_json : t -> Jsonaf.t
end

type request =
  { id : Request_id.t
  ; method_ : string
  ; params : Jsonaf.t
  }
[@@deriving sexp]

type notification =
  { method_ : string
  ; params : Jsonaf.t
  }
[@@deriving sexp]

type response =
  { id : Request_id.t
  ; outcome : (Jsonaf.t, Error.t) result
  }
[@@deriving sexp]

type t =
  | Request of request
  | Notification of notification
  | Response of response
[@@deriving sexp]

(** [request ~id ~method_ ?params ()] creates a request envelope. *)
val request : id:Request_id.t -> method_:string -> ?params:Jsonaf.t -> unit -> t

(** [notification ~method_ ?params ()] creates a notification envelope. *)
val notification : method_:string -> ?params:Jsonaf.t -> unit -> t

(** [success ~id result] creates a successful response envelope. *)
val success : id:Request_id.t -> Jsonaf.t -> t

(** [failure ~id error] creates a failed response envelope. *)
val failure : id:Request_id.t -> Error.t -> t

(** [to_json t] encodes [t] as an Ochat JSON-RPC envelope. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes one Ochat JSON-RPC envelope. *)
val of_json : Jsonaf.t -> (t, Error.t) result
