(** Strict decoding for single and batched HTTP RPC request bodies. *)

type t =
  | Single of Agent_protocol.Envelope.t
  | Batch of Agent_protocol.Envelope.t list

(** [parse ~max_batch_size body] validates UTF-8, JSON structure, a nesting
    limit of 64, duplicate fields, batch size, and every protocol envelope
    before returning work to the HTTP dispatcher. *)
val parse : max_batch_size:int -> string -> (t, Agent_protocol.Error.t) result
