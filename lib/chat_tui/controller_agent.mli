(** [handle_key ~model ~term event] handles Agent-page navigation only.
    Unsupported input leaves hidden Chat state unchanged. *)
val handle_key
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> Controller_types.reaction

module For_testing : sig
  (** [handle_key ~model ~size event] handles an Agent-page key using
      [size] as the terminal geometry source. *)
  val handle_key
    :  model:Model.t
    -> size:(unit -> int * int)
    -> Notty.Unescape.event
    -> Controller_types.reaction
end
