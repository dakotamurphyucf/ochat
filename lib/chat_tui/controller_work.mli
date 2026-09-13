val handle_key
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> Controller_types.reaction

module For_testing : sig
  val handle_key
    :  model:Model.t
    -> size:(unit -> int * int)
    -> Notty.Unescape.event
    -> Controller_types.reaction
end
