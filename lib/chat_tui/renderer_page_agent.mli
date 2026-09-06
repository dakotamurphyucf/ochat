(** [render ~size ~model] renders the Agent page using its page-local scroll
    box. The Chat transcript and editor are not rendered or mutated. *)
val render : size:int * int -> model:Model.t -> Notty.I.t * (int * int)

module For_testing : sig
  (** [render_block_ids ~width ~height ~model ~render] prepares cached geometry
      and returns the stable IDs intersecting the current viewport. [render] is
      called only for stale blocks. *)
  val render_block_ids
    :  width:int
    -> height:int
    -> model:Model.t
    -> render:(Model.Agent_page_state.render_block -> Notty.I.t)
    -> int list
end
