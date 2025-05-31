open Types
module Res_item = Openai.Responses.Item

(** [pair_of_item item] converts an OpenAI response [item] into a
    printable [(role * text)] tuple that can be fed into the renderer.
    Returns [None] for items that do not have a textual representation. *)
val pair_of_item : Res_item.t -> message option

(** Convert a whole list of response items into renderable messages. *)
val of_history : Res_item.t list -> message list
