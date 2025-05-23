(** Rendering helpers – pure functions turning chat **Model** data into Notty
    images.  This module owns the colour scheme and all layout/wrapping
    concerns. *)

open Types

(** Map chat role → display attribute. *)
val attr_of_role : role -> Notty.A.t

(** Convert a single `(role * text)` message into a wrapped Notty image.
    [max_width] is the terminal width available for the image. *)
val message_to_image : max_width:int -> message -> Notty.I.t

(** Render the full message list into a single vertically concatenated image.
    Wrapping is applied to every line. *)
val history_image : width:int -> messages:message list -> Notty.I.t

(** [render_full ~size:(w,h) ~model] returns a tuple [(img, cursor)] where
    [img] is the complete terminal image (history viewport + input editor)
    and [cursor] is the absolute cursor position to be fed into
    [Notty_eio.Term.cursor].  The function is pure – it only reads from the
    immutable snapshot of [model] that is passed in. *)
val render_full : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
