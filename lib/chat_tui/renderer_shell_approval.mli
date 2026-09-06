(** Approval interaction overlay rendered above any active page. *)

(** [overlay ~size ~model base] renders modal prompts as wrapped logical lines,
    preserving internal blank lines. Single-row labels replace line breaks with
    spaces; sanitized newlines never reach the single-line Notty constructor. *)
val overlay : size:int * int -> model:Model.t -> Notty.I.t -> Notty.I.t

(** [cursor ~size ~model] returns the modal-owned text cursor when the active
    interaction contains an editor. Moderator text input follows every rendered
    prompt row, including explicit line breaks and width-dependent wrapping. *)
val cursor : size:int * int -> model:Model.t -> (int * int) option
