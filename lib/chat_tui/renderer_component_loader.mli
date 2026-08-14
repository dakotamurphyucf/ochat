(** [status_text model] returns the active operation's label. Tool execution
    is labelled as working, assistant text generation as writing, and other
    assistant activity as thinking. *)
val status_text : Model.t -> string option

(** [render ~base_attr ~frame text] renders an eased snake highlight across
    [text]. The highlighted prefix grows until the whole label is bright,
    then its tail advances until only the final character remains. *)
val render : base_attr:Notty.A.t -> frame:int -> string -> Notty.I.t
