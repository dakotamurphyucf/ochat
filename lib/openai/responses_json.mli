(** Strict JSON number grammar shared by request validation and lossless wire
    capture. Never use permissive JSON parsing to validate numeric lexemes. *)
val valid_number : string -> bool
