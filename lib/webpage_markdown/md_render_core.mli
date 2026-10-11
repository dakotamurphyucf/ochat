(** Shared rendering implementation. Text pieces defer allocation until final
    admitted output; the legacy wrapper supplies an unbounded context. *)
val to_string : Conversion_budget.t -> Omd.doc -> string
