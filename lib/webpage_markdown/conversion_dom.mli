(** Admits normalized HTML signals before constructing the Soup DOM. Parser
    internal token/recovery work is charged through its byte stream; its own
    transient third-party data is not claimed to obey the retained-node quota. *)
val parse : Conversion_budget.t -> string -> Soup.soup Soup.node
