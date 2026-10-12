(** Shared conversion implementation, private to webpage_markdown. Legacy owns
    its best-effort parsing policy; bounded callers use strict admission. *)
val convert : Conversion_budget.t -> Soup.soup Soup.node -> Omd.doc
