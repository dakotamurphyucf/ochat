(** Singleton registry holding built-in grammars for highlighting. *)

(** Returns a registry pre-populated with built-in grammars. The value is
      constructed on first call and reused thereafter. *)
val get : unit -> Highlight_tm_loader.registry
