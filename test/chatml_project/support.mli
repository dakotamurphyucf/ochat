open Core

(** Test captures are in-memory and retain their exact supplied bytes. Creating a
    capture uses a separate no-op checkpoint, so project checkpoint observations
    count only the decode/select operation under examination. *)
val capture : ?path:string -> ?namespace:string -> string -> Chatml_source.t

(** Fail a test with the typed schema error when a known-valid fixture rejects. *)
val decode_exn : Chatml_source.t -> Chatml_project.t

val select_exn
  :  Chatml_project.t
  -> request:Chatml_project.Selection.request
  -> Chatml_project.Selection.t

(** Compare source identity, exact contents and content digest. Physical identity
    is deliberately not the provenance oracle. *)
val assert_source : Chatml_source.t -> actual:Chatml_source.t -> unit

(** Verify a declaration owns the complete supplied capture, including its
    validated byte range and digest. Assert the separately supplied JSON path;
    never pretend the API provides a field-token byte range. *)
val assert_declared_at
  :  Chatml_source.t
  -> actual:Chatml_project.Declared_at.t
  -> field_path:string list
  -> unit

(** First observe every cooperative checkpoint on a successful baseline run.
    Repeat with a private exception injected at each observed poll; assert it
    escapes unchanged and the operation stops at that poll. Does not assert
    internal interruptibility of the bounded JSON parse stage or fixed counts. *)
val assert_checkpoint_propagation : f:((unit -> unit) -> unit) -> unit
