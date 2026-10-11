open Core
module Owner = Agent_session.Prompt_source_owner
module Receipt = Agent_store.Prompt_source_receipt
module Builder = Agent_session.Prompt_revision_builder
module Catalog = Agent_session.Prompt_catalog

val ok : ('a, 'error) Result.t -> 'a

val with_workspace
  :  (Eio_unix.Stdenv.base
      -> Eio.Fs.dir_ty Eio.Path.t
      -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
      -> unit)
  -> unit

val save : _ Eio.Path.t -> string -> string -> unit
val definition : ?enabled:bool -> _ Eio.Path.t -> Agent_session.Prompt_definition.t
val receipt : Agent_session.Prompt_definition.t -> Receipt.t
val limits : ?leases:int -> unit -> Owner.Limits.t
val parser_limits : unit -> Prompt.Chat_markdown.Limits.t

val open_owner
  :  Eio.Switch.t
  -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> Agent_session.Prompt_definition.t
  -> Owner.t

val capture
  :  ?checkpoint:(unit -> unit)
  -> Eio_unix.Stdenv.base
  -> Owner.Scope.t
  -> Agent_session.Prompt_definition.t
  -> (Owner.Capture.t, Owner.Error.t) Result.t

val root_text : Owner.Capture.t -> string
val error : ('a, Owner.Error.t) Result.t -> Owner.Error.code -> unit
val catalog : Eio_unix.Stdenv.base -> _ Eio.Path.t -> Catalog.t

val prepare
  :  ?checkpoint:(unit -> unit)
  -> Catalog.t
  -> Eio.Switch.t
  -> Agent_session.Prompt_definition.t list
  -> Catalog.prepared Or_error.t

val selection
  :  Catalog.t
  -> Agent_session.Prompt_definition.t
  -> Catalog.Source_selection.t

val revision
  :  Catalog.t
  -> Agent_session.Prompt_definition.t
  -> Agent_session.Prompt_revision.t

val descriptors : Eio_unix.Stdenv.base -> int
