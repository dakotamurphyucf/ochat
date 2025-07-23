open! Core

val index_directory :
  ?vector_db_root:string ->
  index_name:string ->
  description:string ->
  root:_ Eio.Path.t ->
  unit

