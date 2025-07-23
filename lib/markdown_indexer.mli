open! Core

val index_directory :
  ?vector_db_root:string ->
  env:Eio_unix.Stdenv.base ->
  index_name:string ->
  description:string ->
  root:_ Eio.Path.t ->
  unit

