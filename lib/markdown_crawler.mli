open! Core

val crawl : root:_ Eio.Path.t -> f:(doc_path:string -> markdown:string -> unit) -> unit
