type doc = { id : int; text : string }

type t

val tokenize : string -> string list

val create : doc list -> t

val query : t -> text:string -> k:int -> (int * float) list

val write_to_disk : Eio.Fs.dir_ty Eio.Path.t -> t -> unit

val read_from_disk : Eio.Fs.dir_ty Eio.Path.t -> t

val dump_debug : t -> unit
