open Eio

val ( / ) : ([> Fs.dir_ty ] as 'a) Path.t -> string -> 'a Path.t

(** [to_res f] converts the result of function [f] to a [Result.t] type.
    It returns [Ok (f ())] if [f] executes successfully, and [Error string] if an exception is raised. *)
val to_res : (unit -> 'a) -> ('a, string) result

(** [log ~dir ?(file = "./logs.txt") s] appends the string [s] to the log file [file] in directory [dir].
    If the log file does not exist, it will be created with permissions 0o600. *)
val log : dir:Eio.Fs.dir_ty Eio.Path.t -> ?file:string -> string -> unit

(** [console_log ~stdout log] writes the string [log] to the standard output [stdout] . *)
val console_log : stdout:[> Flow.sink_ty ] Resource.t -> string -> unit

(** [save_doc ~dir file p] saves the content of [p] to the file [file] in directory [dir].
    If the file does not exist, it will be created with permissions 0o777. *)
val save_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string -> unit

(** [append_doc ~dir file p] appends the content of [p] to the file [file] in directory [dir].
    If the file does not exist, it will be created with permissions 0o777. *)
val append_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string -> unit

(** [load_doc ~dir file] loads the content of the file [file] in directory [dir] and returns it. *)
val load_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string

(** [delete_doc ~dir file] deletes the file [file] in directory [dir]. *)
val delete_doc : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> unit

(** [mkdir ~dir path] creates a directory at the given [path] in directory [dir]. *)
val mkdir : ?exists_ok:bool -> dir:Eio.Fs.dir_ty Eio.Path.t -> string -> unit

(** [directory ~dir path] reads the directory at the given [path] and returns a string list of all the file and path names in the directory *)
val directory : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string list

(** [is_dir ~dir path] checks if the given [path] in directory [dir] is a directory and returns a boolean value. *)
val is_dir : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> bool

(** [with_dir ~dir f] opens the directory [dir] and applies the function [f] to it. *)
val with_dir : dir:[> Fs.dir_ty ] Path.t -> ([ `Close | `Dir ] Path.t -> 'a) -> 'a

module Net : sig
  (** [get_host url] extracts the host from the given [url] and returns it. *)
  val get_host : string -> string

  (** [get_path url] extracts the path from the given [url] and returns it. *)
  val get_path : string -> string

  val tls_config : Tls.Config.client
  val empty_headers : Http.Header.t

  type _ response =
    | Raw : (Http.Response.t * Cohttp_eio.Body.t -> 'a) -> 'a response
    | Default : string response

  (** [post res_typ ~net ~host ~headers ~path body] sends an HTTP POST request with the given parameters and returns the response. *)
  val post
    :  'a response
    -> net:_ Eio.Net.t
    -> host:string
    -> headers:Http.Header.t
    -> path:string
    -> string
    -> 'a

  (** [get res_typ ~net ~host ?headers path] sends an HTTP GET request with the given parameters and returns the response. *)
  val get
    :  'a response
    -> net:_ Eio.Net.t
    -> host:string
    -> ?headers:Http.Header.t
    -> string
    -> 'a

  (** [download_file net url ~dir ~filename] downloads the file from the given [url] and saves it to the specified [dir] and [filename]. *)
  val download_file
    :  _ Eio.Net.t
    -> string
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> filename:string
    -> unit
end

module type Task_pool_config = sig
  type input
  type output

  val dm : Domain_manager.ty Resource.t
  val stream : (input * output Eio.Promise.u) Eio.Stream.t
  val sw : Eio.Switch.t
  val handler : input -> output
end

(** [Task_pool] is a functor that creates a task pool with the given configuration. *)
module Task_pool : functor (C : Task_pool_config) -> sig
  val spawn : string -> unit
  val submit : C.input -> C.output
end

(** [run_main f] runs the main function [f] with the Eio environment. *)
val run_main : (Eio_unix.Stdenv.base -> 'a) -> 'a

module Server : sig
  open Eio

  val traceln : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
  val handle_client : [> `Flow | `R | `W ] Resource.t -> [< Eio.Net.Sockaddr.t ] -> unit
  val run : _ Eio.Net.listening_socket -> 'a
end

module Client : sig
  val traceln : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

  val run
    :  net:_ Eio.Net.t
    -> clock:_ Eio.Time.clock
    -> addr:Eio.Net.Sockaddr.stream
    -> unit
end

module Run_server : sig
  val main : net:_ Eio.Net.t -> clock:_ Eio.Time.clock -> unit
  val run : unit -> unit
end

module Base64 : sig
  val file_to_data_uri : dir:Eio.Fs.dir_ty Eio.Path.t -> string -> string
end
