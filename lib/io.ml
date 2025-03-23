module U = Unix
open Core
open Cohttp_eio
open Eio

let null_auth ?ip:_ ~host:_ _ =
  Ok None (* Warning: use a real authenticator in your code! *)
;;

let https ~authenticator =
  let tls_config = Tls.Config.client ~authenticator () |> Result.ok |> Option.value_exn in
  fun uri raw ->
    let host =
      Uri.host uri |> Option.map ~f:(fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw
;;

let ( / ) = Path.( / )

(** [to_res f] converts the result of function [f] to a [Result.t] type.
    It returns [Ok (f ())] if [f] executes successfully, and [Error string] if an exception is raised. *)
let to_res f =
  try Ok (f ()) with
  | ex -> Error (Fmt.str "%a" Eio.Exn.pp ex)
;;

(** [log ~dir ?(file = "./logs.txt") s] appends the string [s] to the log file [file] in directory [dir].
    If the log file does not exist, it will be created with permissions 0o600. *)
let log ~dir ?(file = "./logs.txt") s =
  let path = dir / file in
  Path.with_open_out ~create:(`If_missing 0o600) ~append:true path (fun flow ->
    Flow.copy_string s flow)
;;

(** [console_log ~env log] writes the string [log] to the standard output [stdout] in the given environment [env]. *)
let console_log ~stdout log = Eio.Flow.copy_string log stdout

(** [save_doc ~dir file p] saves the content of [p] to the file [file] in directory [dir].
    If the file does not exist, it will be created with permissions 0o777. *)
let save_doc ~dir file p =
  let path = dir / file in
  Path.save ~create:(`Or_truncate 0o777) path p
;;

(** [append_doc ~dir file p] appends the content of [p] to the file [file] in directory [dir].
    If the file does not exist, it will be created with permissions 0o777. *)
let append_doc ~dir file p =
  let path = dir / file in
  Path.save ~append:true ~create:(`If_missing 0o777) path p
;;

(** [load_doc ~dir file] loads the content of the file [file] in directory [dir] and returns it. *)
let load_doc ~dir file =
  let path = dir / file in
  Path.load path
;;

(** [is_dir ~dir path] checks if the given [path] in directory [dir] is a directory and returns a boolean value. *)
let is_dir ~dir path =
  Path.with_open_in (dir / path)
  @@ fun file ->
  match (File.stat file).kind with
  | `Directory -> true
  | _ -> false
;;

(** [with_dir ~dir f] opens the directory [dir] and applies the function [f] to it. *)
let with_dir ~dir f = Path.with_open_dir dir @@ f

module Net = struct
  (** [get_host url] extracts the host from the given [url] and returns it. *)
  let get_host url =
    let uri = Uri.of_string url in
    Uri.host_with_default uri
  ;;

  (** [get_path url] extracts the path from the given [url] and returns it. *)
  let get_path url =
    let uri = Uri.of_string url in
    Uri.path uri
  ;;

  let tls_config =
    let null ?ip:_ ~host:_ _certs = Ok None in
    Tls.Config.client ~authenticator:null () |> Result.ok |> Option.value_exn
  ;;

  let empty_headers = Http.Header.init ()
  (*
     (** [with_https_conn ~env ~host f] establishes an HTTPS connection with the given [host] and applies the function [f] to it. *)
  let with_https_conn ~env ~host f =
    Net.with_tcp_connect ~service:"https" ~host (Stdenv.net env)
    @@ fun conn ->
    f
    @@ Tls_eio.client_of_flow
         tls_config
         ?host:(Domain_name.of_string_exn host |> Domain_name.host |> Result.ok)
         conn
  ;; *)

  type _ response =
    | Raw : (Http.Response.t * Body.t -> 'a) -> 'a response
    | Default : string response

  (** [post res_typ ~env ~host ~headers ~path body] sends an HTTP POST request with the given parameters and returns the response. *)
  let post
    : type a.
      a response
      -> net:_ Eio.Net.t
      -> host:string
      -> headers:Http.Header.t
      -> path:string
      -> string
      -> a
    =
    fun res_typ ~net ~host ~headers ~path body ->
    Eio.Switch.run
    @@ fun sw ->
    let client = Client.make ~https:(Some (https ~authenticator:null_auth)) net in
    let res, body =
      Client.post
        ~sw
        ~body:(Body.of_string body)
        ~headers
        client
        (Uri.make ~scheme:"https" ~path ~host ())
    in
    match res_typ with
    | Default -> (Eio.Buf_read.(parse_exn take_all) body ~max_size:Int.max_value : a)
    | Raw f -> f (res, body)
  ;;

  (** [get res_typ ~env ~host ?headers path] sends an HTTP GET request with the given parameters and returns the response. *)
  let get
    : type a.
      a response
      -> net:_ Eio.Net.t
      -> host:string
      -> ?headers:Http.Header.t
      -> string
      -> a
    =
    fun res_typ ~net ~host ?(headers = empty_headers) path ->
    Eio.Switch.run
    @@ fun sw ->
    let client = Client.make ~https:(Some (https ~authenticator:null_auth)) net in
    let res, body =
      Client.get ~sw ~headers client (Uri.make ~path ~scheme:"https" ~host ())
    in
    match res_typ with
    | Default -> (Eio.Buf_read.(parse_exn take_all) body ~max_size:Int.max_value : a)
    | Raw f -> f (res, body)
  ;;

  (** [download_file env url ~dir ~filename] downloads the file from the given [url] and saves it to the specified [dir] and [filename]. *)
  let download_file net url ~dir ~filename =
    let host = get_host url in
    let path = get_path url in
    let content = get Default ~net ~host path in
    with_dir ~dir @@ fun dir -> save_doc ~dir filename content
  ;;
end

module type Task_pool_config = sig
  type input
  type output

  val dm : Domain_manager.ty Resource.t
  val stream : (input * output Promise.u) Stream.t
  val sw : Switch.t
  val handler : input -> output
end

(** [Task_pool] is a functor that creates a task pool with the given configuration. *)
module Task_pool (C : Task_pool_config) : sig
  val spawn : string -> unit
  val submit : C.input -> C.output
end = struct
  (** [run_worker id stream] runs a worker with the given [id] and processes tasks from the [stream]. *)
  let rec run_worker id stream =
    let request, reply = Stream.take stream in
    traceln "Worker %s processing request" id;
    Promise.resolve reply (C.handler request);
    run_worker id stream
  ;;

  (** [spawn name] spawns a new worker with the given [name]. *)
  let spawn name =
    Fiber.fork_daemon ~sw:C.sw (fun () ->
      Domain_manager.run C.dm (fun () ->
        traceln "Worker %s ready" name;
        run_worker name C.stream))
  ;;

  (** [submit req] submits a task with the given [req] to the task pool and returns the result. *)
  let submit req =
    let res, cb = Promise.create () in
    Stream.add C.stream (req, cb);
    Promise.await res
  ;;
end

(** [run_main f] runs the main function [f] with the Eio environment. *)
let run_main f =
  Eio_main.run
  @@ fun env ->
  (* Mirage_crypto_rng_unix. *)
  Mirage_crypto_rng_unix.use_default ();
  f env
;;

module Server = struct
  (* Prefix all trace output with "server: " *)
  let traceln fmt = traceln ("server: " ^^ fmt)

  module Read = Eio.Buf_read
  module Write = Eio.Buf_write

  (* Read one line from [client] and respond with "OK". *)
  let rec handle_client flow addr =
    traceln "Accepted connection from %a" Eio.Net.Sockaddr.pp addr;
    (* We use a buffered reader because we may need to combine multiple reads
       to get a single line (or we may get multiple lines in a single read,
       although here we only use the first one). *)
    let from_client = Read.of_flow flow ~max_size:100 in
    traceln "Received: %S" (Read.line from_client);
    Write.with_flow flow
    @@ fun to_server ->
    Write.string to_server "OK\n";
    handle_client flow addr
  ;;

  (* Accept incoming client connections on [socket].
     We can handle multiple clients at the same time.
     Never returns (but can be cancelled). *)
  let run socket =
    Eio.Net.run_server
      socket
      handle_client
      ~on_error:(traceln "Error handling connection: %a" Fmt.exn)
      ~max_connections:1000
  ;;
end

module Client = struct
  (* Prefix all trace output with "client: " *)
  let traceln fmt = traceln ("client: " ^^ fmt)

  module Read = Eio.Buf_read
  module Write = Eio.Buf_write

  (* Connect to [addr] on [net], send a message and then read the reply. *)
  let run ~net ~clock ~addr =
    traceln "Connecting to server at %a..." Eio.Net.Sockaddr.pp addr;
    Switch.run
    @@ fun sw ->
    let flow = Eio.Net.connect ~sw net addr in
    let from_client = Read.of_flow flow ~max_size:100 in
    (* let parse p =
       let open Read.Syntax in
       p <* Read.end_of_input
       in *)
    (* We use a buffered writer here so we can create the message in multiple
       steps but still send it efficiently as a single packet: *)
    Write.with_flow flow
    @@ fun to_server ->
    let rec loop ?(i = 0) () =
      if i < 3
      then (
        Write.string to_server "Hello";
        Write.char to_server ' ';
        Write.string to_server "from client\n";
        let reply = Read.line from_client in
        traceln "Got reply %S" reply;
        Eio.Time.sleep clock 1.0;
        loop ~i:(i + 1) ())
      else ()
    in
    loop ()
  ;;
end

module Run_server = struct
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

  (* Run a server and a test client, communicating using [net]. *)
  let main ~net ~clock =
    Switch.run
    @@ fun sw ->
    (* We create the listening socket first so that we can be sure it is ready
       as soon as the client wants to use it. *)
    let listening_socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
    (* Start the server running in a new fiber.
       Using [fork_daemon] here means that it will be stopped once the client is done
       (we don't wait for it to finish because it will keep accepting new connections forever). *)
    Fiber.fork_daemon ~sw (fun () -> Server.run listening_socket);
    (* Test the server: *)
    Fiber.both
      (fun () -> Client.run ~net ~clock ~addr)
      (fun () -> Client.run ~net ~clock ~addr);
    Fiber.both
      (fun () -> Client.run ~net ~clock ~addr)
      (fun () -> Client.run ~net ~clock ~addr)
  ;;

  let run () =
    Eio_main.run
    @@ fun env -> main ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
  ;;
end

module Base64 = struct
  open Core

  let mime_type_of_extension = function
    | "jpg" | "jpeg" -> "image/jpeg"
    | "png" -> "image/png"
    | "gif" -> "image/gif"
    | "bmp" -> "image/bmp"
    | "webp" -> "image/webp"
    | "svg" -> "image/svg+xml"
    | _ -> "image/jpeg" (* default fallback *)
  ;;

  let filename_extension (fname : string) : string =
    (* First, strip any directory paths; keep only the final component. *)
    let base = Filename.basename fname in
    print_endline base;
    match String.rindex base '.' with
    | None -> ""
    | Some i ->
      print_endline (string_of_int i);
      if i = 0
      then
        (* The file name starts with a dot and has nothing else
           (e.g. ".bashrc" -> ""), treat as no extension. *)
        ""
      else
        (* Substring after the last '.' *)
        String.sub base ~pos:i ~len:(String.length base - i)
  ;;

  let file_to_data_uri ~dir (filename : string) : string =
    (* Load the file into memory. Adjust load_doc as needed. *)
    let data = load_doc ~dir filename in
    (* Encode file contents in Base64. *)
    let base64_data = Base64.encode_exn data in
    (* Extract lowercase extension without the leading '.' (if any). *)
    let extension =
      match filename_extension filename with
      | "" -> "" (* No extension found *)
      | ext ->
        let without_dot = String.drop_prefix ext 1 in
        String.lowercase without_dot
    in
    print_endline extension;
    (* Determine the MIME type from the extension. *)
    let mime_type = mime_type_of_extension extension in
    print_endline mime_type;
    (* Construct the Data URI. *)
    Printf.sprintf "data:%s;base64,%s" mime_type base64_data
  ;;
end
