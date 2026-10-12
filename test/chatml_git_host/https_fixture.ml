open Core

module Request = struct
  type t =
    { method_ : string
    ; target : string
    ; headers : (string * string) list
    ; body : string
    }

  let method_ t = t.method_
  let target t = t.target
  let header t name = List.Assoc.find t.headers name ~equal:String.Caseless.equal
  let body t = t.body

  let read flow =
    let input = Eio.Buf_read.of_flow flow ~max_size:1_048_576 in
    let method_, target =
      match String.split (Eio.Buf_read.line input) ~on:' ' with
      | [ method_; target; "HTTP/1.1" ] -> method_, target
      | _ -> failwith "invalid fixture HTTP request"
    in
    let rec headers remaining accumulated =
      if remaining = 0 then failwith "fixture header bound exceeded";
      match Eio.Buf_read.line input with
      | "" -> List.rev accumulated
      | line ->
        (match String.lsplit2 line ~on:':' with
         | None -> failwith "invalid fixture header"
         | Some (name, value) ->
           headers (remaining - 1) ((name, String.strip value) :: accumulated))
    in
    let headers = headers 64 [] in
    let length =
      List.Assoc.find headers "content-length" ~equal:String.Caseless.equal
      |> Option.value_map ~default:0 ~f:Int.of_string
    in
    if length < 0 || length > 1_048_576 then failwith "fixture body bound exceeded";
    { method_; target; headers; body = Eio.Buf_read.take length input }
  ;;
end

module Response = struct
  type t =
    { status : string
    ; headers : (string * string) list
    ; body : string
    }

  let write { status; headers; body } flow =
    let fields =
      List.map
        (("Connection", "close")
         :: ("Content-Length", Int.to_string (String.length body))
         :: headers)
        ~f:(fun (name, value) -> name ^ ": " ^ value ^ "\r\n")
      |> String.concat
    in
    Eio.Flow.copy_string ("HTTP/1.1 " ^ status ^ "\r\n" ^ fields ^ "\r\n" ^ body) flow
  ;;
end

let unwrap = function
  | Ok value -> value
  | Error (`Msg message) -> failwith message
;;

let start net ~sw ~handle =
  Mirage_crypto_rng_unix.use_default ();
  let cert = X509.Certificate.decode_pem Tls_fixture.certificate |> unwrap in
  let key = X509.Private_key.decode_pem Tls_fixture.private_key |> unwrap in
  let config = Tls.Config.server ~certificates:(`Single ([ cert ], key)) () |> unwrap in
  let address = List.hd_exn (Eio.Net.getaddrinfo_stream ~service:"0" net "localhost") in
  let listener = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 net address in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    while true do
      let flow, _ = Eio.Net.accept ~sw listener in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        Fun.protect
          ~finally:(fun () -> Eio.Flow.close flow)
          (fun () ->
             try
               let tls = Tls_eio.server_of_flow config flow in
               Response.write (handle (Request.read tls)) tls
             with
             | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ | End_of_file
             | Eio.Io (Eio.Net.E (Connection_reset _), _) -> ());
        `Stop_daemon)
    done;
    `Stop_daemon);
  sprintf "https://localhost:%d" port
;;
