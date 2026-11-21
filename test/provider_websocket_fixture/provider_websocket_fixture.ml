open! Core

let accept nonce =
  Digestif.SHA1.(
    digest_string (nonce ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") |> to_raw_string)
  |> Base64.encode_exn
;;

let headers ?first_byte reader =
  let first = Option.value first_byte ~default:"" ^ Eio.Buf_read.line reader in
  let rec loop fields =
    let line = Eio.Buf_read.line reader in
    if String.is_empty line
    then first, fields
    else (
      let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
      loop ((String.lowercase key, String.strip value) :: fields))
  in
  loop []
;;

let upgrade flow fields =
  let nonce = List.Assoc.find_exn fields ~equal:String.equal "sec-websocket-key" in
  Eio.Flow.copy_string
    (sprintf
       "HTTP/1.1 101 Switching Protocols\r\n\
        Upgrade: websocket\r\n\
        Connection: Upgrade\r\n\
        Sec-WebSocket-Accept: %s\r\n\
        \r\n"
       (accept nonce))
    flow
;;

let client_message ?first_byte reader =
  let header =
    Option.value first_byte ~default:""
    ^ Eio.Buf_read.take (if Option.is_some first_byte then 1 else 2) reader
  in
  assert (Char.to_int header.[0] = 0x81);
  let short = Char.to_int header.[1] land 127 in
  assert (Char.to_int header.[1] land 128 <> 0);
  let len =
    if short < 126
    then short
    else (
      let bytes = Eio.Buf_read.take (if short = 126 then 2 else 8) reader in
      String.fold bytes ~init:0 ~f:(fun acc c -> (acc lsl 8) lor Char.to_int c))
  in
  let mask = Eio.Buf_read.take 4 reader in
  let payload = Eio.Buf_read.take len reader in
  String.mapi payload ~f:(fun i c ->
    Char.of_int_exn (Char.to_int c lxor Char.to_int mask.[i mod 4]))
  |> Jsonaf.of_string
;;

let server_frame ?(first = 0x81) text =
  let len = String.length text in
  let header =
    if len < 126
    then String.of_char_list [ Char.of_int_exn first; Char.of_int_exn len ]
    else
      String.of_char_list
        [ Char.of_int_exn first
        ; Char.of_int_exn 126
        ; Char.of_int_exn (len lsr 8)
        ; Char.of_int_exn (len land 255)
        ]
  in
  header ^ text
;;

let with_server (env : Eio_unix.Stdenv.base) handler f =
  Eio.Switch.run (fun sw ->
    let socket =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:8
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr socket with
      | `Tcp (_, port) -> port
      | _ -> assert false
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let flow, _ = Eio.Net.accept ~sw socket in
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Exn.protect
            ~finally:(fun () -> Eio.Flow.close flow)
            ~f:(fun () ->
              try handler flow with
              | End_of_file -> ());
          `Stop_daemon)
      done);
    f sw (sprintf "http://127.0.0.1:%d/v1/responses" port))
;;
