open Core

type request =
  { target : string
  ; headers : (string * string) list
  ; body : string
  }

type response =
  { status : int
  ; body : string
  }

let json body = { status = 200; body }

let rec headers reader acc =
  let line = Eio.Buf_read.line reader |> String.strip in
  if String.is_empty line
  then List.rev acc
  else (
    let key, value = String.lsplit2 line ~on:':' |> Option.value_exn in
    headers reader ((String.lowercase key, String.strip value) :: acc))
;;

let read_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
  let line = Eio.Buf_read.line reader in
  let target = List.nth_exn (String.split line ~on:' ') 1 in
  let headers = headers reader [] in
  let length =
    List.Assoc.find headers ~equal:String.equal "content-length"
    |> Option.value_map ~default:0 ~f:Int.of_string
  in
  let body = Eio.Buf_read.take length reader in
  { target; headers; body }
;;

let write_response flow response =
  let header =
    sprintf
      "HTTP/1.1 %d Test\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n"
      response.status
      (String.length response.body)
  in
  Eio.Flow.copy_string (header ^ response.body) flow
;;

let serve ~sw listener handler =
  let rec loop () =
    let flow, _ = Eio.Net.accept ~sw listener in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Fun.protect
        (fun () ->
           match read_request flow with
           | request -> handler flow request
           | exception End_of_file -> ())
        ~finally:(fun () -> Eio.Flow.close flow);
      `Stop_daemon);
    loop ()
  in
  Eio.Fiber.fork_daemon ~sw (fun () -> loop ())
;;

let with_raw_server env handler f =
  Eio.Switch.run (fun sw ->
    let listener =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~backlog:16
        (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr listener with
      | `Tcp (_, port) -> port
      | _ -> failwith "expected TCP listener"
    in
    serve ~sw listener handler;
    f sw (sprintf "http://127.0.0.1:%d" port))
;;

let with_server env handler f =
  with_raw_server env (fun flow request -> write_response flow (handler request)) f
;;
