open Core

let attempt f =
  try
    f ();
    true
  with
  | _ -> false
;;

let bool value = if value then `True else `False

let () =
  let args = Sys.get_argv () in
  let file_access = attempt (fun () -> In_channel.read_all args.(1) |> ignore) in
  let socket_access =
    attempt (fun () ->
      let socket = Caml_unix.socket PF_UNIX SOCK_STREAM 0 in
      Exn.protect
        ~finally:(fun () -> Caml_unix.close socket)
        ~f:(fun () -> Caml_unix.connect socket (ADDR_UNIX args.(2))))
  in
  let descriptor_access =
    attempt (fun () ->
      let fd = Core_unix.File_descr.of_int (Int.of_string args.(3)) in
      let info = Core_unix.fstat fd in
      match info.st_kind with
      | S_REG when Int.equal info.st_ino (Int.of_string args.(4)) -> ()
      | _ -> failwith "not the parent's file descriptor")
  in
  let request =
    Jsonaf.to_string
      (`Object
          [ "file_access", bool file_access
          ; "socket_access", bool socket_access
          ; "descriptor_access", bool descriptor_access
          ; "parent_environment", bool (Option.is_some (Sys.getenv "CHANNEL_PARENT_ONLY"))
          ])
  in
  match
    Shell_access.Request_channel.Client.exchange
      ~limits:Shell_access.Request_channel.default_limits
      request
  with
  | Ok response ->
    Out_channel.output_string stdout (response ^ "\n");
    Out_channel.flush stdout
  | Error error -> failwith error
;;
