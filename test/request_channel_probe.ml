open Core

let attempt f =
  try
    f ();
    true
  with
  | _ -> false
;;

let bool value = if value then `True else `False

let isolation args =
  let file_access = attempt (fun () -> In_channel.read_all args.(1) |> ignore) in
  let socket_access =
    attempt (fun () ->
      let socket = Caml_unix.socket PF_UNIX SOCK_STREAM 0 in
      Exn.protect
        ~finally:(fun () -> Caml_unix.close socket)
        ~f:(fun () -> Caml_unix.connect socket (ADDR_UNIX args.(2))))
  in
  let descriptor_access =
    String.split args.(3) ~on:','
    |> List.exists ~f:(fun descriptor ->
      attempt (fun () ->
        let fd = Core_unix.File_descr.of_int (Int.of_string descriptor) in
        let info = Core_unix.fstat fd in
        match info.st_kind with
        | S_REG when Int.equal info.st_ino (Int.of_string args.(4)) -> ()
        | _ -> failwith "not the parent's file descriptor"))
  in
  Jsonaf.to_string
    (`Object
        [ "file_access", bool file_access
        ; "socket_access", bool socket_access
        ; "descriptor_access", bool descriptor_access
        ; "parent_environment", bool (Option.is_some (Sys.getenv "CHANNEL_PARENT_ONLY"))
        ])
;;

let limits () =
  let module R = Core_unix.RLimit in
  let resources =
    [ "cpu", R.cpu_seconds
    ; "file_size", R.file_size
    ; "open_files", R.num_file_descriptors
    ]
    @
    match R.virtual_memory with
    | Ok resource -> [ "memory", resource ]
    | Error _ -> []
  in
  let value = function
    | R.Limit.Infinity -> `String "unlimited"
    | Limit value -> `Number (Int64.to_string value)
  in
  `Object
    (List.map resources ~f:(fun (name, resource) ->
       let limits = R.get resource in
       name, `Object [ "soft", value limits.cur; "hard", value limits.max ]))
  |> Jsonaf.to_string
;;

let () =
  let args = Sys.get_argv () in
  let request =
    match Array.to_list args with
    | [ _; "--limits" ] -> limits ()
    | _ -> isolation args
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
