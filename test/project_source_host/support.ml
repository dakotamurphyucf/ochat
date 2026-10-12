open Core
module Configuration = Agent_server.Project_source_configuration
module Policy = Agent_server.Project_source_policy
module Host = Agent_server.Project_source_host

let () = Mirage_crypto_rng_unix.use_default ()

let ok = function
  | Ok value -> value
  | Error _ -> failwith "source host fixture admission failed"
;;

let with_workspace f =
  Eio_main.run (fun env ->
    let bytes = Cstruct.create 16 in
    Eio.Flow.read_exact env#secure_random bytes;
    let name =
      "och225-"
      ^ String.concat_map (Cstruct.to_string bytes) ~f:(fun c ->
        sprintf "%02x" (Char.to_int c))
    in
    let root = Eio.Path.(env#fs / "/tmp" / name) in
    Eio.Path.mkdir ~perm:0o700 root;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
      (fun () ->
         let data_root =
           Agent_store.Data_root.create
             ~env
             ~path:(Eio.Path.native_exn Eio.Path.(root / "data"))
           |> ok
         in
         f env root data_root))
;;

let open_host
      ?(configuration = Configuration.local_only)
      ?(checkpoint = ignore)
      env
      data_root
      sw
  =
  Host.open_owned
    configuration
    ~sw
    ~fs:env#fs
    ~data_root
    ~clock:(Eio.Stdenv.mono_clock env)
    ~checkpoint
    ()
  |> ok
;;

let closed = function
  | Error Host.Error.Closed -> ()
  | Error _ | Ok _ -> assert false
;;
