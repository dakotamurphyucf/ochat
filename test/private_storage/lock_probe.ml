open! Core

let ok = function
  | Ok value -> value
  | Error _ -> failwith "synthetic lock probe failed"
;;

let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let arguments = Sys.get_argv () in
      let anchor = Eio.Path.(Eio.Stdenv.fs env / arguments.(1)) in
      let directory =
        Private_storage.Directory.open_or_create
          ~sw
          ~anchor
          ~components:[ ok (Private_storage.Name.create "private") ]
        |> ok
      in
      let lease =
        Private_storage.Lock.acquire
          directory
          (ok (Private_storage.Name.create "coordination"))
          ~sw
          ~mode:
            (if Array.length arguments > 2 && String.equal arguments.(2) "shared"
             then Shared
             else Exclusive)
        |> ok
      in
      Eio.Flow.copy_string "ready\n" (Eio.Stdenv.stdout env);
      ignore
        (Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:32 (Eio.Stdenv.stdin env))
         : string);
      Private_storage.Lock.release lease))
;;
