open! Core

let () =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let executable = (Sys.get_argv ()).(1) in
    let executable =
      if Filename.is_relative executable
      then
        Eio_posix.Low_level.realpath
          (Eio.Path.native_exn Eio.Path.(Eio.Stdenv.cwd env / executable))
      else executable
    in
    let root =
      Eio.Path.(Eio.Stdenv.fs env / Agent_server_test_support.temporary_root env)
    in
    Exn.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Path.rmtree root))
      ~f:(fun () ->
        let run ?(is_success = Int.equal 0) arguments =
          let stderr = Buffer.create 128 in
          let output =
            Eio.Process.parse_out
              (Eio.Stdenv.process_mgr env)
              Eio.Buf_read.take_all
              ~cwd:root
              ~env:[| "PATH=/usr/bin:/bin" |]
              ~stderr:(Eio.Flow.buffer_sink stderr)
              ~is_success
              (executable :: "libraries" :: arguments)
          in
          output, Buffer.contents stderr
        in
        let listed, _ = run [ "list" ] in
        assert (String.is_prefix listed ~prefix:"lifecycle\t");
        let manifest, _ = run [ "show"; "lifecycle" ] in
        let inventory =
          Capability_sources.installed
            ~limits:Capability_sources.Limits.default
            ~checkpoint:Eio.Fiber.check
          |> Result.map_error ~f:Capability_sources.Error.message
          |> Result.ok_or_failwith
        in
        let resource =
          Capability_sources.find
            inventory
            (Capability_sources.Id.of_string "lifecycle"
             |> Result.map_error ~f:Capability_sources.Error.message
             |> Result.ok_or_failwith)
          |> Result.map_error ~f:Capability_sources.Error.message
          |> Result.ok_or_failwith
        in
        [%test_eq: string]
          (String.strip manifest)
          (Jsonaf.to_string (Capability_sources.Resource.manifest resource));
        let member = "examples/libraries/lifecycle/src/lifecycle.chatmli" in
        let exact, _ = run [ "show"; "lifecycle"; "-member"; member ] in
        let selected =
          Capability_sources.Resource.member
            resource
            (Source.Portable_path.create member |> Or_error.ok_exn)
          |> Result.map_error ~f:Capability_sources.Error.message
          |> Result.ok_or_failwith
        in
        [%test_eq: string] exact (Capability_sources.Member.contents selected);
        let exported, _ =
          run
            [ "export"
            ; "lifecycle"
            ; "-parent"
            ; Eio.Path.native_exn root
            ; "-directory"
            ; "copied"
            ]
        in
        assert (String.is_prefix exported ~prefix:"Exported lifecycle (");
        List.iter (Capability_sources.Resource.members resource) ~f:(fun member ->
          [%test_eq: string]
            (Eio.Path.load
               Eio.Path.(
                 root
                 / "copied"
                 / Source.Portable_path.to_string (Capability_sources.Member.path member)))
            (Capability_sources.Member.contents member));
        let _, rejected =
          run
            ~is_success:(fun code -> not (Int.equal code 0))
            [ "export"
            ; "lifecycle"
            ; "-parent"
            ; Eio.Path.native_exn root
            ; "-directory"
            ; "copied"
            ]
        in
        assert (String.is_substring rejected ~substring:"destination was not created");
        print_endline
          "installed CLI: list/show/exact-read/create-only export from an unrelated \
           empty cwd"))
;;
