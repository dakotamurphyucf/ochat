open! Core
module Sources = Capability_sources

let source_ok result =
  Result.map_error result ~f:Sources.Error.message |> Result.ok_or_failwith
;;

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
        let listed_ids =
          String.split_lines listed
          |> List.map ~f:(fun line -> String.lsplit2_exn line ~on:'\t' |> fst)
          |> List.sort ~compare:String.compare
        in
        [%test_eq: string list]
          listed_ids
          [ "goal-requests"; "instruction-selection"; "lifecycle"; "recurrence-requests" ];
        let inventory =
          Sources.installed ~limits:Sources.Limits.default ~checkpoint:Eio.Fiber.check
          |> source_ok
        in
        let member_path member =
          Source.Portable_path.to_string (Sources.Member.path member)
        in
        List.iter listed_ids ~f:(fun id ->
          let resource =
            Sources.find inventory (Sources.Id.of_string id |> source_ok) |> source_ok
          in
          let manifest, _ = run [ "show"; id ] in
          [%test_eq: string]
            (String.strip manifest)
            (Jsonaf.to_string (Sources.Resource.manifest resource));
          let members = Sources.Resource.members resource in
          let selected =
            List.find_exn members ~f:(fun member ->
              String.is_suffix (member_path member) ~suffix:".chatmli")
          in
          let exact, _ = run [ "show"; id; "-member"; member_path selected ] in
          [%test_eq: string] exact (Sources.Member.contents selected);
          let export =
            [ "export"; id; "-parent"; Eio.Path.native_exn root; "-directory"; id ]
          in
          let exported, _ = run export in
          assert (String.is_prefix exported ~prefix:("Exported " ^ id ^ " ("));
          List.iter members ~f:(fun member ->
            [%test_eq: string]
              (Eio.Path.load Eio.Path.(root / id / member_path member))
              (Sources.Member.contents member));
          let _, rejected = run ~is_success:(fun code -> not (Int.equal code 0)) export in
          assert (String.is_substring rejected ~substring:"destination was not created"));
        print_endline
          "installed CLI: list/show/exact-read/create-only export of all four resources \
           from an unrelated cwd"))
;;
