open Core
module Fetch = Chat_response.Fetch
module Ctx = Chat_response.Ctx

let%expect_test "Fetch.get resolves paths relative to ctx.dir first" =
  Eio_main.run
  @@ fun env ->
  (* Setup: create an in-memory directory tree.  The test environment’s CWD
     is the dune build directory; we create a [prompt_dir] sub-folder that
     mimics the situation where the prompt lives in a nested path. *)
  let cwd = Eio.Stdenv.cwd env in
  let prompt_dir = Eio.Path.(cwd / "prompt_dir") in
  Io.mkdir ~dir:cwd "prompt_dir";
  (* File only exists inside [prompt_dir]. *)
  Io.save_doc ~dir:prompt_dir "inner.txt" "INNER";
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir:prompt_dir ~cache ~tool_dir:cwd in
  let content = Fetch.get ~ctx "inner.txt" ~is_local:true in
  print_endline content;
  [%expect {| INNER |}]
;;

let%expect_test "Fetch.get falls back to the process CWD when lookup in ctx.dir fails" =
  Eio_main.run
  @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let prompt_dir = Eio.Path.(cwd / "prompt_dir2") in
  Io.mkdir ~dir:cwd "prompt_dir2";
  (* File lives in the CWD, not in [prompt_dir]. *)
  Io.save_doc ~dir:cwd "cwd_file.txt" "CWD";
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Ctx.create ~env ~dir:prompt_dir ~tool_dir:cwd ~cache in
  let content = Fetch.get ~ctx "cwd_file.txt" ~is_local:true in
  print_endline content;
  [%expect {| CWD |}]
;;
