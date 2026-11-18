open! Core
module Output = Openai.Responses.Tool_output.Output

let output_text = function
  | Output.Text text -> text
  | Output.Content _ -> failwith "expected text output"
;;

let contains text ~substring = String.is_substring text ~substring
let run tool arguments = tool.Ochat_function.run arguments |> output_text

let interrupt_read_directory
  :  'tags.
     ([> `Dir ] as 'tags) Eio.Resource.t
  -> fault:exn option ref
  -> hits:int ref
  -> 'tags Eio.Resource.t
  =
  fun (Eio.Resource.T (directory, handler)) ~fault ~hits ->
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let open_in directory ~sw name =
      match !fault with
      | None -> Original.open_in directory ~sw name
      | Some exn ->
        incr hits;
        raise exn
    ;;
  end
  in
  let bindings =
    Eio.Resource.bindings handler
    |> List.filter ~f:(function
      | H (Eio.Fs.Pi.Dir, _) -> false
      | _ -> true)
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler (H (Eio.Fs.Pi.Dir, (module Directory)) :: bindings))
;;

let temporary_directory fs =
  let rec create remaining =
    if remaining = 0 then failwith "could not create exclusive test directory";
    let path = Eio.Path.(fs / sprintf "/tmp/ochat-read-control-%08x" (Random.bits ())) in
    match Eio.Path.mkdir ~perm:0o700 path with
    | () -> path
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> create (remaining - 1)
  in
  create 16
;;

let%expect_test "read_file propagates provider control exceptions instead of text" =
  Eio_main.run (fun env ->
    let fs = Eio.Stdenv.fs env in
    let dir = temporary_directory fs in
    let file = Eio.Path.(dir / "visible.txt") in
    Eio.Path.save ~create:(`Or_truncate 0o600) file "visible";
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.unlink file;
        Eio.Path.rmdir dir)
      ~f:(fun () ->
        let directory, prefix = dir in
        let fault = ref None in
        let hits = ref 0 in
        let dir = interrupt_read_directory directory ~fault ~hits, prefix in
        let tool = Functions.get_contents ~dir in
        let propagates expected =
          fault := Some expected;
          match run tool {|{"file":"visible.txt"}|} with
          | _ -> false
          | exception actual -> phys_equal expected actual
        in
        let cancellation = propagates (Eio.Cancel.Cancelled Exit) in
        let timeout = propagates Eio.Time.Timeout in
        fault := None;
        printf
          "cancellation=%b timeout=%b provider-hits=%d normal=%b ordinary-error=%b\n"
          cancellation
          timeout
          !hits
          (run tool {|{"file":"visible.txt"}|} |> contains ~substring:"visible")
          (run tool {|{"file":"missing.txt"}|}
           |> String.is_prefix ~prefix:"error running read_file:")));
  [%expect
    {| cancellation=true timeout=true provider-hits=2 normal=true ordinary-error=true |}]
;;

let%expect_test "patch reader preserves control exceptions and ordinary missing files" =
  let patch =
    {|*** Begin Patch
*** Update File: controlled.txt
@@
-before
+after
*** End Patch|}
  in
  let writes = ref 0 in
  let removes = ref 0 in
  let run expected =
    Apply_patch.process_patch
      ~text:patch
      ~open_fn:(fun _ -> raise expected)
      ~write_fn:(fun _ _ -> incr writes)
      ~remove_fn:(fun _ -> incr removes)
  in
  let propagates expected =
    match run expected with
    | _ -> false
    | exception actual -> phys_equal expected actual
  in
  let cancellation = propagates (Eio.Cancel.Cancelled Exit) in
  let timeout = propagates Eio.Time.Timeout in
  let missing =
    match run (Failure "ordinary reader error") with
    | _ -> false
    | exception Apply_patch.Diff_error error ->
      String.equal
        (Apply_patch.error_to_string error)
        "Update file missing: controlled.txt"
  in
  printf
    "cancellation=%b timeout=%b missing=%b writes=%d removes=%d\n"
    cancellation
    timeout
    missing
    !writes
    !removes;
  [%expect {| cancellation=true timeout=true missing=true writes=0 removes=0 |}]
;;
