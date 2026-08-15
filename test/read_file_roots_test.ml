open! Core
module Output = Openai.Responses.Tool_output.Output

let output_text = function
  | Output.Text text -> text
  | Output.Content _ -> failwith "expected text output"
;;

let contains text ~substring = String.is_substring text ~substring
let run tool arguments = tool.Ochat_function.run arguments |> output_text

let%expect_test "cwd capabilities remain anchored to the process working directory" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  let filename = sprintf "ochat-read-file-cwd-%08x.txt" (Random.bits ()) in
  let path = Eio.Path.(cwd / filename) in
  Eio.Path.save ~create:(`Or_truncate 0o600) path "cwd-visible";
  Exn.protect
    ~f:(fun () ->
      let roots = [ Functions.read_file_root ~id:"cwd" ~path:cwd () ] in
      let tool = Functions.get_contents_scoped ~fs ~dir:cwd ~roots () in
      let description = Option.value_exn tool.info.function_.description in
      printf
        "read=%b absolute-description=%b\n"
        (run
           tool
           (Jsonaf.to_string
              (`Object [ "root", `String "cwd"; "file", `String filename ]))
         |> contains ~substring:"cwd-visible")
        (contains description ~substring:(Stdlib.Sys.getcwd ())))
    ~finally:(fun () -> Eio.Path.unlink path);
  [%expect {| read=true absolute-description=true |}]
;;

let%expect_test "scoped read_file advertises and enforces configured roots" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let temporary = Core_unix.mkdtemp "/tmp/ochat-read-file.XXXXXX" in
  let launch_path = Filename.concat temporary "launch" in
  let docs_path = Filename.concat temporary "docs" in
  let outside_path = Filename.concat temporary "outside" in
  let launch = Eio.Path.(fs / launch_path) in
  let docs = Eio.Path.(fs / docs_path) in
  let outside = Eio.Path.(fs / outside_path) in
  List.iter [ launch; docs; outside ] ~f:(Eio.Path.mkdir ~perm:0o700);
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(launch / "local.txt") "local";
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(docs / "guide.md") "guide";
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(outside / "secret.txt") "secret";
  Eio.Path.symlink ~link_to:outside_path Eio.Path.(launch / "escape");
  let roots =
    [ Functions.read_file_root ~id:"project" ~path:launch ~description:"Project files" ()
    ; Functions.read_file_root ~id:"docs" ~path:docs ~description:"Documentation" ()
    ]
  in
  let tool =
    Functions.get_contents_scoped
      ~fs
      ~dir:launch
      ~roots
      ~description:"Prefer the docs root for manuals."
      ()
  in
  let metadata = tool.info.function_ in
  let description = Option.value_exn metadata.description in
  let root_enum =
    Jsonaf.member_exn "properties" metadata.parameters
    |> Jsonaf.member_exn "root"
    |> Jsonaf.member_exn "enum"
    |> Jsonaf.list_exn
    |> List.map ~f:Jsonaf.string_exn
  in
  let properties = Jsonaf.member_exn "properties" metadata.parameters in
  let property_description name =
    Jsonaf.member_exn name properties
    |> Jsonaf.member_exn "description"
    |> Jsonaf.string_exn
  in
  printf
    "description=%b %b %b %b\n"
    (contains description ~substring:"project:")
    (contains description ~substring:launch_path)
    (contains description ~substring:"docs:")
    (contains description ~substring:"Prefer the docs root for manuals.");
  printf
    "arguments=%b %b %b\n"
    (property_description "file" |> contains ~substring:"existing regular UTF-8")
    (property_description "offset" |> contains ~substring:"0-based line offset")
    (property_description "line_count" |> contains ~substring:"rest of the file");
  printf "root-enum=%s\n" (String.concat ~sep:"," root_enum);
  printf
    "launch-relative=%b\n"
    (run tool {|{"file":"local.txt"}|} |> contains ~substring:"local");
  printf
    "named-root=%b\n"
    (run tool {|{"root":"docs","file":"guide.md"}|} |> contains ~substring:"guide");
  printf
    "parent-escape=%b\n"
    (run tool {|{"file":"../outside/secret.txt"}|}
     |> contains ~substring:"outside the configured read roots");
  printf
    "symlink-escape=%b\n"
    (run tool {|{"root":"project","file":"escape/secret.txt"}|}
     |> contains ~substring:"outside the configured read roots");
  printf
    "absolute-outside=%b\n"
    (run
       tool
       (Jsonaf.to_string (`Object [ "file", `String (outside_path ^ "/secret.txt") ]))
     |> contains ~substring:"outside the configured read roots");
  printf
    "directory=%b\n"
    (run tool {|{"root":"project","file":"."}|}
     |> contains ~substring:"refusing non-regular file");
  printf
    "negative-offset=%b negative-line-count=%b\n"
    (run tool {|{"file":"local.txt","offset":-1}|}
     |> contains ~substring:"offset must be non-negative")
    (run tool {|{"file":"local.txt","line_count":-1}|}
     |> contains ~substring:"line_count must be non-negative");
  [%expect
    {|
    description=true true true true
    arguments=true true true
    root-enum=project,docs
    launch-relative=true
    named-root=true
    parent-escape=true
    symlink-escape=true
    absolute-outside=true
    directory=true
    negative-offset=true negative-line-count=true
    |}]
;;

let%expect_test "filesystem root explicitly grants absolute and named reads" =
  Eio_main.run
  @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let temporary = Core_unix.mkdtemp "/tmp/ochat-read-file-root.XXXXXX" in
  let file_path = Filename.concat temporary "visible.txt" in
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(fs / file_path) "visible";
  let roots = [ Functions.read_file_root ~id:"computer" ~path:Eio.Path.(fs / "/") () ] in
  let tool = Functions.get_contents_scoped ~fs ~dir:(Eio.Stdenv.cwd env) ~roots () in
  let arguments = Jsonaf.to_string (`Object [ "file", `String file_path ]) in
  let relative_to_root = String.chop_prefix_exn file_path ~prefix:"/" in
  let named_arguments =
    Jsonaf.to_string
      (`Object [ "root", `String "computer"; "file", `String relative_to_root ])
  in
  printf
    "absolute=%b named=%b\n"
    (run tool arguments |> contains ~substring:"visible")
    (run tool named_arguments |> contains ~substring:"visible");
  [%expect {| absolute=true named=true |}]
;;
