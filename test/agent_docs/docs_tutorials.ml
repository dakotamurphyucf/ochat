open! Core
module CM = Prompt.Chat_markdown
module SL = Source_loader
module Runtime = Chatml_host_runtime

let get value key = Jsonaf.member_exn key value
let string value key = get value key |> Jsonaf.string_exn
let list value key = get value key |> Jsonaf.list_exn
let require condition message = if not condition then failwith message

(* Replay only the bytes declared by the downloadable bundle, using the same
   source resolver and nested-agent traversal as prompt artifact capture. *)
let parse_bundle env scratch sources entry =
  let dir = Eio.Path.(Eio.Stdenv.fs env / scratch) in
  let read = Hash_set.create (module String) in
  let agents = Queue.create () in
  let loader =
    SL.captured_filesystem ~root:dir ~sources
    |> SL.with_observer ~f:(fun source _ -> Hash_set.add read (SL.relative_path source))
    |> SL.with_agent_observer ~f:(fun source -> Queue.enqueue agents source)
  in
  let parse source =
    let raw = SL.read loader source |> Result.ok_or_failwith in
    CM.parse_chat_inputs ~source:(SL.relative_path source) ~source_loader:loader ~dir raw
  in
  let root = SL.root loader ~file:entry |> Result.ok_or_failwith in
  let root_nodes = parse root in
  let visited = Hash_set.of_list (module String) [ entry ] in
  let nested = ref [] in
  while not (Queue.is_empty agents) do
    let source = Queue.dequeue_exn agents in
    let name = SL.relative_path source in
    if not (Hash_set.mem visited name)
    then (
      Hash_set.add visited name;
      nested := (name, parse source) :: !nested)
  done;
  root_nodes, !nested, Hash_set.to_list read |> List.sort ~compare:String.compare
;;

let file_read env scratch example nodes =
  let tools =
    List.filter_map nodes ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
  in
  let spec =
    match
      List.filter_map tools ~f:(function
        | CM.Read_file s -> Some s
        | _ -> None)
    with
    | [ spec ] -> spec
    | _ -> failwith "tutorial needs exactly one read_file declaration"
  in
  let relative =
    match spec.roots with
    | [ { id = "reference"; path = Relative { base = Workspace; path }; _ } ] -> path
    | _ -> failwith "tutorial read root must be the workspace reference directory"
  in
  require (String.equal relative "reference") "unexpected tutorial read root";
  let dir = Eio.Path.(Eio.Stdenv.fs env / scratch / string example "id") in
  let tool =
    Functions.get_contents_scoped
      ~fs:(Eio.Stdenv.fs env)
      ~dir
      ~roots:
        [ Functions.read_file_root ~id:"reference" ~path:Eio.Path.(dir / relative) () ]
      ()
  in
  let call args =
    match tool.Ochat_function.run args with
    | Openai.Responses.Tool_output.Output.Text text -> text
    | _ -> failwith "expected file text"
  in
  require
    (String.is_substring
       (call {|{"root":"reference","file":"project.txt"}|})
       ~substring:"Lantern")
    "tutorial sample read failed";
  require
    (String.is_substring
       (call
          (Jsonaf.to_string
             (`Object
                 [ "root", `String "reference"
                 ; "file", `String ("../" ^ string example "entry")
                 ])))
       ~substring:"outside the configured read roots")
    "tutorial root escape was not rejected"
;;

let workflow source =
  let session = Docs_chatml.session_exn source in
  Docs_chatml.handle_exn session ~phase:"session_start" "Session_start" [];
  for count = 1 to 3 do
    Docs_chatml.handle_exn session ~phase:"turn_end" "Turn_end" [];
    require
      (match Runtime.current_state session with
       | Chatml.Chatml_lang.VInt n -> n = count
       | _ -> false)
      "workflow did not count exactly one completed turn";
    let effects =
      Runtime.committed_local_effects session
      |> Runtime.decode_local_effects
      |> Result.ok_or_failwith
    in
    require
      (match count, effects with
       | (1 | 2), [] -> true
       | 3, [ Runtime.End_session "Three-turn session finished" ] -> true
       | _ -> false)
      "workflow stopped at the wrong turn"
  done
;;

let run env root scratch =
  let load source = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / source) in
  let examples =
    load "docs-src/examples/catalog.json" |> Jsonaf.of_string |> Jsonaf.list_exn
  in
  List.iter examples ~f:(fun example ->
    let files = list example "files" in
    match get example "entry" with
    | `String entry when String.is_suffix entry ~suffix:".chatmd" ->
      let sources =
        List.map files ~f:(fun f -> string f "path", load (string f "source"))
      in
      let directory = Eio.Path.(Eio.Stdenv.fs env / scratch / string example "id") in
      Eio.Path.mkdir ~perm:0o700 directory;
      List.iter sources ~f:(fun (name, contents) ->
        require
          ((not (Filename.is_absolute name))
           && not (List.mem (String.split name ~on:'/') ".." ~equal:String.equal))
          "unsafe example path";
        let parents = Filename.dirname name |> String.split ~on:'/' in
        let _parent =
          List.fold parents ~init:directory ~f:(fun parent part ->
            let child = Eio.Path.(parent / part) in
            if Poly.equal (Eio.Path.kind ~follow:false child) `Not_found
            then Eio.Path.mkdir ~perm:0o700 child;
            child)
        in
        Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(directory / name) contents);
      let nodes, nested, reads = parse_bundle env scratch sources entry in
      let expected =
        entry :: List.map (list example "edges") ~f:(fun e -> string e "toPath")
        |> List.dedup_and_sort ~compare:String.compare
      in
      require
        (List.equal String.equal reads expected)
        ("bundle source closure mismatch: " ^ string example "id");
      List.iter (list example "edges") ~f:(fun edge ->
        let missing = string edge "toPath" in
        let reduced =
          List.filter sources ~f:(fun (name, _) -> not (String.equal name missing))
        in
        require
          (Result.is_error
             (Result.try_with (fun () -> parse_bundle env scratch reduced entry)))
          ("missing companion unexpectedly resolved: " ^ missing));
      (match string example "id" with
       | "file-reader" -> file_read env scratch example nodes
       | "specialist" ->
         file_read env scratch example nodes;
         require
           (match nested with
            | [ ("docs-reviewer.chatmd", child) ] ->
              not
                (List.exists child ~f:(function
                   | CM.Tool _ -> true
                   | _ -> false))
            | _ -> false)
           "specialist closure or tool isolation changed"
       | "three-turns" ->
         workflow (List.Assoc.find_exn sources ~equal:String.equal "three-turns.chatml")
       | _ -> ())
    | _ -> ());
  Eio.Flow.copy_string
    "Tutorial bundles: captured dependency closures, missing companions, scoped file \
     reads, specialist isolation, and three-turn ChatML PASS (offline)\n"
    (Eio.Stdenv.stdout env)
;;
