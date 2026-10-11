open! Core
module CM = Prompt.Chat_markdown
module SL = Source_loader
module Runtime = Chatml_host_runtime

let get value key = Jsonaf.member_exn key value
let string value key = get value key |> Jsonaf.string_exn
let list value key = get value key |> Jsonaf.list_exn
let require condition message = if not condition then failwith message

(* Declared edges select omission candidates; the actual production resolver
   subsequently proves their necessity. No project document is interpreted here. *)
let project_targets edges reads =
  let reached = Hash_set.of_list (module String) reads in
  let pending = Queue.of_list reads in
  let targets = Hash_set.create (module String) in
  while not (Queue.is_empty pending) do
    let owner = Queue.dequeue_exn pending in
    List.iter edges ~f:(fun edge ->
      let target = string edge "toPath" in
      if String.equal (string edge "fromPath") owner
      then (
        Hash_set.add targets target;
        if not (Hash_set.mem reached target)
        then (
          Hash_set.add reached target;
          Queue.enqueue pending target)))
  done;
  Hash_set.to_list targets |> List.sort ~compare:String.compare
;;

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
      Docs_project_bundle.stage
        directory
        ~sources:
          (List.map sources ~f:(fun (name, contents) ->
             Source.Portable_path.create name |> Or_error.ok_exn, contents));
      let entries =
        entry
        :: List.filter_map files ~f:(fun file ->
          Option.some_if
            (String.equal (string file "role") "variant")
            (string file "path"))
      in
      let parsed =
        List.map entries ~f:(fun entry -> entry, parse_bundle env scratch sources entry)
      in
      let nodes, nested, _ = List.Assoc.find_exn parsed entry ~equal:String.equal in
      let reads =
        List.concat_map parsed ~f:(fun (_, (_, _, reads)) -> reads)
        |> List.dedup_and_sort ~compare:String.compare
      in
      let project_edges, parser_edges =
        List.partition_tf (list example "edges") ~f:(fun edge ->
          String.equal (string edge "kind") "project")
      in
      let expected =
        entries @ List.map parser_edges ~f:(fun e -> string e "toPath")
        |> List.dedup_and_sort ~compare:String.compare
      in
      require
        (List.equal String.equal reads expected)
        ("bundle source closure mismatch: " ^ string example "id");
      List.iter parser_edges ~f:(fun edge ->
        let missing = string edge "toPath" in
        let reduced =
          List.filter sources ~f:(fun (name, _) -> not (String.equal name missing))
        in
        let readers =
          List.filter_map parsed ~f:(fun (entry, (_, _, reads)) ->
            Option.some_if (List.mem reads missing ~equal:String.equal) entry)
        in
        require (not (List.is_empty readers)) ("unread companion: " ^ missing);
        require
          (List.for_all readers ~f:(fun entry ->
             Result.is_error
               (Result.try_with (fun () -> parse_bundle env scratch reduced entry))))
          ("missing companion unexpectedly resolved: " ^ missing));
      let project_readers =
        List.filter_map parsed ~f:(fun (entry, (_, _, reads)) ->
          let required = project_targets project_edges reads in
          Option.some_if (not (List.is_empty required)) (entry, required))
      in
      require
        (List.equal
           String.equal
           (List.map project_edges ~f:(fun edge -> string edge "toPath")
            |> List.dedup_and_sort ~compare:String.compare)
           (List.concat_map project_readers ~f:snd
            |> List.dedup_and_sort ~compare:String.compare))
        ("unreachable project companion: " ^ string example "id");
      List.iter project_readers ~f:(fun (entry, required) ->
        Docs_project_bundle.check
          ~env
          ~home_parent:Eio.Path.(Eio.Stdenv.fs env / scratch)
          ~bundle:directory
          ~entry:(Source.Portable_path.create entry |> Or_error.ok_exn)
          ~required:
            (List.map required ~f:(fun path ->
               Source.Portable_path.create path |> Or_error.ok_exn)));
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
    "Tutorial bundles: parser/project dependency admission, missing companions, scoped \
     file reads, specialist isolation, and three-turn ChatML PASS (offline)\n"
    (Eio.Stdenv.stdout env)
;;
