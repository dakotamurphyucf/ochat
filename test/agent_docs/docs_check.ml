open! Core

let load env root file = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file)
let report env message = Eio.Flow.copy_string (message ^ "\n") (Eio.Stdenv.stdout env)
let require predicate message = if not predicate then failwith message

let rec files env root relative =
  let path = Eio.Path.(Eio.Stdenv.fs env / root / relative) in
  Eio.Path.read_dir path
  |> List.concat_map ~f:(fun name ->
    let file = Filename.concat relative name in
    match Eio.Path.kind ~follow:false Eio.Path.(path / name) with
    | `Directory
      when (not (String.is_prefix name ~prefix:".")) && not (String.equal name "_build")
      -> files env root file
    | `Regular_file -> [ file ]
    | _ -> [])
;;

let normalized path =
  String.split path ~on:'/'
  |> List.fold ~init:[] ~f:(fun acc component ->
    match component, acc with
    | "", _ | ".", _ -> acc
    | "..", _ :: rest -> rest
    | "..", [] -> [ ".." ]
    | value, _ -> value :: acc)
  |> List.rev
  |> String.concat ~sep:"/"
;;

let link_targets text =
  let pattern = Re.Perl.compile_pat "\\]\\(([^)\\n]+)\\)" in
  Re.all pattern text
  |> List.map ~f:(fun group ->
    Re.Group.get group 1
    |> String.strip
    |> String.strip ~drop:(fun c -> Char.equal c '<' || Char.equal c '>'))
;;

let external_target value =
  List.exists [ "http:"; "https:"; "mailto:"; "file:" ] ~f:(fun prefix ->
    String.is_prefix value ~prefix)
;;

let check_links env root documents =
  let missing =
    List.concat_map documents ~f:(fun file ->
      let text = load env root file in
      link_targets text @ Docs_links.html_images text
      |> List.filter_map ~f:(fun target ->
        let path =
          String.lsplit2 target ~on:'#' |> Option.value_map ~default:target ~f:fst
        in
        if String.is_empty path || external_target target
        then None
        else (
          let resolved = normalized (Filename.concat (Filename.dirname file) path) in
          if
            String.is_substring resolved ~substring:"_build/"
            || String.is_prefix path ~prefix:"/tmp/"
          then None
          else if
            not
              (Poly.equal
                 (Eio.Path.kind
                    ~follow:true
                    Eio.Path.(Eio.Stdenv.fs env / root / resolved))
                 `Not_found)
          then None
          else Some (file ^ " -> " ^ target))))
  in
  List.iter missing ~f:(report env);
  require (List.is_empty missing) "broken documentation links"
;;

let check_anchors env root documents =
  List.iter documents ~f:(fun file ->
    link_targets (load env root file)
    |> List.iter ~f:(fun target ->
      if not (external_target target)
      then (
        match String.lsplit2 target ~on:'#' with
        | Some (path, anchor)
          when String.is_empty path || String.is_suffix path ~suffix:".md" ->
          let destination =
            if String.is_empty path
            then file
            else normalized (Filename.concat (Filename.dirname file) path)
          in
          require
            (Docs_links.is_valid_anchor (load env root destination) anchor)
            (file ^ " -> invalid anchor " ^ target)
        | _ -> ())))
;;

let check_protocol env root =
  let guide = load env root "docs-src/agent-server/protocol.md" in
  List.iter Agent_protocol.Command.supported_methods ~f:(fun method_ ->
    require
      (String.is_substring guide ~substring:("`" ^ method_ ^ "`"))
      ("undocumented method: " ^ method_));
  let generated = load env root "docs-src/agent-server/protocol-types.md" in
  files env root "lib/agent_protocol"
  |> List.filter ~f:(fun file ->
    String.is_suffix file ~suffix:".mli" && not (String.is_suffix file ~suffix:".pp.mli"))
  |> List.iter ~f:(fun file ->
    require
      (String.is_substring generated ~substring:(String.rstrip (load env root file)))
      ("stale protocol type excerpt: " ^ file))
;;

let declares_callable_contract body =
  String.split_lines body
  |> List.exists
       ~f:
         (String.is_prefix
            ~prefix:"The following excerpt is the current callable contract.")
;;

let check_callable_excerpts env root documents =
  require
    (declares_callable_contract
       "The following excerpt is the current callable contract. Eio owns resources.")
    "callable contract declaration not recognized";
  require
    (not
       (declares_callable_contract "Checks excerpts labeled current callable contract."))
    "incidental callable contract mention treated as a declaration";
  List.iter documents ~f:(fun file ->
    let body = load env root file in
    if declares_callable_contract body
    then (
      let interfaces =
        link_targets body |> List.filter ~f:(String.is_suffix ~suffix:".mli")
      in
      require (List.length interfaces = 1) (file ^ ": ambiguous callable contract");
      let path =
        normalized (Filename.concat (Filename.dirname file) (List.hd_exn interfaces))
      in
      let expected = "```ocaml\n" ^ String.rstrip (load env root path) ^ "\n```" in
      require
        (String.is_substring body ~substring:expected)
        (file ^ ": stale callable contract excerpt")))
;;

let check_json env root documents =
  let pattern =
    Re.(compile (seq [ str "```json\n"; group (non_greedy (rep any)); str "\n```" ]))
  in
  List.iter documents ~f:(fun file ->
    Re.all pattern (load env root file)
    |> List.iter ~f:(fun block ->
      let data =
        Re.Group.get block 1
        |> String.substr_replace_all ~pattern:"SESSION_ID" ~with_:"ses_tutorial"
        |> String.substr_replace_all ~pattern:"ATTACHMENT_ID" ~with_:"att_tutorial"
      in
      let json = Jsonaf.of_string data in
      match Agent_protocol.Envelope.of_json json with
      | Ok (Request request) ->
        (match
           Agent_protocol.Command.of_method_and_params
             ~method_:request.method_
             ~params:request.params
         with
         | Ok _ -> ()
         | Error error ->
           failwith
             (file ^ ": " ^ Sexp.to_string_hum ([%sexp_of: Agent_protocol.Error.t] error)))
      | Ok _ -> ()
      | Error error -> failwith (file ^ ": " ^ error.message)));
  let management_pattern =
    Re.(
      compile
        (seq
           [ str "```json session-management-envelope\n"
           ; group (non_greedy (rep any))
           ; str "\n```"
           ]))
  in
  List.iter documents ~f:(fun file ->
    Re.all management_pattern (load env root file)
    |> List.iter ~f:(fun block ->
      match
        Agent_session.Session_management.decode_request
          (Re.Group.get block 1 |> Jsonaf.of_string)
      with
      | Ok _ -> ()
      | Error error -> failwith (file ^ ": " ^ error.Agent_protocol.Invocation.message)));
  let validation_pattern =
    Re.(
      compile
        (seq
           [ str "```json tool=ochat_validate\n"
           ; group (non_greedy (rep any))
           ; str "\n```"
           ]))
  in
  let module V = Chat_response.Authoring_validation in
  let host =
    V.create_host
      ~runtime_identity:"documentation-validation-target"
      ~targets:[ One_off_script; Standalone_tool; Moderator; Generated_chatmd ]
      ~moderator_surface:Ordinary
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let capabilities =
    Chat_response.Tool_capability.create
      ~owner:"documentation-validation"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "documentation-no-tools")
      []
    |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
    |> Result.ok_or_failwith
  in
  List.iter documents ~f:(fun file ->
    Re.all validation_pattern (load env root file)
    |> List.iter ~f:(fun block ->
      let report =
        V.validate ~env ~host ~capabilities (Re.Group.get block 1 |> Jsonaf.of_string)
      in
      require
        (V.valid report)
        (file
         ^ ": invalid readonly validation example: "
         ^ Jsonaf.to_string (V.to_json report))))
;;

let temporary_root env =
  let suffix =
    Agent_protocol.Id.Transaction.create () |> Agent_protocol.Id.Transaction.to_string
  in
  let root = Filename.concat "/tmp" ("ochat-doc-check-" ^ suffix) in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / root);
  root
;;

let run_example env executable arguments =
  Eio.Process.run (Eio.Stdenv.process_mgr env) (executable :: arguments)
;;

let check_examples env root executable =
  let scratch = temporary_root env in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / scratch))
    (fun () ->
       run_example env executable [ "setup"; scratch; "gpt-5.6-sol" ];
       Docs_observer.run env scratch;
       Docs_library.run env scratch;
       Docs_smoke.batch env root scratch;
       Docs_smoke.shell_examples env root;
       Docs_tutorials.run env root scratch;
       run_example
         env
         executable
         [ "check-requests"
         ; Filename.concat root "docs-src/examples/agent-server/clients/discover.ndjson"
         ];
       List.iter [ "hello.chatmd"; "timer.chatmd" ] ~f:(fun name ->
         let source = load env root ("docs-src/examples/agent-server/prompts/" ^ name) in
         Eio.Path.save
           ~create:(`Or_truncate 0o600)
           Eio.Path.(Eio.Stdenv.fs env / scratch / "hello.chatmd")
           source;
         run_example
           env
           executable
           [ "validate-config"; Filename.concat scratch "unix.sexp" ]);
       Docs_smoke.shell env root;
       Docs_smoke.shell_syntax env root;
       Docs_smoke.timer env root (Filename.concat scratch "workspace"))
;;

let check_cli_flag_inventory () =
  let source =
    {|flag "config" (optional string)
      flag
        "validate-only" no_arg
      flag "-config" (optional string)
      "--connect"; "-help"; "unrelated-value"|}
  in
  require
    (List.equal
       String.equal
       (Docs_inventory.cli_flags source)
       [ "--connect"; "-config"; "-help"; "-validate-only" ])
    "CLI inventory must include Core Command names and literal options without duplicates"
;;

let () =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    match Array.to_list (Sys.get_argv ()) with
    | [ _; "--refresh"; root ] -> Docs_inventory.refresh env root
    | [ _; root; executable; tools_example ] ->
      let root = Eio_posix.Low_level.realpath root in
      let documents =
        files env root "docs-src"
        |> List.filter ~f:(fun file -> String.is_suffix file ~suffix:".md")
      in
      let authored =
        List.filter documents ~f:(fun file ->
          String.is_prefix file ~prefix:"docs-src/agent-server/"
          && not (String.is_suffix file ~suffix:"protocol-types.md"))
      in
      check_protocol env root;
      check_cli_flag_inventory ();
      check_callable_excerpts env root documents;
      require
        (String.equal
           (load env root "docs-src/agent-server/operator-contracts.md")
           (Docs_inventory.contracts env root))
        "stale operator contracts: run docs_check --refresh ROOT";
      check_json env root authored;
      let navigation = "Readme.md" :: "DEVELOPMENT.md" :: documents in
      Docs_links_test.run ();
      check_links env root navigation;
      check_anchors env root navigation;
      Docs_smoke.shell_actions env root;
      Docs_chatml.run env root;
      Docs_chatml_authoring.run env root;
      Docs_child_authoring.run env root;
      Docs_examples.run env root;
      check_examples env root executable;
      run_example env tools_example [];
      report
        env
        (sprintf
           "Documentation checks passed: %d pages, %d methods; no live provider calls"
           (List.length documents)
           (List.length Agent_protocol.Command.supported_methods))
    | _ -> failwith "usage: docs_check REPOSITORY EXAMPLE_EXECUTABLE TOOLS_EXAMPLE")
;;
