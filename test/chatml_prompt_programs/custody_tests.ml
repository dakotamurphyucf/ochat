open Core

let%test_module "immutable program and parser custody" =
  (module struct
    module C = Chatml_prompt_programs
    module S = Chatml_source
    module P = Chatml_prepared_program
    module B = Chatml_project_build
    module CM = Prompt.Chat_markdown
    module I = Chatmd_parser_input
    module D = Document_schema
    module Storage = Agent_store.Prompt_program_bundle
    module Contract = Chatml.Chatml_compiler_contract

    let ok = function
      | Ok value -> value
      | Error _ -> failwith "valid custody rejected"
    ;;

    let reject = function
      | Error _ -> ()
      | Ok _ -> failwith "invalid custody admitted"
    ;;

    let checkpoint = ignore
    let contract = Contract.for_target One_off_v1 ~checkpoint
    let compiler_contract = Contract.fingerprint contract ~checkpoint

    let limits
          ?(programs = 8)
          ?(inputs = 16)
          ?(sources = 32)
          ?(bytes = 8 * 1024 * 1024)
          ?(work = 1_000_000)
          ?(document = D.Limits.default)
          ()
      =
      C.Limits.create
        ~max_programs:programs
        ~max_parser_inputs:inputs
        ~max_logical_sources:sources
        ~max_total_input_bytes:bytes
        ~project_work_steps:work
        ~document
      |> Or_error.ok_exn
    ;;

    let parser_limits =
      CM.Limits.create
        ~max_input_bytes:(8 * 1024 * 1024)
        ~max_parse_calls:64
        ~max_tokens:10000
        ~max_depth:32
      |> Or_error.ok_exn
    ;;

    let bundle root sources =
      Chatmd_source_bundle.create ~root_file:root ~sources () |> Result.ok_or_failwith
    ;;

    let parse ?(preprocess = Fn.id) dir root sources =
      let body = List.Assoc.find_exn sources root ~equal:String.equal in
      CM.prepare_chat_inputs
        ~source:root
        ~source_loader:(Source_loader.captured_filesystem ~root:dir ~sources)
        ~dir
        ~preprocess
        ~limits:parser_limits
        ~checkpoint
        body
    ;;

    let replay dir authored captured =
      CM.replay_chat_inputs
        (C.root_input captured)
        ~source_loader:(Chatmd_source_bundle.loader authored ~root:dir)
        ~dir
        ~limits:parser_limits
        ~checkpoint
    ;;

    let binding parsed id =
      CM.Parsed.declarations parsed
      |> List.find_exn ~f:(fun d -> String.equal (CM.Declaration.script_id d) id)
      |> fun declaration ->
      C.Binding.of_declaration parsed declaration ~checkpoint |> Or_error.ok_exn
    ;;

    let make_graph
          ?(namespace = "pkg\000\255")
          ?(name = "env\000\255")
          ?(contract = contract)
          body
      =
      let digest = Contract.fingerprint contract ~checkpoint in
      let source path contents =
        S.create
          (S.Id.create ~package_namespace:namespace ~relative_path:path |> Or_error.ok_exn)
          ~contents
          ~expected_digest:None
          ~max_bytes:None
          ~checkpoint
        |> Or_error.ok_exn
      in
      let entry = source "inline.chatml" body in
      let implementation = source "graph.chatml" "let value = 3" in
      let interface = source "graph.chatmli" "val value : int" in
      let location =
        S.locate entry ~start_byte:0 ~end_byte:0 ~checkpoint |> Or_error.ok_exn
      in
      let definition =
        P.Definition.create
          ~implementation:(S.id implementation)
          ~interface:(Some (S.id interface))
          ~declared_at:location
        |> Or_error.ok_exn
      in
      let scopes =
        List.map [ entry; implementation; interface ] ~f:(fun source ->
          P.Scope.create
            (S.id source)
            ~bindings:
              (if S.Id.equal (S.id source) (S.id entry)
               then
                 [ P.Scope.
                     { name = "Graph"
                     ; module_id = P.Definition.id definition
                     ; declared_at = location
                     }
                 ; P.Scope.
                     { name = "Again"
                     ; module_id = P.Definition.id definition
                     ; declared_at = location
                     }
                 ]
               else [])
            ~selected_at:None
            ~checkpoint
          |> Or_error.ok_exn)
      in
      P.create
        ~max_input_bytes:None
        ~compiler_contract:digest
        ~environment:
          (P.Environment.create ~name ~required_contract:digest ~declared_at:None
           |> Or_error.ok_exn)
        ~provenance:Implicit
        ~entry:(S.id entry)
        ~sources:[ entry; implementation; interface ]
        ~definitions:[ definition ]
        ~scopes
        ~checkpoint
        ()
      |> ok
    ;;

    let fixture dir =
      let body = "let main = 0" in
      let tag =
        "<script id=\"m\" language=\"chatml\" kind=\"moderator\">" ^ body ^ "</script>"
      in
      let raw = "<!-- é -->\r\n" ^ tag in
      let sources = [ "root.chatmd", raw ] in
      let authored = bundle "root.chatmd" sources in
      let parsed =
        parse ~preprocess:(fun raw -> "\r\n" ^ raw) dir "root.chatmd" sources
      in
      authored, parsed, binding parsed "m", make_graph body
    ;;

    let capture
          ?(checkpoint = checkpoint)
          ?(bounds = limits ())
          (authored, parsed, binding, prepared)
      =
      C.capture
        authored
        ~parsed_contexts:[ parsed ]
        ~root:(CM.Parsed.root_input parsed)
        ~programs:[ binding, Provided prepared ]
        ~active_programs:[ binding, compiler_contract ]
        ~limits:bounds
        ~checkpoint
    ;;

    let store_with_json captured json =
      let document = D.Document.inspect ~limits:D.Limits.default json |> ok in
      Storage.create
        ~metadata:document
        ~payloads:(Storage.payloads (C.to_storage captured))
      |> ok
    ;;

    let document captured =
      Storage.Manifest.metadata (Storage.manifest (C.to_storage captured))
    ;;

    let rec replace json path value =
      match path, json with
      | [], _ -> value
      | name :: rest, `Object fields ->
        `Object
          (List.map fields ~f:(fun (key, old) ->
             key, if String.equal key name then replace old rest value else old))
      | index :: rest, `Array values ->
        `Array
          (List.mapi values ~f:(fun i old ->
             if String.equal index (Int.to_string i) then replace old rest value else old))
      | _ -> failwith "invalid handwritten fixture path"
    ;;

    let%expect_test
        "exact effective declaration and opaque graph identities survive custody"
      =
      Eio_main.run (fun env ->
        let dir = Eio.Stdenv.cwd env in
        let ((authored, parsed, binding, prepared) as f) = fixture dir in
        let captured = capture f |> ok in
        let storage = C.to_storage captured in
        let retained =
          C.of_storage storage ~authored ~limits:(limits ()) ~checkpoint |> ok
        in
        let restored =
          C.restore
            retained
            ~parsed:(replay dir authored retained)
            ~contracts:[ binding, contract ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        let program = List.hd_exn (C.Restored.programs restored) in
        let actual = C.Restored.Program.prepared program in
        assert (S.Digest.equal (P.fingerprint prepared) (P.fingerprint actual));
        assert (String.equal (S.Id.package_namespace (P.entry actual)) "pkg\000\255");
        assert (String.equal (P.Environment.name (P.environment actual)) "env\000\255");
        [%test_eq: int] (C.Binding.start_byte binding) (2 + String.length "<!-- é -->\r\n");
        [%test_eq: int] (List.length (P.sources actual)) 3;
        assert (
          not
            (String.equal
               (I.authored_sha256 (C.root_input retained))
               (I.effective_sha256 (C.root_input retained))));
        printf
          "declaration=%s range=%d..%d\n"
          (C.Binding.script_id binding)
          (C.Binding.start_byte binding)
          (C.Binding.end_byte binding);
        printf
          "root-replayed=%b namespace-bytes=%d environment-bytes=%d\n"
          (String.equal
             (I.effective (CM.Parsed.root_input parsed))
             (I.effective (C.root_input retained)))
          (String.length (S.Id.package_namespace (P.entry actual)))
          (String.length (P.Environment.name (P.environment actual)));
        printf
          "sources=%d aliases=%d fingerprint-preserved=%b private-payloads=%d\n"
          (List.length (P.sources actual))
          (List.length
             (P.Scope.bindings (P.find_scope actual (P.entry actual) |> Option.value_exn)))
          (S.Digest.equal (P.fingerprint prepared) (P.fingerprint actual))
          (List.length (Storage.payloads storage)));
      [%expect
        {|
      declaration=m range=15..86
      root-replayed=true namespace-bytes=5 environment-bytes=5
      sources=3 aliases=2 fingerprint-preserved=true private-payloads=4
      |}]
    ;;

    let%test_unit
        "metadata/source/interface/selection tampering rejects and retained original \
         remains valid"
      =
      Eio_main.run (fun env ->
        let dir = Eio.Stdenv.cwd env in
        let ((authored, _, binding, _) as f) = fixture dir in
        let captured = capture f |> ok in
        let json = D.Document.json (document captured) in
        let cases =
          [ [ "payload"; "root" ], `String (String.make 64 '0')
          ; [ "payload"; "inputs"; "0"; "effective_bytes" ], `Number "0"
          ; [ "payload"; "sources"; "0"; "digest" ], `String (String.make 64 '0')
          ; [ "payload"; "programs"; "0"; "sources" ], `Array []
          ; ( [ "payload"
              ; "programs"
              ; "0"
              ; "graph"
              ; "payload"
              ; "definitions"
              ; "0"
              ; "interface"
              ; "path"
              ]
            , `String "missing.chatmli" )
          ; [ "payload"; "selections"; "0"; "binding"; "start_byte" ], `Number "0"
          ]
        in
        List.iter cases ~f:(fun (path, value) ->
          let storage = store_with_json captured (replace json path value) in
          match C.of_storage storage ~authored ~limits:(limits ()) ~checkpoint with
          | Error _ -> ()
          | Ok admitted ->
            reject
              (C.restore
                 admitted
                 ~parsed:(replay dir authored admitted)
                 ~contracts:[ binding, contract ]
                 ~limits:(limits ())
                 ~checkpoint));
        let extra = Storage.Payload.create "surplus" |> ok in
        let surplus =
          Storage.create
            ~metadata:(document captured)
            ~payloads:(extra :: Storage.payloads (C.to_storage captured))
          |> ok
        in
        reject (C.of_storage surplus ~authored ~limits:(limits ()) ~checkpoint);
        let retained =
          C.of_storage (C.to_storage captured) ~authored ~limits:(limits ()) ~checkpoint
          |> ok
        in
        ignore
          (C.restore
             retained
             ~parsed:(replay dir authored retained)
             ~contracts:[ binding, contract ]
             ~limits:(limits ())
             ~checkpoint
           |> ok
           : C.Restored.t))
    ;;

    let%test_unit
        "retained child restores only active role while preserving inactive native \
         parent and unknown fields"
      =
      Eio_main.run (fun env ->
        let dir = Eio.Stdenv.cwd env in
        let own_body = "let parent = 1"
        and child_body = "let main = 0" in
        let parent =
          "<script id=\"own\" language=\"chatml\" kind=\"tool\">"
          ^ own_body
          ^ "</script><import src=\"child.chatmd\" namespace=\"child\"/>"
        in
        let child =
          "<script id=\"m\" language=\"chatml\" kind=\"moderator\">"
          ^ child_body
          ^ "</script>"
        in
        let sources = [ "parent.chatmd", parent; "child.chatmd", child ] in
        let authored = bundle "parent.chatmd" sources in
        let parent_parsed = parse dir "parent.chatmd" sources
        and child_parsed = parse dir "child.chatmd" sources in
        let own = binding parent_parsed "own"
        and imported = binding parent_parsed "child:m" in
        let current = binding child_parsed "m" in
        let native =
          Contract.for_static_surface
            (Chatml.Chatml_static_surface.of_runtime_surface
               Chatml.Chatml_builtin_surface.empty
               ~checkpoint)
            ~required_bindings:[]
        in
        let native_digest = Contract.fingerprint native ~checkpoint in
        let parent_graph = make_graph ~namespace:"parent" ~contract:native own_body
        and child_graph = make_graph child_body in
        let captured =
          C.capture
            authored
            ~parsed_contexts:[ parent_parsed; child_parsed ]
            ~root:(CM.Parsed.root_input parent_parsed)
            ~programs:[ own, Provided parent_graph; imported, Provided child_graph ]
            ~active_programs:[ own, native_digest; imported, compiler_contract ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        let original = D.Document.json (document captured) in
        let unknown =
          match original with
          | `Object fields -> `Object (("future", `String "retained") :: fields)
          | _ -> assert false
        in
        let captured =
          C.of_storage
            (store_with_json captured unknown)
            ~authored
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        let child_authored = bundle "child.chatmd" sources in
        let rerooted =
          C.reroot
            captured
            ~authored:child_authored
            ~parsed:child_parsed
            ~programs:[ imported, compiler_contract, current ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        let rerooted =
          C.adopt
            rerooted
            ~previous:captured
            ~authored:child_authored
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        let incomplete =
          C.capture
            child_authored
            ~parsed_contexts:[ parent_parsed; child_parsed ]
            ~root:(CM.Parsed.root_input child_parsed)
            ~programs:[ current, Provided child_graph ]
            ~active_programs:[ current, compiler_contract ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        reject
          (C.adopt
             incomplete
             ~previous:captured
             ~authored:child_authored
             ~limits:(limits ())
             ~checkpoint);
        reject
          (C.extend
             captured
             ~authored
             ~parsed_contexts:[]
             ~programs:[]
             ~limits:(limits ())
             ~checkpoint);
        reject
          (C.extend
             captured
             ~authored
             ~parsed_contexts:[ child_parsed ]
             ~programs:[]
             ~limits:(limits ())
             ~checkpoint);
        let restarted =
          C.of_storage
            (C.to_storage rerooted)
            ~authored:child_authored
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        [%test_eq: int] (List.length (C.bindings restarted)) 2;
        assert (
          D.Json.equal
            (D.Json.field (D.Document.json (document restarted)) ~name:"future"
             |> function
             | D.Json.Value v -> v
             | _ -> assert false)
            (`String "retained"));
        let restored =
          C.restore
            restarted
            ~parsed:(replay dir child_authored restarted)
            ~contracts:[ current, contract ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        [%test_eq: int] (List.length (C.Restored.programs restored)) 1;
        let again =
          C.reroot
            restarted
            ~authored:child_authored
            ~parsed:child_parsed
            ~programs:[ imported, compiler_contract, current ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        assert (
          Storage.Manifest.equal
            (Storage.manifest (C.to_storage restarted))
            (Storage.manifest (C.to_storage again)));
        let malformed =
          replace
            (D.Document.json (document restarted))
            [ "payload"; "programs"; "0"; "graph"; "payload"; "fingerprint" ]
            (`Number "0")
        in
        reject
          (C.of_storage
             (store_with_json restarted malformed)
             ~authored:child_authored
             ~limits:(limits ())
             ~checkpoint))
    ;;

    let%test_unit
        "selected custody restores exact lock through its owner and preserves actual \
         Build kind"
      =
      Eio_main.run (fun env ->
        let dir = Eio.Stdenv.cwd env in
        let fixture = Fixtures.make () in
        let build = Fixtures.compose fixture |> Support.ok in
        let prepared = B.prepared build in
        let source = P.find_source prepared (P.entry prepared) |> Option.value_exn in
        let body = S.contents source in
        let root =
          "<script id=\"m\" language=\"chatml\" kind=\"moderator\">" ^ body ^ "</script>"
        in
        let authored = bundle "root.chatmd" [ "root.chatmd", root ] in
        let parsed = parse dir "root.chatmd" (Chatmd_source_bundle.sources authored) in
        let binding = binding parsed "m" in
        let lock = B.lock_source build in
        let formatted =
          S.create
            (S.id lock)
            ~contents:("\n " ^ S.contents lock ^ " \n")
            ~expected_digest:None
            ~max_bytes:None
            ~checkpoint
          |> Or_error.ok_exn
        in
        let restored_build =
          B.restore
            formatted
            ~sources:(Support.without_lock build)
            ~contract
            ~limits:(Support.limits ())
            ~checkpoint
          |> Support.ok
        in
        let captured =
          C.capture
            authored
            ~parsed_contexts:[ parsed ]
            ~root:(CM.Parsed.root_input parsed)
            ~programs:[ binding, Selected restored_build ]
            ~active_programs:[ binding, compiler_contract ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        let retained =
          C.of_storage (C.to_storage captured) ~authored ~limits:(limits ()) ~checkpoint
          |> ok
        in
        let restored =
          C.restore
            retained
            ~parsed:(replay dir authored retained)
            ~contracts:[ binding, contract ]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        match C.Restored.Program.input (List.hd_exn (C.Restored.programs restored)) with
        | Provided _ -> assert false
        | Selected actual ->
          Support.assert_same_source formatted (B.lock_source actual);
          assert (
            S.Digest.equal
              (P.fingerprint (B.prepared actual))
              (P.fingerprint (B.prepared restored_build))))
    ;;

    let%test_unit
        "exact document/input bounds, early oversized identity and checkpoint identity"
      =
      Eio_main.run (fun env ->
        let dir = Eio.Stdenv.cwd env in
        let ((authored, parsed, binding, prepared) as f) = fixture dir in
        let captured = capture f |> ok in
        let exact =
          D.Json.validate_and_measure
            ~limits:D.Limits.default
            (D.Document.json (document captured))
          |> ok
        in
        let doc bytes =
          D.Limits.create
            ~max_bytes:bytes
            ~max_depth:64
            ~max_fields:10000
            ~max_nodes:10000
          |> ok
        in
        ignore (capture ~bounds:(limits ~document:(doc exact) ()) f |> ok : C.t);
        reject (capture ~bounds:(limits ~document:(doc (exact - 1)) ()) f);
        let bytes = P.Accounting.input_bytes (P.accounting prepared) in
        ignore (capture ~bounds:(limits ~bytes ()) f |> ok : C.t);
        reject (capture ~bounds:(limits ~bytes:(bytes - 1) ()) f);
        let huge = make_graph ~namespace:(String.make 10000 'p') "let main = 0" in
        let polls = ref 0 in
        reject
          (C.capture
             authored
             ~parsed_contexts:[ parsed ]
             ~root:(CM.Parsed.root_input parsed)
             ~programs:[ binding, Provided huge ]
             ~active_programs:[ binding, compiler_contract ]
             ~limits:(limits ~document:(doc 1024) ())
             ~checkpoint:(fun () -> incr polls));
        assert (!polls < 20);
        let foreign = parse dir "root.chatmd" (Chatmd_source_bundle.sources authored) in
        reject
          (C.Binding.of_declaration
             parsed
             (List.hd_exn (CM.Parsed.declarations foreign))
             ~checkpoint);
        let parser_only checkpoint =
          C.capture
            authored
            ~parsed_contexts:[ parsed ]
            ~root:(CM.Parsed.root_input parsed)
            ~programs:[]
            ~active_programs:[]
            ~limits:(limits ())
            ~checkpoint
          |> ok
        in
        Support.assert_checkpoint_identity ~f:(fun checkpoint ->
          ignore (parser_only checkpoint : C.t));
        Support.assert_checkpoint_identity ~f:(fun checkpoint ->
          ignore
            (C.of_storage
               (C.to_storage captured)
               ~authored
               ~limits:(limits ())
               ~checkpoint
             |> ok
             : C.t));
        Support.assert_checkpoint_identity ~f:(fun checkpoint ->
          ignore
            (C.restore
               captured
               ~parsed:(replay dir authored captured)
               ~contracts:[ binding, contract ]
               ~limits:(limits ())
               ~checkpoint
             |> ok
             : C.Restored.t)))
    ;;

    let%test_unit
        "graph-local source revisions and whole-custody allowances remain independent"
      =
      Eio_main.run (fun env ->
        let dir = Eio.Stdenv.cwd env in
        let fixture = Fixtures.make () in
        let build = Fixtures.compose fixture |> Support.ok in
        let selected = B.prepared build in
        let original path =
          P.find_source selected (Support.id ~namespace:"lib" path) |> Option.value_exn
        in
        let changed_util =
          Support.capture ~namespace:"lib" "src/util.chatml" "let value = 9"
        in
        let changed_catalog =
          Support.catalog
            (original "chatml.project.json")
            ~request:(Chatml_project.Selection.Library { environment = None })
            ~members:[ changed_util; original "src/util.chatmli" ]
        in
        let changed_input =
          B.Catalog_input.create
            changed_catalog
            ~origin:Local
            ~acquisition_key:"independent-project"
            ~dependencies:[]
        in
        let second_build =
          Fixtures.compose fixture ~catalogs:[ changed_input ] |> Support.ok
        in
        let second_selected = B.prepared second_build in
        assert (
          S.Id.equal (S.id (B.lock_source build)) (S.id (B.lock_source second_build)));
        assert (
          not
            (S.Digest.equal
               (S.digest (B.lock_source build))
               (S.digest (B.lock_source second_build))));
        let body =
          S.contents (P.find_source selected (P.entry selected) |> Option.value_exn)
        in
        let other = "let main = 1" in
        let tag id body =
          "<script id=\""
          ^ id
          ^ "\" kind=\"tool\" language=\"chatml\">"
          ^ body
          ^ "</script>"
        in
        let raw = tag "a" body ^ tag "b" other ^ tag "s1" body ^ tag "s2" body in
        let authored = bundle "root.chatmd" [ "root.chatmd", raw ] in
        let parsed = parse dir "root.chatmd" (Chatmd_source_bundle.sources authored) in
        let a = binding parsed "a"
        and b = binding parsed "b"
        and s1 = binding parsed "s1"
        and s2 = binding parsed "s2" in
        let first = make_graph body
        and second = make_graph other in
        assert (S.Id.equal (P.entry first) (P.entry second));
        assert (not (S.Digest.equal (P.fingerprint first) (P.fingerprint second)));
        let programs =
          [ a, C.Input.Provided first
          ; b, Provided second
          ; s1, Selected build
          ; s2, Selected second_build
          ]
        in
        let active =
          List.map [ a; b; s1; s2 ] ~f:(fun binding -> binding, compiler_contract)
        in
        let total =
          List.sum
            (module Int)
            [ first; second; selected; second_selected ]
            ~f:(fun p -> P.Accounting.input_bytes (P.accounting p))
        in
        let restored_build =
          B.restore
            (B.lock_source build)
            ~sources:(Support.without_lock build)
            ~contract
            ~limits:(Support.limits ())
            ~checkpoint
          |> Support.ok
        in
        let second_restored =
          B.restore
            (B.lock_source second_build)
            ~sources:(Support.without_lock second_build)
            ~contract
            ~limits:(Support.limits ())
            ~checkpoint
          |> Support.ok
        in
        let work =
          B.Accounting.work_steps (B.accounting restored_build)
          + B.Accounting.work_steps (B.accounting second_restored)
        in
        let capture bounds =
          C.capture
            authored
            ~parsed_contexts:[ parsed ]
            ~root:(CM.Parsed.root_input parsed)
            ~programs
            ~active_programs:active
            ~limits:bounds
            ~checkpoint
        in
        (* Four revisions in the two provided graphs, plus seven in the first
         selected graph and exactly two changed revisions: Util and its lock. *)
        let source_count = 4 + List.length (P.sources selected) + 2 in
        let captured = capture (limits ~bytes:total ~sources:source_count ()) |> ok in
        reject (capture (limits ~bytes:total ~sources:(source_count - 1) ()));
        let shared = S.Digest.to_hex (S.digest (Fixtures.helper fixture)) in
        [%test_eq: int]
          (List.count
             (Storage.payloads (C.to_storage captured))
             ~f:(fun payload -> String.equal (Storage.Payload.sha256 payload) shared))
          1;
        reject (capture (limits ~bytes:(total - 1) ()));
        let restarted =
          C.of_storage
            (C.to_storage captured)
            ~authored
            ~limits:(limits ~bytes:total ())
            ~checkpoint
          |> ok
        in
        let contracts = List.map [ a; b; s1; s2 ] ~f:(fun binding -> binding, contract) in
        let restored =
          C.restore
            restarted
            ~parsed
            ~contracts
            ~limits:(limits ~bytes:total ~work ())
            ~checkpoint
          |> ok
        in
        List.iter
          [ a, first; b, second; s1, selected; s2, second_selected ]
          ~f:(fun (binding, expected) ->
            let actual =
              C.Restored.find restored binding ~compiler_contract
              |> Option.value_exn
              |> C.Restored.Program.prepared
            in
            assert (S.Id.equal (P.entry actual) (P.entry expected));
            assert (S.Digest.equal (P.fingerprint actual) (P.fingerprint expected)));
        reject
          (C.restore
             restarted
             ~parsed
             ~contracts
             ~limits:(limits ~bytes:(total - 1) ~work ())
             ~checkpoint);
        reject
          (C.restore
             restarted
             ~parsed
             ~contracts
             ~limits:(limits ~bytes:total ~work:(work - 1) ())
             ~checkpoint);
        let json = D.Document.json (document captured) in
        let descriptors =
          D.Json.field (D.Document.payload (document captured)) ~name:"sources"
          |> function
          | D.Json.Value (`Array values) -> values
          | _ -> assert false
        in
        (* Two globally valid revisions of one graph-local ID cannot both enter
         any one program's reference inventory, including inactive programs. *)
        let revisions =
          List.filter descriptors ~f:(fun descriptor ->
            match D.Json.field descriptor ~name:"id" with
            | D.Json.Value id ->
              (match D.Json.field id ~name:"path" with
               | D.Json.Value (`String path) -> String.equal path "inline.chatml"
               | _ -> false)
            | _ -> false)
        in
        [%test_eq: int] (List.length revisions) 2;
        let duplicate_id =
          replace json [ "payload"; "programs"; "0"; "sources" ] (`Array revisions)
        in
        reject
          (C.of_storage
             (store_with_json captured duplicate_id)
             ~authored
             ~limits:(limits ())
             ~checkpoint))
    ;;
  end)
;;
