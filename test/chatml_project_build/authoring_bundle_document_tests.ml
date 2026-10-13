open Core
module A = Chatml_authoring_bundle_document
module B = Chatml_project_build
module P = Chatml_project
module R = Chatml_prepared_program
module S = Chatml_source
module W = Agent_protocol.Authoring_bundle.Bundle
module M = Agent_protocol.Authoring_common.Source
module Selection = Agent_protocol.Authoring_context.Context.Selection
module CM = Prompt.Chat_markdown

let wire_ok = function
  | Ok value -> value
  | Error _ -> failwith "valid document wire metadata rejected"
;;

let metadata source =
  M.create
    ~id:(S.id source)
    ~digest:(S.digest source)
    ~byte_length:(S.byte_length source)
    ~editor_revision:None
  |> wire_ok
;;

let member source = W.Member.create ~source:(metadata source) |> wire_ok

let document_member source path =
  let member = Source.Portable_path.create path |> Or_error.ok_exn in
  W.Manifest.Document_member.create ~source:(S.id source) ~member |> wire_ok
;;

let manifest ~root ~document_members ~scripts ~sources =
  let document =
    W.Manifest.Chatmd_document.create ~root_source:(S.id root) ~document_members ~scripts
    |> wire_ok
  in
  W.Manifest.create
    ~basis:(Chatmd_document document)
    ~members:(List.map sources ~f:member)
  |> wire_ok
;;

let selector_of_parsed ?declaration_source ~dir root sources =
  let bundle =
    Chatmd_source_bundle.create ~root_file:"entry.chatmd" ~sources ()
    |> Result.ok_or_failwith
  in
  let parsed = CM.parse_source_bundle ~dir bundle in
  let declaration = List.hd_exn (CM.Parsed.declarations parsed.parsed_root) in
  let binding =
    Chatml_prompt_programs.Binding.of_declaration
      parsed.parsed_root
      declaration
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let selector =
    W.Manifest.Script.Selector.create
      ~registration_source:(S.id root)
      ~declaration_source:(S.id (Option.value declaration_source ~default:root))
      ~start_byte:(Chatml_prompt_programs.Binding.start_byte binding)
      ~end_byte:(Chatml_prompt_programs.Binding.end_byte binding)
      ~script_id:(Chatml_prompt_programs.Binding.script_id binding)
      ~body_digest:(Chatml_prompt_programs.Binding.script_digest binding)
      ~body_byte_length:(String.length (CM.Declaration.script_body declaration))
    |> wire_ok
  in
  selector, declaration
;;

let limits ?(work = 1_000_000) ?(input_bytes = 16_000_000) () =
  Chatml_authoring_bundle_project.Limits.create
    ~max_work_steps:work
    ~max_retained_input_bytes:input_bytes
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let%test_unit "actual selected ChatMD script binds its captured Project entry" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.fs env in
    let contract =
      Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
    in
    let original = Recomposition_tests.library contract in
    let entry =
      R.find_source (B.prepared original) (R.entry (B.prepared original))
      |> Option.value_exn
    in
    let body = S.contents entry in
    let root =
      Support.capture
        ~namespace:"role"
        "entry.chatmd"
        (sprintf
           {|<script id="main" language="chatml" kind="moderator" project="projects/chatml.project.json">%s</script>|}
           body)
    in
    let catalog_sources =
      Support.without_lock original
      |> List.filter ~f:(fun source -> not (S.Id.equal (S.id source) (S.id entry)))
    in
    let declaration =
      List.find_exn catalog_sources ~f:(fun source ->
        S.Id.equal (S.id source) (P.Lock.root (B.lock original)))
    in
    let selector, actual_declaration =
      selector_of_parsed
        ~dir
        root
        [ "entry.chatmd", S.contents root
        ; "projects/chatml.project.json", S.contents declaration
        ]
    in
    [%test_eq: string] (CM.Declaration.script_body actual_declaration) body;
    let routes =
      List.map
        (P.Lock.catalogs (B.lock original))
        ~f:(fun catalog ->
          W.Manifest.Route.create
            ~declaration:(P.Lock.Catalog.declaration catalog)
            ~origin:(P.Lock.Catalog.origin catalog)
            ~dependencies:(P.Lock.Catalog.dependencies catalog)
          |> wire_ok)
    in
    let entry_metadata =
      W.Manifest.Project.Captured_entry.create
        ~source:(metadata entry)
        ~delivery:Derived_script
      |> wire_ok
    in
    let make_project entry ~source_members =
      W.Manifest.Project.create
        ~root_source:(S.id declaration)
        ~selection:(Selection.create ~target:Library ~environment:None |> wire_ok)
        ~entry
        ~source_members:(List.map source_members ~f:S.id)
        ~routes
        ~mode:
          (Compose
             { selected_previous_lock = None
             ; generated_lock = Support.id ~namespace:"role" "uploaded.lock.json"
             })
      |> wire_ok
    in
    let project =
      make_project (Captured entry_metadata) ~source_members:catalog_sources
    in
    let script =
      W.Manifest.Script.create ~selector ~choice:(Selected_project project) |> wire_ok
    in
    let documents =
      [ document_member root "entry.chatmd"
      ; document_member declaration "projects/chatml.project.json"
      ]
    in
    let upload =
      manifest
        ~root
        ~document_members:documents
        ~scripts:[ script ]
        ~sources:(root :: catalog_sources)
    in
    let selected = [ selector, A.Selection.create ~contract ~previous:None ] in
    let validate upload sources =
      A.validate
        upload
        ~sources
        ~dir
        ~selected_projects:selected
        ~limits:(limits ())
        ~checkpoint:ignore
    in
    let result =
      match validate upload (root :: catalog_sources) with
      | Ok value -> value
      | Error _ -> failwith "genuine selected document closure rejected"
    in
    let selected = List.hd_exn (A.Validated.scripts result) in
    let built = A.Validated.Script.project selected |> Option.value_exn in
    Support.assert_same_source
      entry
      (R.find_source
         (B.prepared (Chatml_authoring_bundle_project.Validated.build built))
         (S.id entry)
       |> Option.value_exn);
    let falsely_source_only =
      W.Manifest.Script.create ~selector ~choice:Source_only |> wire_ok
    in
    let source_only_upload =
      manifest
        ~root
        ~document_members:documents
        ~scripts:[ falsely_source_only ]
        ~sources:[ root; declaration ]
    in
    (match
       A.validate
         source_only_upload
         ~sources:[ root; declaration ]
         ~dir
         ~selected_projects:[]
         ~limits:(limits ())
         ~checkpoint:ignore
     with
     | Error problem ->
       (match A.Error.problem problem with
        | Dynamic_project_unavailable -> ()
        | _ -> failwith "authored project was mislabeled for another reason")
     | Ok _ -> failwith "authored project was mislabeled Source_only");
    let external_document =
      Support.capture
        ~namespace:"role"
        "external.chatmd"
        {|<script id="main" language="chatml" kind="moderator" project="projects/chatml.project.json" src="script.chatml"/>|}
    in
    let external_body = Support.capture ~namespace:"role" "script.chatml" body in
    let external_selector, _ =
      selector_of_parsed
        ~dir
        external_document
        [ "entry.chatmd", S.contents external_document
        ; "projects/chatml.project.json", S.contents declaration
        ; "script.chatml", body
        ]
    in
    let external_entry =
      W.Manifest.Project.Captured_entry.create
        ~source:(metadata entry)
        ~delivery:Uploaded_member
      |> wire_ok
    in
    let external_project =
      make_project (Captured external_entry) ~source_members:(entry :: catalog_sources)
    in
    let external_script =
      W.Manifest.Script.create
        ~selector:external_selector
        ~choice:(Selected_project external_project)
      |> wire_ok
    in
    let external_upload =
      manifest
        ~root:external_document
        ~document_members:
          [ document_member external_document "entry.chatmd"
          ; document_member declaration "projects/chatml.project.json"
          ; document_member external_body "script.chatml"
          ]
        ~scripts:[ external_script ]
        ~sources:(external_document :: external_body :: entry :: catalog_sources)
    in
    (match
       A.validate
         external_upload
         ~sources:(external_document :: external_body :: entry :: catalog_sources)
         ~dir
         ~selected_projects:
           [ external_selector, A.Selection.create ~contract ~previous:None ]
         ~limits:(limits ())
         ~checkpoint:ignore
     with
     | Error problem ->
       (match A.Error.problem problem with
        | Script_body_mismatch -> ()
        | _ -> failwith "wrong external entry was rejected for another reason")
     | Ok _ -> failwith "external body was rebound to a different entry source");
    let wrong_entry =
      Support.capture ~namespace:"host" "wrong.chatml" "let initial_state = 999"
    in
    let wrong_captured =
      W.Manifest.Project.Captured_entry.create
        ~source:(metadata wrong_entry)
        ~delivery:Uploaded_member
      |> wire_ok
    in
    let wrong_project =
      make_project
        (Captured wrong_captured)
        ~source_members:(wrong_entry :: catalog_sources)
    in
    match W.Manifest.Script.create ~selector ~choice:(Selected_project wrong_project) with
    | Error _ -> ()
    | Ok _ -> failwith "foreign Project entry body passed script-wire admission")
;;

let%test_unit "imported external source-only script has exact document closure" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.fs env in
    let root =
      Support.capture
        ~namespace:"docs"
        "root.chatmd"
        {|<import src="parts/definitions.chatmd" namespace="lab"/>|}
    in
    let imported =
      Support.capture
        ~namespace:"docs"
        "parts/definitions.chatmd"
        {|<script id="worker" language="chatml" kind="tool" src="worker.chatml"/><tool name="work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|}
    in
    let worker =
      Support.capture
        ~namespace:"docs"
        "parts/worker.chatml"
        "let run = fun ctx input -> Task.pure(`Complete(input))"
    in
    let schema = Support.capture ~namespace:"docs" "parts/schema.json" "true" in
    let sources =
      [ "entry.chatmd", S.contents root
      ; "parts/definitions.chatmd", S.contents imported
      ; "parts/worker.chatml", S.contents worker
      ; "parts/schema.json", S.contents schema
      ]
    in
    let selector, _ = selector_of_parsed ~declaration_source:imported ~dir root sources in
    let script = W.Manifest.Script.create ~selector ~choice:Source_only |> wire_ok in
    let documents =
      [ document_member root "entry.chatmd"
      ; document_member imported "parts/definitions.chatmd"
      ; document_member worker "parts/worker.chatml"
      ; document_member schema "parts/schema.json"
      ]
    in
    let inventory = [ root; imported; worker; schema ] in
    let upload =
      manifest ~root ~document_members:documents ~scripts:[ script ] ~sources:inventory
    in
    let validate upload sources =
      A.validate
        upload
        ~sources
        ~dir
        ~selected_projects:[]
        ~limits:(limits ())
        ~checkpoint:ignore
    in
    let validated =
      match validate upload inventory with
      | Ok value -> value
      | Error _ -> failwith "genuine imported source-only script rejected"
    in
    let selected = List.hd_exn (A.Validated.scripts validated) in
    assert (Option.is_none (A.Validated.Script.project selected));
    [%test_eq: string]
      (Source.Portable_path.to_string
         (Chatml_prompt_programs.Binding.declaration_path
            (A.Validated.Script.binding selected)))
      "parts/definitions.chatmd";
    let spare = Support.capture ~namespace:"docs" "spare.txt" "unused" in
    let surplus =
      manifest
        ~root
        ~document_members:(document_member spare "spare.txt" :: documents)
        ~scripts:[ script ]
        ~sources:(spare :: inventory)
    in
    (match validate surplus (spare :: inventory) with
     | Error problem ->
       (match A.Error.problem problem with
        | Missing_or_surplus_document_member -> ()
        | _ -> failwith "unused document source rejected for another reason")
     | Ok _ -> failwith "unused document source entered closure");
    let missing =
      manifest
        ~root
        ~document_members:
          (List.filter documents ~f:(fun member ->
             not (S.Id.equal (W.Manifest.Document_member.source member) (S.id worker))))
        ~scripts:[ script ]
        ~sources:[ root; imported; schema ]
    in
    (match validate missing [ root; imported; schema ] with
     | Error problem ->
       (match A.Error.problem problem with
        | Parser Source_unavailable -> ()
        | _ -> failwith "missing external source rejected for another reason")
     | Ok _ -> failwith "missing external source entered closure");
    let polls = ref 0 in
    ignore
      (A.validate
         upload
         ~sources:inventory
         ~dir
         ~selected_projects:[]
         ~limits:(limits ())
         ~checkpoint:(fun () -> incr polls)
       |> function
       | Ok value -> value
       | Error _ -> failwith "counted external closure rejected"
       : A.Validated.t);
    assert (!polls > 10);
    let exception Interrupted of unit ref in
    let identity = ref () in
    let reached = ref 0 in
    match
      A.validate
        upload
        ~sources:inventory
        ~dir
        ~selected_projects:[]
        ~limits:(limits ())
        ~checkpoint:(fun () ->
          incr reached;
          if Int.equal !reached (!polls - 1) then raise (Interrupted identity))
    with
    | exception Interrupted actual -> assert (phys_equal identity actual)
    | Ok _ | Error _ -> failwith "late parser closure interruption was lost")
;;

let%test_unit "two authored script environments select distinct captured Builds" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.fs env in
    let contract =
      Chatml.Chatml_compiler_contract.for_target Moderator_v1 ~checkpoint:ignore
    in
    let original = Recomposition_tests.library contract in
    let original_entry =
      R.find_source (B.prepared original) (R.entry (B.prepared original))
      |> Option.value_exn
    in
    let declaration =
      Support.capture
        ~namespace:"role"
        "chatml.project.json"
        {|{"schema_version":1,"namespace":"role","source_dirs":["src"],"environments":{"default":{"modules":{"Counter":"self.Counter"}},"other":{"modules":{}}}}|}
    in
    let catalog_sources =
      declaration
      :: (Support.without_lock original
          |> List.filter ~f:(fun source ->
            not
              (S.Id.equal (S.id source) (S.id declaration)
               || S.Id.equal (S.id source) (S.id original_entry))))
    in
    let body = S.contents original_entry in
    let root =
      Support.capture
        ~namespace:"role"
        "two.chatmd"
        (sprintf
           {|<script id="first" language="chatml" kind="tool" project="projects/chatml.project.json" environment="default">%s</script><script id="second" language="chatml" kind="tool" project="projects/chatml.project.json" environment="other">%s</script>|}
           body
           body)
    in
    let parsed =
      Chatmd_source_bundle.create
        ~root_file:"entry.chatmd"
        ~sources:
          [ "entry.chatmd", S.contents root
          ; "projects/chatml.project.json", S.contents declaration
          ]
        ()
      |> Result.ok_or_failwith
      |> CM.parse_source_bundle ~dir
    in
    let declarations = CM.Parsed.declarations parsed.parsed_root in
    [%test_eq: int] (List.length declarations) 2;
    let routes =
      List.map
        (P.Lock.catalogs (B.lock original))
        ~f:(fun catalog ->
          W.Manifest.Route.create
            ~declaration:(P.Lock.Catalog.declaration catalog)
            ~origin:(P.Lock.Catalog.origin catalog)
            ~dependencies:(P.Lock.Catalog.dependencies catalog)
          |> wire_ok)
    in
    let selections = [ "default"; "other" ] in
    let scripts, selectors =
      List.map2_exn declarations selections ~f:(fun script_declaration environment ->
        let binding =
          Chatml_prompt_programs.Binding.of_declaration
            parsed.parsed_root
            script_declaration
            ~checkpoint:ignore
          |> Or_error.ok_exn
        in
        let selector =
          W.Manifest.Script.Selector.create
            ~registration_source:(S.id root)
            ~declaration_source:(S.id root)
            ~start_byte:(Chatml_prompt_programs.Binding.start_byte binding)
            ~end_byte:(Chatml_prompt_programs.Binding.end_byte binding)
            ~script_id:(Chatml_prompt_programs.Binding.script_id binding)
            ~body_digest:(Chatml_prompt_programs.Binding.script_digest binding)
            ~body_byte_length:
              (String.length (CM.Declaration.script_body script_declaration))
          |> wire_ok
        in
        let derived_id = Support.id ~namespace:"host" (environment ^ ".chatml") in
        let derived = S.reidentify original_entry ~id:derived_id in
        let captured =
          W.Manifest.Project.Captured_entry.create
            ~source:(metadata derived)
            ~delivery:Derived_script
          |> wire_ok
        in
        let selected =
          Selection.create ~target:Library ~environment:(Some environment) |> wire_ok
        in
        let project =
          W.Manifest.Project.create
            ~root_source:(S.id declaration)
            ~selection:selected
            ~entry:(Captured captured)
            ~source_members:(List.map catalog_sources ~f:S.id)
            ~routes
            ~mode:
              (Compose
                 { selected_previous_lock = None
                 ; generated_lock =
                     Support.id ~namespace:"role" (environment ^ ".uploaded.lock.json")
                 })
          |> wire_ok
        in
        ( W.Manifest.Script.create ~selector ~choice:(Selected_project project) |> wire_ok
        , selector ))
      |> List.unzip
    in
    let upload =
      manifest
        ~root
        ~document_members:
          [ document_member root "entry.chatmd"
          ; document_member declaration "projects/chatml.project.json"
          ]
        ~scripts
        ~sources:(root :: catalog_sources)
    in
    let selected_projects =
      List.map selectors ~f:(fun selector ->
        selector, A.Selection.create ~contract ~previous:None)
    in
    let validated =
      match
        A.validate
          upload
          ~sources:(root :: catalog_sources)
          ~dir
          ~selected_projects
          ~limits:(limits ())
          ~checkpoint:ignore
      with
      | Ok value -> value
      | Error _ -> failwith "two authentic selected environments rejected"
    in
    [%test_eq: string list]
      (List.map (A.Validated.scripts validated) ~f:(fun script ->
         let project = A.Validated.Script.project script |> Option.value_exn in
         P.Lock.environment
           (B.lock (Chatml_authoring_bundle_project.Validated.build project))))
      selections;
    let snapshot_bytes =
      List.sum (module Int) (root :: catalog_sources) ~f:S.byte_length
    in
    let document_bytes = S.byte_length root + S.byte_length declaration in
    let build_bytes =
      List.sum
        (module Int)
        (A.Validated.scripts validated)
        ~f:(fun script ->
          let project = A.Validated.Script.project script |> Option.value_exn in
          let prepared =
            B.prepared (Chatml_authoring_bundle_project.Validated.build project)
          in
          R.Accounting.input_bytes (R.accounting prepared))
    in
    let exact_input =
      snapshot_bytes
      + document_bytes
      + S.byte_length root
      + (2 * String.length body)
      + build_bytes
    in
    let verify input_bytes =
      A.validate
        upload
        ~sources:(root :: catalog_sources)
        ~dir
        ~selected_projects
        ~limits:(limits ~input_bytes ())
        ~checkpoint:ignore
    in
    (match verify exact_input with
     | Ok _ -> ()
     | Error _ -> failwith "exact aggregate retained-input budget rejected");
    match verify (exact_input - 1) with
    | Error problem ->
      (match A.Error.problem problem with
       | Project lower ->
         (match Chatml_authoring_bundle_project.Error.problem lower with
          | Input_limit -> ()
          | Builder build_error ->
            (match Chatml_project_build.Error.problem build_error with
             | Limit_exceeded -> ()
             | Prepared { problem = Input_limit _; _ } -> ()
             | _ -> failwith "aggregate limit surfaced from another builder error")
          | _ -> failwith "aggregate limit failed for another Project reason")
       | _ -> failwith "aggregate limit failed outside Project admission")
    | Ok _ -> failwith "second selected Build reset aggregate input allowance")
;;
