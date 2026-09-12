open! Core
module Metadata = Chatmd_shell_spec.Authoring_metadata

type excerpt =
  { path : string
  ; heading : string
  ; include_children : bool
  }
[@@deriving sexp]

type review =
  | Pending
  | Audited of
      { excerpt_sha256 : string list
      ; evidence : string list
      }
[@@deriving sexp]

type specification =
  { id : string
  ; title : string
  ; prerequisites : string list
  ; surfaces : string list
  ; excerpts : excerpt list
  ; review : review
  }
[@@deriving sexp]

type fragment =
  { source : excerpt
  ; document_sha256 : string
  ; sha256 : string
  ; text : string
  }

type topic =
  { specification : specification
  ; fragments : fragment list
  ; sha256 : string
  }

type t =
  { identity : string
  ; topics : topic String.Map.t
  ; surface_ids : string list
  }

let digest = Chatmd_shell_spec.Source_ref.digest
let identity t = t.identity
let topics t = Map.data t.topics
let unique strings = Option.is_none (List.find_a_dup strings ~compare:String.compare)

let heading_depth line =
  let hashes = String.take_while line ~f:(Char.equal '#') |> String.length in
  match hashes > 0 && hashes <= 6 && String.length line > hashes with
  | true when Char.equal line.[hashes] ' ' -> Some hashes
  | _ -> None
;;

let fence_start line =
  let line = String.strip line in
  match String.is_empty line with
  | true -> None
  | false ->
    let character = line.[0] in
    (match character with
     | '`' | '~' ->
       let count = String.take_while line ~f:(Char.equal character) |> String.length in
       if count >= 3 then Some (character, count) else None
     | _ -> None)
;;

let fence_end (character, minimum) line =
  let line = String.strip line in
  let count = String.take_while line ~f:(Char.equal character) |> String.length in
  count >= minimum && String.is_empty (String.strip (String.drop_prefix line count))
;;

let section ~text ~heading ~include_children =
  let open Result.Let_syntax in
  let%bind depth =
    match heading_depth heading with
    | Some depth -> Ok depth
    | None -> Error ("invalid exact topic heading: " ^ heading)
  in
  let rec scan lines offset fence headings =
    match lines with
    | [] ->
      (match fence with
       | Some _ -> Error "unclosed Markdown code fence in topic source"
       | None -> Ok (List.rev headings))
    | raw :: rest ->
      let line = String.rstrip raw in
      let next = offset + String.length raw + 1 in
      (match fence with
       | Some current ->
         let fence = if fence_end current line then None else fence in
         scan rest next fence headings
       | None ->
         (match fence_start line with
          | Some _ as fence -> scan rest next fence headings
          | None ->
            let headings =
              match heading_depth line with
              | Some level -> (offset, level, line) :: headings
              | None -> headings
            in
            scan rest next None headings))
  in
  let%bind headings = scan (String.split text ~on:'\n') 0 None [] in
  match List.filter headings ~f:(fun (_, _, title) -> String.equal title heading) with
  | [] -> Error ("topic heading not found: " ^ heading)
  | _ :: _ :: _ -> Error ("ambiguous topic heading: " ^ heading)
  | [ (start, _, _) ] ->
    let finish =
      List.find_map headings ~f:(fun (offset, level, _) ->
        match offset > start && ((not include_children) || level <= depth) with
        | true -> Some offset
        | false -> None)
      |> Option.value ~default:(String.length text)
    in
    Ok (String.sub text ~pos:start ~len:(finish - start))
;;

let topic t ~id =
  match Map.find t.topics id with
  | Some topic -> Ok topic
  | None -> Error ("authoring topic is not installed: " ^ id)
;;

let closure topics roots =
  let open Result.Let_syntax in
  let completed = Hash_set.create (module String) in
  let rec visit trail reversed id =
    match Hash_set.mem completed id with
    | true -> Ok reversed
    | false ->
      (match List.mem trail id ~equal:String.equal, Map.find topics id with
       | true, _ ->
         Error
           ("authoring topic dependency cycle: "
            ^ String.concat ~sep:" -> " (List.rev (id :: trail)))
       | _, None -> Error ("authoring topic is not installed: " ^ id)
       | false, Some topic ->
         let%map reversed =
           List.fold_result
             topic.specification.prerequisites
             ~init:reversed
             ~f:(visit (id :: trail))
         in
         Hash_set.add completed id;
         topic :: reversed)
  in
  List.fold_result roots ~init:[] ~f:(visit []) |> Result.map ~f:List.rev
;;

let create ~sources specifications =
  let open Result.Let_syntax in
  let surface_ids = Authoring_sources.surface_ids sources in
  let%bind resolved =
    List.map specifications ~f:(fun specification ->
      let fail message = Error (specification.id ^ ": " ^ message) in
      let%bind () =
        match
          Metadata.valid_topic specification.id
          && (not (String.is_empty (String.strip specification.title)))
          && unique specification.prerequisites
          && List.for_all specification.prerequisites ~f:Metadata.valid_topic
          && (not (List.is_empty specification.surfaces))
          && unique specification.surfaces
          && List.for_all
               specification.surfaces
               ~f:(List.mem surface_ids ~equal:String.equal)
          && not (List.is_empty specification.excerpts)
        with
        | false -> fail "invalid topic specification"
        | true ->
          (match specification.review with
           | Pending -> Ok ()
           | Audited { evidence; _ }
             when (not (List.is_empty evidence))
                  && unique evidence
                  && List.for_all evidence ~f:(fun s ->
                    not (String.is_empty (String.strip s))) -> Ok ()
           | Audited _ -> fail "audited topic requires evidence references")
      in
      let%bind fragments =
        List.map specification.excerpts ~f:(fun source ->
          let%bind document = Authoring_sources.document sources ~path:source.path in
          let%map text =
            section
              ~text:document.text
              ~heading:source.heading
              ~include_children:source.include_children
          in
          { source; document_sha256 = document.sha256; sha256 = digest text; text })
        |> Result.all
        |> Result.map_error ~f:(fun message -> specification.id ^ ": " ^ message)
      in
      let%bind () =
        match specification.review with
        | Pending -> Ok ()
        | Audited { excerpt_sha256; _ } ->
          (match
             List.equal
               String.equal
               excerpt_sha256
               (List.map fragments ~f:(fun f -> f.sha256))
           with
           | true -> Ok ()
           | false -> fail "audited excerpt hashes changed; review the topic again")
      in
      let sha256 =
        [%sexp
          (specification : specification)
        , (List.map fragments ~f:(fun f -> f.document_sha256, f.sha256)
           : (string * string) list)]
        |> Sexp.to_string_mach
        |> digest
      in
      Ok (specification.id, { specification; fragments; sha256 }))
    |> Result.all
  in
  let%bind topics =
    match String.Map.of_alist resolved with
    | `Duplicate_key id -> Error ("duplicate authoring topic: " ^ id)
    | `Ok topics when Map.is_empty topics -> Error "empty authoring corpus"
    | `Ok topics -> Ok topics
  in
  let%bind _ = closure topics (Map.keys topics) in
  let%bind () =
    Map.data topics
    |> List.map ~f:(fun topic ->
      List.map topic.specification.prerequisites ~f:(fun id ->
        let dependency = Map.find_exn topics id in
        match
          List.for_all
            topic.specification.surfaces
            ~f:(List.mem dependency.specification.surfaces ~equal:String.equal)
        with
        | true -> Ok ()
        | false ->
          Error
            (topic.specification.id
             ^ ": prerequisite unavailable on a declared surface: "
             ^ id))
      |> Result.all_unit)
    |> Result.all_unit
  in
  let identity =
    [%sexp
      (Authoring_sources.identity sources : string)
    , (Map.to_alist (Map.map topics ~f:(fun topic -> topic.sha256))
       : (string * string) list)]
    |> Sexp.to_string_mach
    |> digest
  in
  Ok { identity; topics; surface_ids }
;;

let assemble t ~surface_id ~roots =
  let open Result.Let_syntax in
  let%bind () =
    match List.mem t.surface_ids surface_id ~equal:String.equal, roots with
    | false, _ -> Error ("unknown authoring compiler surface: " ^ surface_id)
    | _, [] -> Error "authoring topic roots must not be empty"
    | true, _ when unique roots -> Ok ()
    | _ -> Error "duplicate authoring topic root"
  in
  let%bind () =
    List.map roots ~f:(fun id ->
      let%bind requested = topic t ~id in
      match List.mem requested.specification.surfaces surface_id ~equal:String.equal with
      | true -> Ok ()
      | false -> Error ("authoring topic unavailable on " ^ surface_id ^ ": " ^ id))
    |> Result.all_unit
  in
  (* Construction already proved every dependency supports its dependent's
     surfaces. Report an incompatible requested root before traversing its graph. *)
  closure t.topics roots
;;

let pending t =
  topics t
  |> List.filter_map ~f:(fun topic ->
    match topic.specification.review with
    | Pending -> Some topic.specification.id
    | Audited _ -> None)
;;

module Coverage = struct
  type target =
    { id : string
    ; surface_id : string
    ; contract_sha256 : string
    }
  [@@deriving sexp]

  type mapping =
    { target_id : string
    ; contract_sha256 : string
    ; topic_id : string
    ; topic_closure_sha256 : string
    ; evidence : string list
    }
  [@@deriving sexp]

  type report =
    { mapped : string list
    ; missing : target list
    }
  [@@deriving sexp]

  let compiler_targets ~sources ~surface_ids =
    let open Result.Let_syntax in
    let%bind () =
      match surface_ids with
      | [] -> Error "coverage requires explicit compiler surfaces"
      | _ when not (unique surface_ids) -> Error "duplicate coverage surface"
      | _ -> Ok ()
    in
    let%map inventories =
      List.map surface_ids ~f:(fun surface_id ->
        Authoring_sources.signatures sources ~surface_id)
      |> Result.all
    in
    List.concat_map inventories ~f:(fun inventory ->
      let module I = Chatml.Chatml_surface_inventory in
      let surface_id = inventory.I.surface_id in
      let make kind name contract =
        let id = surface_id ^ "/" ^ kind ^ "/" ^ name in
        { id; surface_id; contract_sha256 = digest (id ^ "\n" ^ contract) }
      in
      List.map inventory.modules ~f:(fun name -> make "module" name name)
      @ List.map inventory.items ~f:(fun item ->
        let kind =
          match item.I.kind with
          | Global -> "global"
          | Module_export -> "module_export"
          | Type_alias -> "type_alias"
          | Entrypoint -> "entrypoint"
        in
        make
          kind
          item.name
          (Chatml.Chatml_builtin_spec.sexp_of_ty item.scheme |> Sexp.to_string_mach)))
    |> List.sort ~compare:(fun a b -> String.compare a.id b.id)
  ;;

  let topic_contract corpus ~surface_id ~topic_id =
    let open Result.Let_syntax in
    let%bind closure = assemble corpus ~surface_id ~roots:[ topic_id ] in
    let%map reviewed =
      List.map closure ~f:(fun topic ->
        match topic.specification.review with
        | Pending ->
          Error ("coverage topic has not been audited: " ^ topic.specification.id)
        | Audited _ -> Ok (topic.specification.id, topic.sha256))
      |> Result.all
    in
    [%sexp (surface_id : string), (reviewed : (string * string) list)]
    |> Sexp.to_string_mach
    |> digest
  ;;

  let audit corpus ~targets ~mappings =
    let open Result.Let_syntax in
    let%bind () =
      match targets with
      | [] -> Error "coverage requires a nonempty feature inventory"
      | _ -> Ok ()
    in
    let%bind targets_by_id =
      match
        String.Map.of_alist (List.map targets ~f:(fun target -> target.id, target))
      with
      | `Duplicate_key id -> Error ("duplicate coverage target: " ^ id)
      | `Ok targets -> Ok targets
    in
    let%bind () =
      match
        List.find_a_dup
          (List.map mappings ~f:(fun mapping -> mapping.target_id))
          ~compare:String.compare
      with
      | Some id -> Error ("duplicate coverage mapping: " ^ id)
      | None -> Ok ()
    in
    let%map mapped =
      List.map mappings ~f:(fun mapping ->
        let fail message = Error (mapping.target_id ^ ": " ^ message) in
        let%bind target =
          match Map.find targets_by_id mapping.target_id with
          | Some target -> Ok target
          | None -> fail "mapping has no inventory target"
        in
        let%bind () =
          match String.equal mapping.contract_sha256 target.contract_sha256 with
          | true -> Ok ()
          | false -> fail "compiler contract changed; review documentation coverage"
        in
        let%bind topic_closure_sha256 =
          topic_contract corpus ~surface_id:target.surface_id ~topic_id:mapping.topic_id
        in
        let%bind () =
          match String.equal mapping.topic_closure_sha256 topic_closure_sha256 with
          | true -> Ok ()
          | false -> fail "topic changed; review documentation coverage"
        in
        let%map () =
          match mapping.evidence with
          | [] -> fail "coverage requires example or behavioral test evidence"
          | references
            when List.exists references ~f:(fun reference ->
                   String.is_empty (String.strip reference)) ->
            fail "coverage contains empty evidence"
          | _ -> Ok ()
        in
        target.id)
      |> Result.all
    in
    let mapped = List.sort mapped ~compare:String.compare in
    let covered = String.Set.of_list mapped in
    { mapped
    ; missing =
        Map.data targets_by_id
        |> List.filter ~f:(fun target -> not (Set.mem covered target.id))
    }
  ;;

  let require_complete report =
    match report.missing with
    | [] -> Ok ()
    | missing ->
      Error
        ("unmapped authoring features: "
         ^ String.concat ~sep:", " (List.map missing ~f:(fun target -> target.id)))
  ;;

  let entrypoint_mappings =
    let make target_id contract_sha256 topic_id topic_closure_sha256 =
      { target_id
      ; contract_sha256
      ; topic_id
      ; topic_closure_sha256
      ; evidence =
          [ "test/agent_docs/docs_chatml_authoring.ml"
          ; "test/chatml_composition/authoring_context_tests.ml"
          ]
      }
    in
    [ make
        "one_off_v1/entrypoint/main"
        "23c3696f85a3a6c1e947b8e65c12066294eb57f9b7ea5668b23fcd6fe5d46273"
        "runtime.invocations.one-off"
        "b1bd3b4204bc034f37ebeb05afb34a2fdf7a89d34267845b9ce1a394ee057161"
    ; make
        "tool_v1/entrypoint/run"
        "776c8a90b01720d82560e9ad51ff7d507d904cc82ddae1f46b9f9388658e6a9f"
        "runtime.invocations.standalone"
        "8652b88c2cf4a91ed5974d049bd206c976ed0190d7de69c3992db8de06d21e4c"
    ; make
        "moderator_v1/entrypoint/initial_state"
        "83c4550d994027c5d2347ec8fe8b1efe366e48105a2d8563b8b17293b534807b"
        "runtime.invocations.moderator"
        "f58992cc9c62f7bfec65e3381309595e527c2e94c008f0b62a79180082230071"
    ; make
        "moderator_v1/entrypoint/on_event"
        "dae4c9cf7e731d167b4088108fa0dded53478857a436e69163fe4c8215ba5775"
        "runtime.invocations.moderator"
        "f58992cc9c62f7bfec65e3381309595e527c2e94c008f0b62a79180082230071"
    ; make
        "delegated_moderator_v1/entrypoint/initial_state"
        "c179907ea7bf651414fa4400607c3f75beb87fa748e048e10abb1e8feaff7f8f"
        "runtime.invocations.moderator"
        "4f6e6c33e84bef5b5f8a01906c9c87d51f6a80df28e40181666545c3f1dd4257"
    ; make
        "delegated_moderator_v1/entrypoint/on_event"
        "5d25a5033754052c6e715e2dffa44385a922c922c41639eb52c1e9baf235c93e"
        "runtime.invocations.moderator"
        "4f6e6c33e84bef5b5f8a01906c9c87d51f6a80df28e40181666545c3f1dd4257"
    ]
  ;;
end

let language_foundation ~sources =
  let make ?(path = "guide/chatml-ocaml-differences.md") id title prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path; heading; include_children = false })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_typechecker_test.ml"
              ; "lib/chatml/chatml_parser.mly"
              ; "lib/chatml/chatml_builtin_spec.ml"
              ]
          }
    }
  in
  create
    ~sources
    [ make
        "chatml.introduction"
        "ChatML identity and example contracts"
        []
        [ ( "# ChatML differences from OCaml"
          , "0b4293c4b5abdd44a0683f87258d7c4e26031b7ea17096a878f60e8da93a8366" )
        ]
    ; make
        "chatml.syntax.calls"
        "Explicit calls and function arity"
        [ "chatml.introduction" ]
        [ ( "## Calls have explicit arity"
          , "c55da5935d7814b63011188393163116efa9bb58d52f34f8549f2fbed6a8d723" )
        ]
    ; make
        "chatml.syntax.containers"
        "Array, record and variant syntax"
        [ "chatml.syntax.calls" ]
        [ ( "## Arrays, records and variant payloads use different delimiters"
          , "5af96134b1afc60510d2cd3f76d8387ec199117695f78b5512230dfb06cebd53" )
        ]
    ; make
        "chatml.types"
        "Type differences, matching and mutation"
        [ "chatml.syntax.containers" ]
        [ ( "## Records are structural, with conservative joins"
          , "f8da96fda8d1121048679c04ecbffa9c81409027e1a5543e390b4fdfe39c660c" )
        ; ( "## Match coverage depends on the inferred type"
          , "ee17ae31fb3f1f32964b346de33d57fca0a67e6bc3b79ca570ce837a2ba14261" )
        ; ( "## Annotate bindings; declare recursive data explicitly"
          , "ddbb952ff1bc935d15c577ddfa7923f7a81a4446ec0f4292fde091b9d41fff24" )
        ; ( "## Mutation restricts polymorphism"
          , "58b4cc03f4dadf55ce7e147868765def1ed0594798bd2fde12d86978c44115f2" )
        ]
    ; make
        "chatml.modules"
        "Module exports and qualified access"
        [ "chatml.syntax.calls" ]
        [ ( "## Modules export their own declarations"
          , "0ee9e0eb854feaeadd0cddc91187e061da19e017a7602245575f633b9be52e17" )
        ]
    ; make
        "chatml.operators"
        "ChatML operators and builtin conventions"
        [ "chatml.syntax.containers" ]
        [ ( "## Operators and builtins are ChatML's own API"
          , "a630f832c6d693026ac0c883b508ea681968459d2806394f8bdcdaeff299589e" )
        ]
    ; make
        "chatml.tasks"
        "Task composition, failures and JSON"
        [ "chatml.types"; "chatml.operators" ]
        [ ( "## Tasks are values; the host runs the returned task"
          , "3e9ec46d97c490b33b4894755df3d0addd8023e7ea95fab91c02bc6a6bfe1d04" )
        ]
    ; make
        ~path:"guide/chatml-authoring-language.md"
        "chatml.programs"
        "Writing programs: source, control flow, matching and structured data"
        [ "chatml.tasks"; "chatml.modules" ]
        [ ( "# Writing ChatML programs"
          , "6a630398a3512067a4bf364a12a27e4fcd7e66ca1b6aa3a908f1ba6ec9a6f3b2" )
        ; ( "## Source text and operators"
          , "e27196e5dfde0d4be194493d8a3c74cfc75154edfa793b25e29b7806cde3b03f" )
        ; ( "## Functions, loops and modules"
          , "0fb2be402c3218392e48d5db0e0b31ca4321ccc033c4b643df1df203a4b42290" )
        ; ( "## Matching and explicit data types"
          , "edb7f09d1277d7e1c3aed88669efebd7efaddf375eccd1b383a5aae8506b89e0" )
        ; ( "## Standard library for structured data"
          , "7096551fff9d2f77779a577d846c44e4bfecfcfa3ba8fad171e0160d3c7ad640" )
        ; ( "## Effects, errors and execution boundaries"
          , "00f02e2fb345cd50ba3f9ab75dc834f3d95e8f62895ce214faa63cd74d0d0f8a" )
        ]
    ]
;;

let runtime_foundation ~sources =
  let open Result.Let_syntax in
  let%bind language = language_foundation ~sources in
  let shared = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let managed = [ "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ] in
  let make id title surfaces prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-authoring-runtime.md"
          ; heading
          ; include_children = false
          })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_composition/one_off_tests.ml"
              ; "test/chatml_composition/standalone_tests.ml"
              ; "test/agent_server_restart_test.ml"
              ; "lib/chatml/chatml_extension_surface.mli"
              ; "lib/chat_response/moderator_invocation.mli"
              ; "lib/chat_response/managed_tool_registry.mli"
              ; "lib/chatmd_shell_spec/tool_schema.ml"
              ]
          }
    }
  in
  let runtime =
    [ make
        "runtime.invocations.contracts"
        "Execution categories and entrypoints"
        shared
        [ "chatml.introduction" ]
        [ ( "# ChatML authoring: execution and invocation contracts"
          , "cbc5f77d3ba722e09f7e77655c7a9253d1717f64159212b0ddd026c519ebbd9d" )
        ; ( "## Choose the execution contract"
          , "f5a91ea0ac03dd83274f223c1f838975d53438b6b1a80319f4bd6981b930ff2f" )
        ]
    ; make
        "chatmd.declarations.schemas"
        "Extension bindings and schema dialect"
        managed
        [ "runtime.invocations.contracts"; "runtime.authority.tool-selection" ]
        [ ( "## Bind scripts and schemas in ChatMD"
          , "7202fa9d2c52f73204d61a26ca2267e526bf84b916c78144c1839095f4e60b88" )
        ]
    ; make
        "runtime.authority.tool-selection"
        "Selected tools and target authority"
        shared
        [ "runtime.invocations.contracts" ]
        [ ( "## Authority and target surfaces"
          , "0b13a35c7e4faf79b31e02be822c931b449893521651c5860681928ad742a635" )
        ]
    ; make
        "runtime.invocations.validation"
        "Static checks versus execution admission"
        shared
        [ "runtime.authority.tool-selection" ]
        [ ( "## Non-executing validation"
          , "ec87fc05c16551292822f460a8e051525b8c8762bc4bf7ed7e37dec87d6ea2d0" )
        ]
    ; make
        "runtime.invocations.one-off"
        "One-off tool-using computations"
        [ "one_off_v1" ]
        [ "runtime.invocations.contracts"
        ; "chatml.tasks"
        ; "runtime.authority.tool-selection"
        ; "runtime.invocations.validation"
        ]
        [ ( "## One-off tool-using computations"
          , "4bcbe559d74a660b742b4d5a5479071d8d060664dc76c8b0fb329b296ec7bf57" )
        ]
    ; make
        "runtime.invocations.standalone"
        "Standalone tools and outcomes"
        [ "tool_v1" ]
        [ "runtime.invocations.contracts"
        ; "chatml.tasks"
        ; "chatmd.declarations.schemas"
        ; "runtime.invocations.validation"
        ]
        [ ( "## Standalone tools and explicit outcomes"
          , "26842dcf524875eb6a6f542406d089049512b4fd951fd2c3804a4afe56f7841c" )
        ]
    ; make
        "runtime.invocations.moderator"
        "Moderator resolution and retained state"
        [ "moderator_v1"; "delegated_moderator_v1" ]
        [ "runtime.invocations.contracts"
        ; "chatml.tasks"
        ; "chatmd.declarations.schemas"
        ; "runtime.invocations.validation"
        ]
        [ ( "## Moderator tools and session-owned state"
          , "65364456f6d94b00a5f4b0cf028ceecc08b2b4b7f31bb3b95d3ff7300edd63dd" )
        ]
    ]
  in
  let make_child id title prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces = shared
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-authoring-children.md"
          ; heading
          ; include_children = false
          })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_child_authoring.ml"
              ; "test/authoring_validation_test.ml"
              ; "test/agent_server_generated_test.ml"
              ; "test/agent_server_generated_shell_test.ml"
              ; "test/agent_server_helper_test.ml"
              ; "lib/agent_session/generated_session_request.ml"
              ; "lib/agent_session/session_management.ml"
              ; "lib/agent_session/managed_session_service.mli"
              ; "lib/agent_session/native_tool_invocation.ml"
              ; "lib/agent_session/script_tool_calls.ml"
              ; "lib/chat_response/generated_admission.ml"
              ]
          }
    }
  in
  let children =
    [ make_child
        "runtime.delegation.generated"
        "Captured child definitions and static validation"
        [ "runtime.authority.tool-selection"
        ; "runtime.invocations.validation"
        ; "chatml.tasks"
        ]
        [ ( "# ChatML authoring: persisted child sessions"
          , "928f6305d1a1276069889a94f78439cb75b1a20e1ca24e8c32ed1bdfd7fb67a5" )
        ; ( "## Capture and validate a generated definition"
          , "f9d854a4e0b53f824bb4c20197c3432fe4854dfa02a70db1e5eae5c45197af6a" )
        ]
    ; make_child
        "runtime.delegation.creation"
        "Creation retries, lifetimes and inherited authority"
        [ "runtime.delegation.generated" ]
        [ ( "## Create, retry and retain authority"
          , "1fe4d0034367286bcfbb781acf2a01eac7fdf5f72a416143cd04c60fd1033f83" )
        ]
    ; make_child
        "runtime.delegation.submissions"
        "Durable submissions and terminal receipt waits"
        [ "runtime.delegation.creation" ]
        [ ( "## Submit work and track completion"
          , "ae01ce63d8baeb96aa426655003c1ed82d864ae20583102414e28ef6952ea0b2" )
        ]
    ; make_child
        "runtime.delegation.output"
        "Output pages, fragments and cursor recovery"
        [ "runtime.delegation.submissions" ]
        [ ( "## Read output and recover cursors"
          , "9fe135857a2020bbafbe080538233609db986ca86f683b09bc1d8a04cba6c3d1" )
        ]
    ; make_child
        "runtime.delegation.stop-helper"
        "Stop receipts and shared helper authority"
        [ "runtime.delegation.output" ]
        [ ( "## Stop and use the shared helper path"
          , "df76e24e3c872f2f6ebb04dec7b2a550ead1ea92b338caa3ae2473a754250c08" )
        ]
    ]
  in
  let moderators = [ "moderator_v1"; "delegated_moderator_v1" ] in
  let make_background id title surfaces prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-authoring-background.md"
          ; heading
          ; include_children = false
          })
    ; review =
        Audited
          { excerpt_sha256 = List.map sections ~f:snd
          ; evidence =
              [ "test/agent_docs/docs_chatml_authoring.ml"
              ; "test/chatml_composition/background_shell_tests.ml"
              ; "test/chatml_composition/ingress_socket_tests.ml"
              ; "lib/chatml/chatml_extension_surface.ml"
              ; "lib/chat_response/background_job_operations.mli"
              ; "lib/chat_response/background_delivery.ml"
              ; "lib/chat_response/schedule_delivery.ml"
              ; "lib/chat_response/ingress_delivery.ml"
              ; "lib/agent_session/script_subscription_service.mli"
              ; "lib/agent_session/script_notification_service.mli"
              ; "lib/agent_session/notification_delivery.mli"
              ; "lib/agent_protocol/subscription.mli"
              ]
          }
    }
  in
  let background =
    [ make_background
        "runtime.jobs.owned"
        "Owned tool/script jobs and terminal results"
        shared
        [ "chatml.tasks"
        ; "runtime.authority.tool-selection"
        ; "runtime.recovery.background"
        ]
        [ ( "# ChatML authoring: background work and delivery"
          , "9325ab62bf13da4e28435229745a845394a656faa4c55a74f17655c20adad49d" )
        ; ( "## Start and inspect owned jobs"
          , "85e40afc8b2384462aa77386aad3ae9b38af656639c3683acf72c902fa3e5674" )
        ]
    ; make_background
        "runtime.jobs.acknowledgement"
        "Pending acknowledgements and source-owned completion events"
        moderators
        [ "runtime.jobs.owned"; "runtime.invocations.moderator" ]
        [ ( "## Acknowledge before publishing a result"
          , "8464fb4c18ff6b012ea34334723ee1e6d7b124d6fc79e440ed174ea5d26a40e5" )
        ]
    ; make_background
        "runtime.jobs.shell-example"
        "Checked shell-backed asynchronous coordinator"
        moderators
        [ "runtime.delivery.notifications" ]
        [ ( "## Shell-backed coordinator example"
          , "972d192ed9347e8fbde22ced410558f5be8972ad47b4b7e5b81af652aafefeef" )
        ]
    ; make_background
        "runtime.jobs.subscriptions"
        "Subscription lifetimes, epochs and retained terminal winners"
        moderators
        [ "runtime.jobs.acknowledgement" ]
        [ ( "## Track a workflow with subscriptions"
          , "1e05496cdb7c3f35f9c8bd403e017db94b1665f554fcb4cf0f427213efd55fd4" )
        ]
    ; make_background
        "runtime.jobs.timers"
        "One-shot timers, misfire policy and bounded polling"
        moderators
        [ "runtime.jobs.subscriptions" ]
        [ ( "## Schedule checks and choose recovery behavior"
          , "9736013396d1444d85a26a1f15a377796d57d8a60f27b15269e0c2306ccd5d69" )
        ]
    ; make_background
        "runtime.delivery.notifications"
        "Acknowledgement ordering, publication and model wake-ups"
        moderators
        [ "runtime.jobs.acknowledgement" ]
        [ ( "## Publish data and request a model turn"
          , "9cbc50caaec23ef9ac8362612502405cfeed103f48eb5c826574e84232caac3d" )
        ]
    ; make_background
        "runtime.delivery.ingress"
        "External producer registration and data delivery"
        moderators
        [ "runtime.jobs.subscriptions"; "runtime.delivery.notifications" ]
        [ ( "## Receive external completion data"
          , "6cbb2723d0f49f6db020dd620bcae79bb12390bb4937310b6fd3466a65d77eae" )
        ]
    ; make_background
        "runtime.recovery.background"
        "Staged transactions, cancellation and interrupted execution"
        shared
        [ "chatml.tasks"; "runtime.authority.tool-selection" ]
        [ ( "## Keep transaction and restart guarantees precise"
          , "8c4c7cb181f94d2fd3ed244d8b1a9f65a97bdbc449374eb4f635cb3c989115de" )
        ]
    ]
  in
  create
    ~sources
    (List.map (topics language) ~f:(fun topic -> topic.specification)
     @ runtime
     @ children
     @ background)
;;
