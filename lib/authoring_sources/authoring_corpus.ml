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

let language_foundation ~sources =
  let make id title prerequisites sections =
    { id
    ; title
    ; prerequisites
    ; surfaces = [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
    ; excerpts =
        List.map sections ~f:(fun (heading, _) ->
          { path = "guide/chatml-ocaml-differences.md"
          ; heading
          ; include_children = false
          })
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
  create
    ~sources
    (List.map (topics language) ~f:(fun topic -> topic.specification) @ runtime)
;;
