open! Core
module Corpus = Authoring_corpus
module Sources = Authoring_sources
module V = Authoring_validation
module C = Tool_capability
module Metadata = Chatmd_shell_spec.Authoring_metadata
module Schema = Chatmd_shell_spec.Tool_schema
module Digest = Chatmd_shell_spec.Source_ref

type t =
  { corpus : Corpus.t
  ; sources : Sources.t
  ; secret : string
  ; default_tokens : int
  ; max_tokens : int
  ; fingerprint : string
  }

type reference_part =
  { index : int
  ; item_sha256 : string
  }

type reference_topic =
  { topic : Agent_protocol.Authoring_guidance.topic
  ; total_parts : int
  ; parts : reference_part list
  }

type reference_receipt =
  { query_identity : string
  ; host_identity : string
  ; capability_fingerprint : string
  ; scope : string
  ; surface_id : string
  ; corpus_identity : string
  ; response_sha256 : string
  ; topics : reference_topic list
  }

type response =
  { json : Jsonaf.t
  ; receipt : reference_receipt option
  }

let fingerprint t = t.fingerprint
let installed_corpus t = t.corpus
let corpus_for_host t ~host = Option.value (V.corpus host) ~default:t.corpus

let query_fingerprint corpus ~default_tokens ~max_tokens =
  [%sexp
    ("ochat.authoring-query.v3" : string)
  , (Corpus.identity corpus : string)
  , (default_tokens : int)
  , (max_tokens : int)]
  |> Sexp.to_string
  |> Digest.digest
;;

let with_host_budget t ~host =
  match V.configured_context_budget host with
  | None -> t
  | Some budget ->
    { t with
      default_tokens = budget.default_tokens
    ; max_tokens = budget.max_tokens
    ; fingerprint =
        query_fingerprint
          t.corpus
          ~default_tokens:budget.default_tokens
          ~max_tokens:budget.max_tokens
    }
;;

let create
      ?(default_tokens = V.default_context_budget.default_tokens)
      ?(max_tokens = V.default_context_budget.max_tokens)
      ?(authored_packages = [])
      ?authored_max_bytes
      ~secret
      ()
  =
  let open Result.Let_syntax in
  let%bind () =
    match
      String.length secret >= 16
      && default_tokens > 0
      && default_tokens <= max_tokens
      && max_tokens <= 1_000_000
    with
    | true -> Ok ()
    | false -> Error "invalid authoring query key or budgets"
  in
  let%bind sources = Sources.installed () in
  let%bind corpus = Corpus.runtime_foundation ~sources in
  let%bind corpus =
    Corpus.extend_authored ?max_bytes:authored_max_bytes corpus authored_packages
  in
  (* Require the maintained inventories to match the installed implementation
     before serving guidance. They do not automatically discover every public
     feature; packages remain incomplete until the full authoring audit. *)
  let%bind () =
    let module Coverage = Corpus.Coverage in
    List.map
      [ Coverage.compiler_targets, Coverage.reviewed_mappings
      ; Coverage.grammar_targets, Coverage.grammar_mappings
      ; Coverage.semantic_targets, Coverage.semantic_mappings
      ]
      ~f:(fun (inventory, mappings) ->
        let%bind targets =
          inventory
            ~sources
            ~surface_ids:
              [ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
        in
        let%bind report = Coverage.audit corpus ~targets ~mappings in
        Coverage.require_complete report)
    |> Result.all_unit
  in
  let fingerprint = query_fingerprint corpus ~default_tokens ~max_tokens in
  Ok { corpus; sources; secret; default_tokens; max_tokens; fingerprint }
;;

let scoped_corpus ?host t ~capabilities =
  let t =
    match host with
    | None -> t
    | Some host -> { t with corpus = corpus_for_host t ~host }
  in
  let installed = Corpus.authored_packages t.corpus in
  let packages =
    C.references capabilities
    |> List.filter_map ~f:(fun reference ->
      match C.find capabilities ~name:reference.name with
      | Error _ -> None
      | Ok binding ->
        Option.bind (C.metadata binding).authoring ~f:(fun help ->
          Option.some_if
            (List.exists installed ~f:(fun package ->
               String.equal help.package package.Metadata.package))
            help.package))
    |> List.dedup_and_sort ~compare:String.compare
  in
  Corpus.scope_authored t.corpus ~packages
  |> Result.map_error ~f:(fun _ ->
    "selected authored packages have an unavailable dependency")
;;

let authored_roots corpus ~capabilities ~task =
  let installed = Corpus.authored_packages corpus in
  C.references capabilities
  |> List.concat_map ~f:(fun reference ->
    match C.find capabilities ~name:reference.name with
    | Error _ -> []
    | Ok binding ->
      (match (C.metadata binding).authoring with
       | Some help
         when List.mem help.tasks task ~equal:Metadata.equal_task
              && List.exists installed ~f:(fun package ->
                String.equal help.package package.Metadata.package) -> help.topics
       | _ -> []))
  |> List.dedup_and_sort ~compare:String.compare
;;

let tasks =
  [ Metadata.One_off_script
  ; Standalone_tool
  ; Moderator_tool
  ; Child_agent
  ; Background_workflow
  ]
;;

let features =
  [ "background_tools", "runtime.jobs.owned"
  ; "subscriptions", "runtime.jobs.subscriptions"
  ; "timers", "runtime.jobs.timers"
  ; "notifications", "runtime.delivery.notifications"
  ; "external_events", "runtime.delivery.ingress"
  ; "child_sessions", "runtime.delegation.stop-helper"
  ]
;;

let enum values =
  `Object
    [ "type", `String "string"; "enum", `Array (List.map values ~f:(fun s -> `String s)) ]
;;

let nullable type_ properties =
  `Object (("type", `Array [ `String type_; `String "null" ]) :: properties)
;;

let nullable_string maximum =
  nullable
    "string"
    [ "minLength", `Number "1"; "maxLength", `Number (Int.to_string maximum) ]
;;

let parameters =
  let properties =
    [ "version", `Object [ "type", `String "integer"; "enum", `Array [ `Number "1" ] ]
    ; "operation", enum [ "search"; "topic"; "prepare"; "continue" ]
    ; ( "task"
      , nullable
          "string"
          [ ( "enum"
            , `Array
                (`Null :: List.map tasks ~f:(fun task -> `String (Metadata.task_id task)))
            )
          ] )
    ; "query", nullable_string 512
    ; "topic_id", nullable_string 256
    ; ( "features"
      , nullable
          "array"
          [ "items", enum (List.map features ~f:fst); "maxItems", `Number "6" ] )
    ; "cursor", nullable_string 12288
    ; ( "max_tokens"
      , nullable "integer" [ "minimum", `Number "1"; "maximum", `Number "1000000" ] )
    ]
  in
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map properties ~f:(fun (name, _) -> `String name))
    ; "additionalProperties", `False
    ]
;;

let schema =
  Schema.compile parameters
  |> Result.map_error ~f:(fun _ -> "invalid authoring query schema")
  |> Result.ok_or_failwith
;;

let fields = function
  | `Object fields -> fields
  | _ -> []
;;

let field json name = List.Assoc.find (fields json) name ~equal:String.equal

let text json name =
  match field json name with
  | Some (`String s) -> s
  | _ -> ""
;;

let integer n = `Number (Int.to_string n)
let strings values = `Array (List.map values ~f:(fun s -> `String s))

let validate_operation request =
  let required, unused =
    match text request "operation" with
    | "search" -> [ "task"; "query" ], [ "topic_id"; "features"; "cursor" ]
    | "topic" -> [ "task"; "topic_id" ], [ "query"; "features"; "cursor" ]
    | "prepare" -> [ "task" ], [ "query"; "topic_id"; "cursor" ]
    | "continue" -> [ "cursor" ], [ "task"; "query"; "topic_id"; "features" ]
    | _ -> [], []
  in
  let null name =
    match field request name with
    | Some `Null -> true
    | _ -> false
  in
  match List.exists required ~f:null, List.for_all unused ~f:null with
  | false, true -> Ok ()
  | _ -> Error "provide the operation's required values and set unused fields to null"
;;

let task_of_request request =
  List.find tasks ~f:(fun task ->
    String.equal (Metadata.task_id task) (text request "task"))
  |> Result.of_option ~error:"unknown authoring task"
;;

let surface = V.task_surface
let task_surface = surface

let roots task request =
  match text request "operation" with
  | "topic" -> Ok [ text request "topic_id" ]
  | "prepare" ->
    let base =
      match task with
      | Metadata.One_off_script -> [ "runtime.invocations.one-off" ]
      | Standalone_tool -> [ "chatmd.definitions"; "runtime.invocations.standalone" ]
      | Moderator_tool -> [ "chatmd.definitions"; "runtime.invocations.moderator" ]
      | Child_agent -> [ "chatmd.definitions"; "runtime.delegation.stop-helper" ]
      | Background_workflow ->
        [ "chatmd.definitions"; "runtime.jobs.timers"; "runtime.delivery.notifications" ]
    in
    let requested =
      match field request "features" with
      | Some (`Array xs) -> xs
      | _ -> []
    in
    let extra =
      List.filter_map requested ~f:(function
        | `String name -> List.Assoc.find features name ~equal:String.equal
        | _ -> None)
    in
    Ok
      (List.dedup_and_sort
         (("chatml.evaluation"
           :: "chatml.inference"
           :: "chatml.programs"
           :: "chatml.task-effects"
           :: base)
          @ extra)
         ~compare:String.compare)
  | _ -> Error "operation has no topic roots"
;;

(* A flat orientation teaches why a feature matters before asking the author to
   retrieve its contract. Compiler/reference availability is not an effect grant. *)
let orientation corpus ~host ~capabilities ~surface_id =
  let guide title purpose when_to_read task topic_ids =
    let readable_here =
      List.for_all topic_ids ~f:(fun id ->
        match Corpus.topic corpus ~id with
        | Error _ -> false
        | Ok topic -> List.mem topic.specification.surfaces surface_id ~equal:String.equal)
    in
    `Object
      [ "title", `String title
      ; "enables", `String purpose
      ; "read_before", `String when_to_read
      ; "topic_ids", strings topic_ids
      ; "suggested_task", `String (Metadata.task_id task)
      ; ("readable_on_selected_surface", if readable_here then `True else `False)
      ; ( "suggested_task_enabled"
        , if
            Result.is_ok (surface host task)
            && Option.is_none (V.execution_unavailable_reason host task)
          then `True
          else `False )
      ; ( "execution_unavailable_reason"
        , match V.execution_unavailable_reason host task with
          | None -> `Null
          | Some reason -> `String reason )
      ]
  in
  let guides =
    [ guide
        "ChatML language"
        "Express deterministic logic with functions, arrays, structural records, \
         variants, pattern matching and modules. Compose tool effects with tasks and \
         let*."
        "Writing ChatML: OCaml familiarity helps, but call syntax, containers, \
         inference, operators and task execution differ."
        Metadata.One_off_script
        [ "chatml.programs"
        ; "chatml.inference"
        ; "chatml.evaluation"
        ; "chatml.task-effects"
        ]
    ; guide
        "One-off scripts"
        "Combine selected tools with filtering, transformation and branching in one \
         computation, without creating a conversational agent."
        "Choosing a script entrypoint or assuming that constructing a task executes it."
        One_off_script
        [ "runtime.invocations.one-off" ]
    ; guide
        "ChatMD and reusable tools"
        "Bind a script and strict input/output schemas as a reusable tool that an agent \
         can call."
        "Defining an agent's extension declarations, handler contract or tool outcomes."
        Standalone_tool
        [ "chatmd.definitions"; "runtime.invocations.standalone" ]
    ; guide
        "Stateful moderation and custom tools"
        "Handle session events and moderator-owned tool calls using retained state; \
         coordinate agent behavior and custom completion logic."
        "Implementing a ghost/moderator tool or changing what happens during an agent \
         conversation."
        Moderator_tool
        [ "runtime.invocations.moderator" ]
    ; guide
        "Background jobs and asynchronous results"
        "Start owned tool or script work, acknowledge it as pending, inspect completion \
         and deliver results later. A shell tool can supply background process work when \
         delegated."
        "Starting long-running work or returning before its result is ready: ownership \
         and acknowledgement ordering determine valid delivery."
        Background_workflow
        [ "runtime.jobs.acknowledgement"; "runtime.jobs.shell-example" ]
    ; guide
        "Subscriptions and timers"
        "Track a workflow across events, timeouts and polling attempts; retain a \
         terminal winner and reject stale callbacks."
        "Polling a child session, racing completion against a deadline, or deciding what \
         a missed timer means after restart."
        Background_workflow
        [ "runtime.jobs.timers" ]
    ; guide
        "Notifications and external events"
        "Publish completed data, optionally request an agent turn, and accept \
         authenticated completion data from external producers."
        "Waking an agent or integrating a watcher/service: data delivery, wake admission \
         and producer registration are separate contracts."
        Background_workflow
        [ "runtime.delivery.notifications"; "runtime.delivery.ingress" ]
    ; guide
        "Persisted child agents"
        "Create a child with its own instructions, model and moderation; use its session \
         reference to send work, inspect status, wait, read outputs and stop it."
        "Delegating an ongoing task: children inherit authority ceilings, and retries, \
         output cursors and session lifetime have explicit rules."
        Child_agent
        [ "runtime.delegation.stop-helper" ]
    ; guide
        "Authority and validation"
        "Check generated source without running it and understand which tools, shell \
         operations and files can be delegated."
        "Executing generated code or spawning a child: reading documentation and passing \
         static checks never grant execution authority."
        One_off_script
        [ "runtime.invocations.validation" ]
    ; guide
        "Transactions and recovery"
        "Design workflows around committed records, cancellation and restart behavior \
         without assuming external effects run exactly once."
        "Retrying work, cancelling existing jobs, or relying on a task or moderator \
         state to survive a process restart."
        Background_workflow
        [ "runtime.recovery.background" ]
    ]
  in
  let tools =
    C.references capabilities
    |> List.map ~f:(fun reference ->
      let description =
        match C.find capabilities ~name:reference.name with
        | Error _ -> `Null
        | Ok binding ->
          (match
             Authoring_tool_description.describe
               ~host
               ~capabilities
               ~name:reference.name
               ~description:(C.descriptor binding).function_.description
           with
           | None -> `Null
           | Some description -> `String description)
      in
      `Object [ "name", `String reference.name; "description", description ])
  in
  `Object
    [ ( "reading_strategy"
      , `String
          "Start by choosing the features useful for your task from this flat map. \
           Before implementing an unfamiliar feature, fetch its topic_ids with \
           operation=topic using a compatible task, or request it through prepare's \
           features. Topic retrieval includes prerequisites automatically; search finds \
           specific APIs. Read the returned contracts and examples, validate the \
           complete definition, then execute. The map is orientation, not a replacement \
           for those contracts." )
    ; ( "availability"
      , `String
          "Enabled authoring tasks reflect compiler targets and known execution \
           limitations; readable topics describe compiler/reference compatibility. \
           Neither proves a runtime effect service is installed. selected_tools lists \
           only the invoking scope's bindings; execution still checks current \
           permissions, tool selection and host services." )
    ; ( "enabled_authoring_tasks"
      , strings
          (List.filter_map tasks ~f:(fun task ->
             match surface host task with
             | Ok _ when Option.is_none (V.execution_unavailable_reason host task) ->
               Some (Metadata.task_id task)
             | Ok _ -> None
             | Error _ -> None)) )
    ; "selected_tools", `Array tools
    ; "reference_topics", strings [ "reference.signatures"; "reference.tools" ]
    ; "guides", `Array guides
    ]
;;

let signature_items t ~surface_id =
  let module Inventory = Chatml.Chatml_surface_inventory in
  let open Result.Let_syntax in
  let%map inventory = Sources.signatures t.sources ~surface_id in
  let source_identity =
    Inventory.to_json inventory |> Jsonaf.to_string |> Digest.digest
  in
  let groups =
    Inventory.reference_items inventory
    |> List.fold ~init:String.Map.empty ~f:(fun groups item ->
      let group =
        match text item "kind" with
        | "entrypoint" -> "0:entrypoints"
        | "type_alias" -> "1:type aliases"
        | "global" -> "2:globals"
        | _ ->
          let name = text item "name" in
          let module_name =
            String.lsplit2 name ~on:'.' |> Option.value_map ~default:name ~f:fst
          in
          "3:" ^ module_name
      in
      Map.add_multi groups ~key:group ~data:item)
    |> Map.to_alist
  in
  `Object
    [ "kind", `String "signature_legend"
    ; "topic_id", `String "reference.signatures"
    ; "surface", `String surface_id
    ; "source_sha256", `String source_identity
    ; "notation", `String Inventory.reference_notation
    ]
  :: List.map groups ~f:(fun (group, reversed) ->
    `Object
      [ "kind", `String "compiler_signatures"
      ; "topic_id", `String "reference.signatures"
      ; "surface", `String surface_id
      ; "source_sha256", `String source_identity
      ; "group", `String (String.drop_prefix group 2)
      ; "declarations", `Array (List.rev reversed)
      ])
;;

let tool_items ~host capabilities =
  let open Result.Let_syntax in
  C.references capabilities
  |> List.map ~f:(fun reference ->
    let%map binding =
      C.find capabilities ~name:reference.name
      |> Result.map_error ~f:(fun error -> error.C.message)
    in
    let descriptor = (C.descriptor binding).function_ in
    `Object
      [ "kind", `String "selected_tool"
      ; "topic_id", `String "reference.tools"
      ; "name", `String reference.name
      ; ( "description"
        , match
            Authoring_tool_description.describe
              ~host
              ~capabilities
              ~name:reference.name
              ~description:descriptor.description
          with
          | None -> `Null
          | Some text -> `String text )
      ; "input_schema", descriptor.parameters
      ; ("strict", if descriptor.strict then `True else `False)
      ; "binding_fingerprint", `String reference.fingerprint
      ; ( "result_contract"
        , `String
            (match C.result_contract binding with
             | Native_output -> "native_output"
             | Invocation_v1 -> "invocation_v1") )
      ])
  |> Result.all
;;

let strip_metadata line = String.is_prefix line ~prefix:"<!-- ochat-authoring-example: "

(* Maintained corpus fences are top-level. Keep whole fenced examples together,
   replacing verification metadata with a visible contract/rejection label. *)
let blocks source =
  let flush current blocks =
    match current with
    | [] -> blocks
    | _ -> String.concat ~sep:"\n" (List.rev current) :: blocks
  in
  let rec loop current accumulated fence = function
    | [] -> List.rev (flush current accumulated)
    | line :: rest ->
      (match fence with
       | Some delimiter ->
         let current = line :: current in
         if String.equal (String.strip line) delimiter
         then loop [] (flush current accumulated) None rest
         else loop current accumulated fence rest
       | None when strip_metadata line ->
         let metadata =
           String.chop_prefix_exn line ~prefix:"<!-- ochat-authoring-example: "
           |> fun s -> String.chop_suffix_exn s ~suffix:" -->" |> Jsonaf.of_string
         in
         let label =
           "Example "
           ^ text metadata "id"
           ^ " ("
           ^ text metadata "surface"
           ^ ")"
           ^
           match field metadata "stage" with
           | Some (`String stage) -> "; expected " ^ stage ^ " rejection"
           | _ -> ""
         in
         loop [ label ] (flush current accumulated) None rest
       | None
         when String.is_prefix line ~prefix:"```" || String.is_prefix line ~prefix:"~~~"
         ->
         let delimiter = String.prefix line 3 in
         loop (line :: current) accumulated (Some delimiter) rest
       | None when String.is_empty (String.strip line) ->
         loop [] (flush current accumulated) None rest
       | None -> loop (line :: current) accumulated None rest)
  in
  loop [] [] None (String.split_lines source)
;;

let origin_fields (topic : Corpus.topic) =
  match topic.origin with
  | Installed -> [ "source_kind", `String "installed" ]
  | Authored { package; package_sha256 } ->
    [ "source_kind", `String "authored_conventions"
    ; "author_package", `String package
    ; "author_package_sha256", `String package_sha256
    ; ( "authority"
      , `String
          "Author-supplied conventions; not authoritative compiler or runtime semantics."
      )
    ]
;;

let topic_items topic =
  List.mapi topic.Corpus.fragments ~f:(fun part fragment ->
    let text =
      match topic.origin with
      | Installed -> blocks fragment.text |> String.concat ~sep:"\n\n"
      | Authored _ -> fragment.text
    in
    `Object
      ([ "topic_id", `String topic.specification.id
       ; "title", `String topic.specification.title
       ; "topic_sha256", `String topic.sha256
       ; "source", `String fragment.source.path
       ; "source_sha256", `String fragment.document_sha256
       ; "section", `String fragment.source.heading
       ; "part", integer part
       ; "content_sha256", `String (Digest.digest text)
       ; "text", `String text
       ]
       @ origin_fields topic))
;;

let utf8_prefix text max_bytes =
  let rec take length =
    let result = String.prefix text length in
    match Stdlib.String.is_valid_utf_8 result with
    | true -> result
    | false -> take (length - 1)
  in
  take (Int.min (String.length text) max_bytes)
;;

let search_terms query =
  String.lowercase query
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n'; '`'; '('; ')'; ',' ]
  |> List.filter ~f:(fun term ->
    not
      (List.mem
         [ ""; "and"; "or"; "the"; "a"; "an"; "to"; "for"; "with"; "how" ]
         term
         ~equal:String.equal))
  |> List.dedup_and_sort ~compare:String.compare
;;

let search corpus ~surface_id query =
  let query = String.lowercase (String.strip query) in
  let terms = search_terms query in
  Corpus.topics corpus
  |> List.filter_map ~f:(fun topic ->
    match List.mem topic.specification.surfaces surface_id ~equal:String.equal with
    | false -> None
    | true ->
      let id = String.lowercase topic.specification.id in
      let title = String.lowercase topic.specification.title in
      let body =
        List.map topic.fragments ~f:(fun f -> f.text) |> String.concat ~sep:"\n"
      in
      let lower = String.lowercase body in
      let score =
        List.sum
          (module Int)
          terms
          ~f:(fun term ->
            let contains s = String.is_substring s ~substring:term in
            (if contains id then 30 else 0)
            + (if contains title then 20 else 0)
            + if contains lower then 1 else 0)
        + if String.equal query id then 1000 else 0
      in
      (match score > 0 with
       | false -> None
       | true ->
         let excerpt =
           blocks body
           |> List.find ~f:(fun block ->
             List.exists terms ~f:(fun term ->
               String.is_substring (String.lowercase block) ~substring:term))
           |> Option.value ~default:topic.specification.title
           |> fun text -> utf8_prefix text 240
         in
         Some
           ( score
           , topic.specification.id
           , `Object
               ([ "topic_id", `String topic.specification.id
                ; "title", `String topic.specification.title
                ; "excerpt", `String excerpt
                ; "topic_sha256", `String topic.sha256
                ; "prerequisites", strings topic.specification.prerequisites
                ]
                @ origin_fields topic) )))
  |> List.sort ~compare:(fun (a, aid, _) (b, bid, _) ->
    match Int.compare b a with
    | 0 -> String.compare aid bid
    | other -> other)
  |> List.map ~f:(fun (_, _, item) -> item)
;;

let reference_search t ~host ~capabilities ~surface_id query =
  let open Result.Let_syntax in
  let terms = search_terms query in
  let matches value =
    let value = String.lowercase value in
    List.exists terms ~f:(fun term -> String.is_substring value ~substring:term)
  in
  let result id title declarations =
    let selected =
      List.filter declarations ~f:(fun (name, description) ->
        matches name || matches description)
    in
    match selected, String.equal (String.lowercase (String.strip query)) id with
    | [], false -> None
    | _ ->
      Some
        (`Object
            [ "topic_id", `String id
            ; "title", `String title
            ; ( "excerpt"
              , `String
                  (List.take selected 3
                   |> List.map ~f:(fun (name, description) -> name ^ ": " ^ description)
                   |> String.concat ~sep:"\n"
                   |> fun text -> utf8_prefix text 600) )
            ; "matching_symbols", strings (List.take selected 8 |> List.map ~f:fst)
            ; "matching_count", integer (List.length selected)
            ; "prerequisites", `Array []
            ])
  in
  let%bind inventory = Sources.signatures t.sources ~surface_id in
  let signatures =
    Chatml.Chatml_surface_inventory.reference_items inventory
    |> List.map ~f:(fun item -> text item "name", text item "signature")
  in
  let%map tools = tool_items ~host capabilities in
  List.filter_opt
    [ result
        "reference.signatures"
        "Compiler signatures for the selected target"
        signatures
    ; result
        "reference.tools"
        "Selected tool schemas and calling contracts"
        (List.map tools ~f:(fun item -> text item "name", text item "description"))
    ]
;;

let sign t payload = Digestif.SHA256.(hmac_string ~key:t.secret payload |> to_hex)

let cursor t ~context ~request ~offset =
  let payload =
    `Object [ "context", `String context; "request", request; "offset", integer offset ]
    |> Jsonaf.to_string
  in
  Base64.encode_exn payload ^ "." ^ sign t payload
;;

let resume t ~context encoded =
  let open Result.Let_syntax in
  let%bind payload, signature =
    String.rsplit2 encoded ~on:'.'
    |> Result.of_option ~error:"invalid continuation cursor"
  in
  let%bind payload =
    Base64.decode payload |> Result.map_error ~f:(fun _ -> "invalid continuation cursor")
  in
  let%bind () =
    match String.equal signature (sign t payload) with
    | true -> Ok ()
    | false -> Error "invalid continuation cursor"
  in
  let%bind json =
    Result.try_with (fun () -> Jsonaf.of_string payload)
    |> Result.map_error ~f:(fun _ -> "invalid continuation cursor")
  in
  let%bind () =
    match String.equal (text json "context") context with
    | true -> Ok ()
    | false -> Error "continuation context changed; repeat the original query"
  in
  match field json "request", field json "offset" with
  | Some request, Some (`Number number) ->
    (match Int.of_string_opt number with
     | Some offset when offset >= 0 -> Ok (request, offset)
     | _ -> Error "invalid continuation offset")
  | _ -> Error "invalid continuation cursor"
;;

let reference_topics ~corpus ~installation_identity items ~offset ~count =
  let groups =
    List.mapi items ~f:(fun position item -> position, item)
    |> List.fold ~init:String.Map.empty ~f:(fun groups (position, item) ->
      match text item "topic_id" with
      | "" -> groups
      | id -> Map.add_multi groups ~key:id ~data:(position, item))
  in
  Map.to_alist groups
  |> List.filter_map ~f:(fun (id, reversed) ->
    let all = List.rev reversed in
    let parts =
      List.filter_mapi all ~f:(fun index (position, item) ->
        match position >= offset && position < offset + count with
        | false -> None
        | true -> Some { index; item_sha256 = Digest.digest (Jsonaf.to_string item) })
    in
    match parts with
    | [] -> None
    | _ ->
      let metadata =
        match id with
        | "reference.tools" | "reference.signatures" ->
          Some
            ( Digest.digest (Jsonaf.to_string (`Array (List.map all ~f:snd)))
            , Agent_protocol.Authoring_guidance.Installed installation_identity )
        | _ ->
          (match Corpus.topic corpus ~id with
           | Error _ -> None
           | Ok topic ->
             Some
               ( topic.sha256
               , match topic.origin with
                 | Installed ->
                   Agent_protocol.Authoring_guidance.Installed installation_identity
                 | Authored owner -> Authored owner.package_sha256 ))
      in
      Option.map metadata ~f:(fun (document_sha256, source) ->
        let total_parts = List.length all in
        { topic =
            { id; document_sha256; source; complete = List.length parts = total_parts }
        ; total_parts
        ; parts
        }))
;;

let matches_response receipt json =
  String.equal receipt.response_sha256 (Digest.digest (Jsonaf.to_string json))
;;

let virtual_topics t ~host ~capabilities ~task =
  let open Result.Let_syntax in
  let%bind surface_id = task_surface host task in
  let%bind signatures = signature_items t ~surface_id in
  let%map tools = tool_items ~host capabilities in
  let corpus = corpus_for_host t ~host in
  let items = signatures @ tools in
  reference_topics
    ~corpus
    ~installation_identity:(Corpus.identity corpus)
    items
    ~offset:0
    ~count:(List.length items)
  |> List.map ~f:(fun reference -> { reference.topic with complete = false })
;;

let reference_to_protocol (receipt : reference_receipt) =
  let module R = Agent_protocol.Authoring_reference in
  R.create
    ~query_identity:receipt.query_identity
    ~host_identity:receipt.host_identity
    ~capability_fingerprint:receipt.capability_fingerprint
    ~scope:receipt.scope
    ~surface_id:receipt.surface_id
    ~corpus_identity:receipt.corpus_identity
    ~response_sha256:receipt.response_sha256
    ~topics:
      (List.map receipt.topics ~f:(fun topic ->
         R.
           { topic = topic.topic
           ; total_parts = topic.total_parts
           ; parts =
               List.map topic.parts ~f:(fun part ->
                 { index = part.index; item_sha256 = part.item_sha256 })
           }))
;;

let query_with_receipt t ~host ~capabilities ~scope request =
  let t = with_host_budget t ~host in
  let run () =
    let open Result.Let_syntax in
    let%bind () =
      match String.length (Jsonaf.to_string request) <= 16384 with
      | true ->
        Schema.validate schema request
        |> Result.map_error ~f:(fun _ -> "invalid version-1 documentation request")
      | false -> Error "documentation request exceeds 16 KiB"
    in
    let%bind () = validate_operation request in
    (* Guidance source identity names the host installation, as in materialized
       primer/preload receipts. The response's corpus identity names the scoped
       view; withdrawing authored packages must not relabel installed sources. *)
    let installation_identity = Corpus.identity (corpus_for_host t ~host) in
    let%bind corpus = scoped_corpus ~host t ~capabilities in
    let t = { t with corpus } in
    let%bind requested_budget =
      match field request "max_tokens" with
      | Some (`Number n) ->
        Int.of_string_opt n
        |> Result.of_option ~error:"max_tokens must use decimal integer notation"
      | _ -> Ok t.default_tokens
    in
    let%bind () =
      match requested_budget <= t.max_tokens with
      | true -> Ok ()
      | false -> Error ("max_tokens exceeds host ceiling " ^ Int.to_string t.max_tokens)
    in
    let context =
      [%sexp
        (t.fingerprint : string)
      , (V.host_fingerprint host : string)
      , (C.fingerprint capabilities : string)
      , (scope : string)]
      |> Sexp.to_string
      |> Digest.digest
    in
    let%bind base, offset =
      match text request "operation" with
      | "continue" -> resume t ~context (text request "cursor")
      | _ ->
        Ok
          ( `Object
              (List.filter (fields request) ~f:(fun (key, _) ->
                 not (String.equal key "max_tokens")))
          , 0 )
    in
    let%bind task = task_of_request base in
    let%bind surface_id = surface host task in
    let operation = text base "operation" in
    let%bind () =
      match operation with
      | "prepare" ->
        let child_feature =
          match field base "features" with
          | Some (`Array values) ->
            List.exists values ~f:(function
              | `String "child_sessions" -> true
              | _ -> false)
          | _ -> false
        in
        let checked_task = if child_feature then Metadata.Child_agent else task in
        (match V.execution_unavailable_reason host checked_task with
         | None -> Ok ()
         | Some reason -> Error reason)
      | _ -> Ok ()
    in
    let%bind items, covered =
      match operation, text base "topic_id" with
      | "topic", "reference.signatures" ->
        let%map items = signature_items t ~surface_id in
        items, [ "reference.signatures" ]
      | "topic", "reference.tools" ->
        let%map items = tool_items ~host capabilities in
        items, [ "reference.tools" ]
      | "search", _ ->
        let query = text base "query" in
        let%map references = reference_search t ~host ~capabilities ~surface_id query in
        search t.corpus ~surface_id query @ references, []
      | ("topic" | "prepare"), _ ->
        let%bind roots = roots task base in
        let roots =
          match operation with
          | "prepare" ->
            roots @ authored_roots t.corpus ~capabilities ~task
            |> List.dedup_and_sort ~compare:String.compare
          | _ -> roots
        in
        let%map topics = Corpus.assemble t.corpus ~surface_id ~roots in
        ( List.concat_map topics ~f:topic_items
        , List.map topics ~f:(fun topic -> topic.specification.id) )
      | _ -> Error "invalid continuation operation"
    in
    let%bind items, covered =
      match operation with
      | "prepare" ->
        let%bind tools = tool_items ~host capabilities in
        let%map signatures = signature_items t ~surface_id in
        ( `Object
            [ "kind", `String "orientation"
            ; "content", orientation t.corpus ~host ~capabilities ~surface_id
            ]
          :: (items @ tools @ signatures)
        , covered @ [ "reference.tools"; "reference.signatures" ] )
      | _ -> Ok (items, covered)
    in
    let remaining = List.drop items offset in
    let response page count minimum =
      let next_offset = offset + count in
      let complete = next_offset >= List.length items in
      let body estimate =
        `Object
          [ "version", integer 1
          ; "operation", `String operation
          ; "task", `String (Metadata.task_id task)
          ; "surface", `String surface_id
          ; "runtime_identity", `String (V.runtime_identity host)
          ; "corpus_identity", `String (Corpus.identity t.corpus)
          ; "capability_fingerprint", `String (C.fingerprint capabilities)
          ; "coverage", `String "reviewed_foundation_not_full_feature_coverage"
          ; ( "package_complete"
            , if String.equal operation "prepare" then `False else `Null )
          ; "topic_sequence", strings covered
          ; "items", `Array page
          ; ("complete", if complete then `True else `False)
          ; ( "next_cursor"
            , if complete
              then `Null
              else `String (cursor t ~context ~request:base ~offset:next_offset) )
          ; ( "budget"
            , `Object
                [ "max_tokens", integer requested_budget
                ; "token_estimate", integer estimate
                ; "method", `String "utf8_bytes_div_3_estimate"
                ; ( "minimum_next_tokens"
                  , match minimum with
                    | None -> `Null
                    | Some n -> integer n )
                ] )
          ]
      in
      let rec measured estimate =
        let json = body estimate in
        let actual = (String.length (Jsonaf.to_string json) + 2) / 3 in
        match actual = estimate with
        | true -> json, actual
        | false -> measured actual
      in
      measured 0
    in
    let rec pack reversed count = function
      | [] ->
        let page, size = response (List.rev reversed) count None in
        (match size <= requested_budget with
         | true -> Ok (page, count)
         | false ->
           Error
             ("budget too small for response metadata; request at least "
              ^ Int.to_string size))
      | next :: rest ->
        let _, size = response (List.rev (next :: reversed)) (count + 1) None in
        (match size <= requested_budget with
         | true -> pack (next :: reversed) (count + 1) rest
         | false ->
           let minimum =
             match count with
             | 0 -> Some size
             | _ -> None
           in
           let page, page_size = response (List.rev reversed) count minimum in
           (match page_size <= requested_budget with
            | true -> Ok (page, count)
            | false ->
              Error
                ("budget too small for response metadata; request at least "
                 ^ Int.to_string page_size)))
    in
    let%map json, count = pack [] 0 remaining in
    let topics =
      match operation with
      | "search" -> []
      | _ -> reference_topics ~corpus:t.corpus ~installation_identity items ~offset ~count
    in
    let receipt =
      match topics with
      | [] -> None
      | _ ->
        Some
          { query_identity = t.fingerprint
          ; host_identity = V.host_fingerprint host
          ; capability_fingerprint = C.fingerprint capabilities
          ; scope
          ; surface_id
          ; corpus_identity = Corpus.identity t.corpus
          ; response_sha256 = Digest.digest (Jsonaf.to_string json)
          ; topics
          }
    in
    { json; receipt }
  in
  match run () with
  | Ok response -> response
  | Error message ->
    { json =
        `Object
          [ "version", integer 1
          ; "error", `String "authoring.query_rejected"
          ; "message", `String message
          ]
    ; receipt = None
    }
;;

let query t ~host ~capabilities ~scope request =
  (query_with_receipt t ~host ~capabilities ~scope request).json
;;
