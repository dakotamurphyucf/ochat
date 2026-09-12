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

let fingerprint t = t.fingerprint
let installed_corpus t = t.corpus

let create ?(default_tokens = 12000) ?(max_tokens = 32000) ~secret () =
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
  let%bind targets =
    Corpus.Coverage.compiler_targets
      ~sources
      ~surface_ids:[ "one_off_v1"; "tool_v1"; "moderator_v1"; "delegated_moderator_v1" ]
  in
  (* Validate the maintained subset without claiming that all features have
     semantic coverage. The query still labels its packages incomplete. *)
  let%bind _ =
    Corpus.Coverage.audit corpus ~targets ~mappings:Corpus.Coverage.reviewed_mappings
  in
  let fingerprint =
    [%sexp
      ("ochat.authoring-query.v2" : string)
    , (Corpus.identity corpus : string)
    , (default_tokens : int)
    , (max_tokens : int)]
    |> Sexp.to_string
    |> Digest.digest
  in
  Ok { corpus; sources; secret; default_tokens; max_tokens; fingerprint }
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

let surface host task =
  let target, surface =
    match task with
    | Metadata.One_off_script -> V.One_off_script, "one_off_v1"
    | Standalone_tool -> V.Standalone_tool, "tool_v1"
    | Child_agent -> V.Generated_chatmd, "delegated_moderator_v1"
    | Moderator_tool | Background_workflow ->
      ( V.Moderator
      , (match V.moderator_surface host with
         | Ordinary -> "moderator_v1"
         | Delegated -> "delegated_moderator_v1") )
  in
  match List.mem (V.targets host) target ~equal:V.equal_target with
  | true -> Ok surface
  | false -> Error "authoring task is unavailable on the invoking host"
;;

let task_surface = surface

let roots task request =
  match text request "operation" with
  | "topic" -> Ok [ text request "topic_id" ]
  | "prepare" ->
    let base =
      match task with
      | Metadata.One_off_script -> [ "runtime.invocations.one-off" ]
      | Standalone_tool -> [ "runtime.invocations.standalone" ]
      | Moderator_tool -> [ "runtime.invocations.moderator" ]
      | Child_agent -> [ "runtime.delegation.stop-helper" ]
      | Background_workflow -> [ "runtime.jobs.timers"; "runtime.delivery.notifications" ]
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
         (("chatml.programs" :: "chatml.task-effects" :: base) @ extra)
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
        , if Result.is_ok (surface host task) then `True else `False )
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
        [ "chatml.programs"; "chatml.task-effects" ]
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
        [ "runtime.invocations.standalone" ]
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
          (match (C.descriptor binding).function_.description with
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
          "Enabled authoring tasks describe host validation targets; readable topics \
           describe compiler/reference compatibility. Neither proves a runtime effect \
           service is installed. selected_tools lists only the invoking scope's \
           bindings; execution still checks current permissions, tool selection and host \
           services." )
    ; ( "enabled_authoring_tasks"
      , strings
          (List.filter_map tasks ~f:(fun task ->
             match surface host task with
             | Ok _ -> Some (Metadata.task_id task)
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

let tool_items capabilities =
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
        , match descriptor.description with
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

let topic_items topic =
  List.mapi topic.Corpus.fragments ~f:(fun part fragment ->
    let text = blocks fragment.text |> String.concat ~sep:"\n\n" in
    `Object
      [ "topic_id", `String topic.specification.id
      ; "title", `String topic.specification.title
      ; "topic_sha256", `String topic.sha256
      ; "source", `String fragment.source.path
      ; "source_sha256", `String fragment.document_sha256
      ; "section", `String fragment.source.heading
      ; "part", integer part
      ; "content_sha256", `String (Digest.digest text)
      ; "text", `String text
      ])
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
               [ "topic_id", `String topic.specification.id
               ; "title", `String topic.specification.title
               ; "excerpt", `String excerpt
               ; "topic_sha256", `String topic.sha256
               ; "prerequisites", strings topic.specification.prerequisites
               ] )))
  |> List.sort ~compare:(fun (a, aid, _) (b, bid, _) ->
    match Int.compare b a with
    | 0 -> String.compare aid bid
    | other -> other)
  |> List.map ~f:(fun (_, _, item) -> item)
;;

let reference_search t ~capabilities ~surface_id query =
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
  let%map tools = tool_items capabilities in
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

let query t ~host ~capabilities ~scope request =
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
    let%bind items, covered =
      match operation, text base "topic_id" with
      | "topic", "reference.signatures" ->
        let%map items = signature_items t ~surface_id in
        items, [ "reference.signatures" ]
      | "topic", "reference.tools" ->
        let%map items = tool_items capabilities in
        items, [ "reference.tools" ]
      | "search", _ ->
        let query = text base "query" in
        let%map references = reference_search t ~capabilities ~surface_id query in
        search t.corpus ~surface_id query @ references, []
      | ("topic" | "prepare"), _ ->
        let%bind roots = roots task base in
        let%map topics = Corpus.assemble t.corpus ~surface_id ~roots in
        ( List.concat_map topics ~f:topic_items
        , List.map topics ~f:(fun topic -> topic.specification.id) )
      | _ -> Error "invalid continuation operation"
    in
    let%bind items, covered =
      match operation with
      | "prepare" ->
        let%bind tools = tool_items capabilities in
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
         | true -> Ok page
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
            | true -> Ok page
            | false ->
              Error
                ("budget too small for response metadata; request at least "
                 ^ Int.to_string page_size)))
    in
    pack [] 0 remaining
  in
  match run () with
  | Ok response -> response
  | Error message ->
    `Object
      [ "version", integer 1
      ; "error", `String "authoring.query_rejected"
      ; "message", `String message
      ]
;;
