open! Core
module Error = Protocol_error
module C = Developer_codec
module D = Document_schema
module Common = Developer_common

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

let optional fields name decode =
  let* value = C.optional fields name in
  Option.value_map value ~default:(Ok None) ~f:(fun json ->
    Result.map (decode json) ~f:Option.some)
;;

module Features = struct
  let repl = "developer.repl"
  let events = "developer.repl.events"
end

let supported_method_names =
  [ "developer.repl.capabilities"
  ; "developer.repl.create"
  ; "developer.repl.submit"
  ; "developer.repl.observe"
  ; "developer.repl.cancel_submission"
  ; "developer.repl.inspect_type"
  ; "developer.repl.task_references_page"
  ; "developer.repl.task.submit"
  ; "developer.repl.task.observe"
  ; "developer.repl.task.await_terminal"
  ; "developer.repl.task.cancel_and_wait"
  ; "developer.repl.reset_and_wait"
  ; "developer.repl.close_and_wait"
  ; "developer.repl.watch"
  ; "developer.repl.unwatch"
  ]
;;

module Json_limits = struct
  let to_json t =
    `Object
      [ "max_bytes", C.integer_json (D.Limits.max_bytes t)
      ; "max_depth", C.integer_json (D.Limits.max_depth t)
      ; "max_fields", C.integer_json (D.Limits.max_fields t)
      ; "max_nodes", C.integer_json (D.Limits.max_nodes t)
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields ~allowed:[ "max_bytes"; "max_depth"; "max_fields"; "max_nodes" ] json
    in
    let* max_bytes = get fields "max_bytes" C.positive in
    let* max_depth = get fields "max_depth" C.positive in
    let* max_fields = get fields "max_fields" C.positive in
    let* max_nodes = get fields "max_nodes" C.positive in
    D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
    |> Result.map_error ~f:(fun _ -> Error.invalid_request "invalid REPL JSON limits")
  ;;
end

module Limits = struct
  type t =
    { request : D.Limits.t
    ; result : D.Limits.t
    ; page : D.Limits.t
    ; response : D.Limits.t
    ; max_source_bytes : int
    ; max_reference_bytes : int
    ; max_output_bytes : int
    ; max_page_items : int
    ; max_event_bytes : int
    ; max_registry_entries : int
    }

  let finite n = n > 0 && n <= Authoring_codec.maximum_wire_integer

  let finite_json t =
    List.for_all
      [ D.Limits.max_bytes t
      ; D.Limits.max_depth t
      ; D.Limits.max_fields t
      ; D.Limits.max_nodes t
      ]
      ~f:finite
  ;;

  let create
        ~request
        ~result
        ~page
        ~response
        ~max_source_bytes
        ~max_reference_bytes
        ~max_output_bytes
        ~max_page_items
        ~max_event_bytes
        ~max_registry_entries
    =
    if
      List.for_all
        [ max_source_bytes
        ; max_reference_bytes
        ; max_output_bytes
        ; max_page_items
        ; max_event_bytes
        ; max_registry_entries
        ]
        ~f:finite
      && List.for_all [ request; result; page; response ] ~f:finite_json
      && max_source_bytes <= D.Limits.max_bytes request
      && D.Limits.max_bytes page <= D.Limits.max_bytes response
      && D.Limits.max_bytes result <= D.Limits.max_bytes response
      && max_event_bytes <= D.Limits.max_bytes response
    then
      Ok
        { request
        ; result
        ; page
        ; response
        ; max_source_bytes
        ; max_reference_bytes
        ; max_output_bytes
        ; max_page_items
        ; max_event_bytes
        ; max_registry_entries
        }
    else C.invalid "invalid REPL wire limits"
  ;;

  let request t = t.request
  let result t = t.result
  let page t = t.page
  let response t = t.response
  let max_source_bytes t = t.max_source_bytes
  let max_reference_bytes t = t.max_reference_bytes
  let max_output_bytes t = t.max_output_bytes
  let max_page_items t = t.max_page_items
  let max_event_bytes t = t.max_event_bytes
  let max_registry_entries t = t.max_registry_entries

  let to_json t =
    `Object
      [ "request", Json_limits.to_json t.request
      ; "result", Json_limits.to_json t.result
      ; "page", Json_limits.to_json t.page
      ; "response", Json_limits.to_json t.response
      ; "max_source_bytes", C.integer_json t.max_source_bytes
      ; "max_reference_bytes", C.integer_json t.max_reference_bytes
      ; "max_output_bytes", C.integer_json t.max_output_bytes
      ; "max_page_items", C.integer_json t.max_page_items
      ; "max_event_bytes", C.integer_json t.max_event_bytes
      ; "max_registry_entries", C.integer_json t.max_registry_entries
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "request"
          ; "result"
          ; "page"
          ; "response"
          ; "max_source_bytes"
          ; "max_reference_bytes"
          ; "max_output_bytes"
          ; "max_page_items"
          ; "max_event_bytes"
          ; "max_registry_entries"
          ]
        json
    in
    let* request = get fields "request" Json_limits.of_json in
    let* result = get fields "result" Json_limits.of_json in
    let* page = get fields "page" Json_limits.of_json in
    let* response = get fields "response" Json_limits.of_json in
    let* max_source_bytes = get fields "max_source_bytes" C.positive in
    let* max_reference_bytes = get fields "max_reference_bytes" C.positive in
    let* max_output_bytes = get fields "max_output_bytes" C.positive in
    let* max_page_items = get fields "max_page_items" C.positive in
    let* max_event_bytes = get fields "max_event_bytes" C.positive in
    let* max_registry_entries = get fields "max_registry_entries" C.positive in
    create
      ~request
      ~result
      ~page
      ~response
      ~max_source_bytes
      ~max_reference_bytes
      ~max_output_bytes
      ~max_page_items
      ~max_event_bytes
      ~max_registry_entries
  ;;
end

module Wire_id = struct
  let valid_char = function
    | '0' .. '9' | 'a' .. 'z' | '-' | '_' -> true
    | _ -> false
  ;;

  let validate prefix value =
    if
      String.length value > String.length prefix
      && String.length value <= 96
      && String.is_prefix value ~prefix
      && String.for_all value ~f:valid_char
    then Ok value
    else C.invalid "invalid REPL comparison ID"
  ;;

  module Instance = struct
    type t = string [@@deriving equal]

    let of_string = validate "ri_"
    let to_string t = t
  end

  module Epoch = struct
    type t = string [@@deriving equal]

    let of_string = validate "re_"
    let to_string t = t
  end

  module Revision = struct
    type t = string [@@deriving equal]

    let of_string = validate "rv_"
    let to_string t = t
  end
end

module Create = struct
  type t =
    { run_id : Id.Developer_run.t
    ; key : Idempotency_key.t
    ; route : Common.Route.t
    ; target : Common.Target.t
    ; prompt : Id.Prompt_definition.t
    ; workspace : Id.Workspace_definition.t
    ; requested_execution : Common.Execution_limits.t option
    ; requested_max_output_bytes : int option
    }

  let raw_json t =
    `Object
      ([ "run_id", Id.Developer_run.to_json t.run_id
       ; "key", Idempotency_key.to_json t.key
       ; "route", Common.Route.to_json t.route
       ; "target", `String (Common.Target.to_string t.target)
       ; "prompt", Id.Prompt_definition.to_json t.prompt
       ; "workspace", Id.Workspace_definition.to_json t.workspace
       ]
       @ C.optional_field
           "requested_execution"
           t.requested_execution
           Common.Execution_limits.to_json
       @ C.optional_field
           "requested_max_output_bytes"
           t.requested_max_output_bytes
           C.integer_json)
  ;;

  let create
        ~limits
        ~run_id
        ~key
        ~route
        ~target
        ~prompt
        ~workspace
        ~requested_execution
        ~requested_max_output_bytes
    =
    let t =
      { run_id
      ; key
      ; route
      ; target
      ; prompt
      ; workspace
      ; requested_execution
      ; requested_max_output_bytes
      }
    in
    let* () =
      match target with
      | One_off_v1 | Tool_v1 | Developer_tool_v1 -> Ok ()
      | Moderator_v1 | Delegated_moderator_v1 ->
        C.invalid "REPL requires a Standard target"
    in
    let* () =
      match requested_max_output_bytes with
      | Some n when n < 256 || n > limits.Limits.max_output_bytes ->
        C.invalid "invalid requested REPL output bound"
      | None | Some _ -> Ok ()
    in
    let* () = C.validate ~limits:limits.Limits.request (raw_json t) in
    Ok t
  ;;

  let run_id t = t.run_id
  let key t = t.key
  let route t = t.route
  let target t = t.target
  let prompt t = t.prompt
  let workspace t = t.workspace
  let requested_execution t = t.requested_execution
  let requested_max_output_bytes t = t.requested_max_output_bytes
  let to_json = raw_json

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields =
      C.fields
        ~allowed:
          [ "run_id"
          ; "key"
          ; "route"
          ; "target"
          ; "prompt"
          ; "workspace"
          ; "requested_execution"
          ; "requested_max_output_bytes"
          ]
        json
    in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* key = get fields "key" Idempotency_key.of_json in
    let* route =
      get fields "route" (Common.Route.of_json ~limits:limits.Limits.request)
    in
    let* target =
      get fields "target" (fun json ->
        let* value = C.string ~max_bytes:48 json in
        Common.Target.of_string value)
    in
    let* prompt = get fields "prompt" Id.Prompt_definition.of_json in
    let* workspace = get fields "workspace" Id.Workspace_definition.of_json in
    let* requested_execution =
      optional fields "requested_execution" Common.Execution_limits.of_json
    in
    let* requested_max_output_bytes =
      optional fields "requested_max_output_bytes" C.positive
    in
    create
      ~limits
      ~run_id
      ~key
      ~route
      ~target
      ~prompt
      ~workspace
      ~requested_execution
      ~requested_max_output_bytes
  ;;
end

module Submit = struct
  type t =
    { instance : Wire_id.Instance.t
    ; epoch : Wire_id.Epoch.t
    ; expected_revision : Wire_id.Revision.t
    ; key : Idempotency_key.t
    ; source : string
    }

  let to_json t =
    `Object
      [ "instance", `String (Wire_id.Instance.to_string t.instance)
      ; "epoch", `String (Wire_id.Epoch.to_string t.epoch)
      ; "expected_revision", `String (Wire_id.Revision.to_string t.expected_revision)
      ; "key", Idempotency_key.to_json t.key
      ; "source", `String t.source
      ]
  ;;

  let create ~limits ~instance ~epoch ~expected_revision ~key ~source =
    let t = { instance; epoch; expected_revision; key; source } in
    if String.length source > limits.Limits.max_source_bytes
    then C.limit "REPL source exceeds wire bound"
    else
      Result.map (C.validate ~limits:limits.Limits.request (to_json t)) ~f:(fun () -> t)
  ;;

  let instance t = t.instance
  let epoch t = t.epoch
  let expected_revision t = t.expected_revision
  let key t = t.key
  let source t = t.source

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields =
      C.fields ~allowed:[ "instance"; "epoch"; "expected_revision"; "key"; "source" ] json
    in
    let* instance =
      get fields "instance" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Instance.of_string value)
    in
    let* epoch =
      get fields "epoch" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Epoch.of_string value)
    in
    let* expected_revision =
      get fields "expected_revision" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Revision.of_string value)
    in
    let* key = get fields "key" Idempotency_key.of_json in
    let* source =
      get fields "source" (C.string ~max_bytes:limits.Limits.max_source_bytes)
    in
    create ~limits ~instance ~epoch ~expected_revision ~key ~source
  ;;
end

module Lookup = struct
  type t =
    { instance : Wire_id.Instance.t
    ; key : Idempotency_key.t
    }

  let create ~instance ~key = { instance; key }
  let instance t = t.instance
  let key t = t.key

  let to_json t =
    `Object
      [ "instance", `String (Wire_id.Instance.to_string t.instance)
      ; "key", Idempotency_key.to_json t.key
      ]
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields = C.fields ~allowed:[ "instance"; "key" ] json in
    let* instance =
      get fields "instance" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Instance.of_string value)
    in
    let* key = get fields "key" Idempotency_key.of_json in
    Ok (create ~instance ~key)
  ;;
end

module Current_epoch = struct
  type t =
    { instance : Wire_id.Instance.t
    ; epoch : Wire_id.Epoch.t
    ; key : Idempotency_key.t
    }

  let create ~instance ~epoch ~key = { instance; epoch; key }
  let instance t = t.instance
  let epoch t = t.epoch
  let key t = t.key

  let to_json t =
    `Object
      [ "instance", `String (Wire_id.Instance.to_string t.instance)
      ; "epoch", `String (Wire_id.Epoch.to_string t.epoch)
      ; "key", Idempotency_key.to_json t.key
      ]
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields = C.fields ~allowed:[ "instance"; "epoch"; "key" ] json in
    let* instance =
      get fields "instance" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Instance.of_string value)
    in
    let* epoch =
      get fields "epoch" (fun json ->
        let* value = C.string ~max_bytes:96 json in
        Wire_id.Epoch.of_string value)
    in
    let* key = get fields "key" Idempotency_key.of_json in
    Ok (create ~instance ~epoch ~key)
  ;;
end

module Type_selector = struct
  type t =
    | Expression
    | Binding_index of int
  [@@deriving equal, sexp_of]

  let to_json = function
    | Expression -> `Object [ "kind", `String "expression" ]
    | Binding_index index ->
      `Object [ "kind", `String "binding_index"; "index", C.integer_json index ]
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "index" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "expression" ->
      let* () = C.fields ~allowed:[ "kind" ] json |> Result.map ~f:ignore in
      Ok Expression
    | "binding_index" ->
      let* index = get fields "index" C.nonnegative in
      Ok (Binding_index index)
    | _ -> C.invalid "unknown REPL type selector"
  ;;
end

module Inspect = struct
  type t =
    { current : Current_epoch.t
    ; statement_index : int
    ; selector : Type_selector.t
    ; max_bytes : int
    }

  let to_json t =
    `Object
      [ "current", Current_epoch.to_json t.current
      ; "statement_index", C.integer_json t.statement_index
      ; "selector", Type_selector.to_json t.selector
      ; "max_bytes", C.integer_json t.max_bytes
      ]
  ;;

  let create ~limits ~current ~statement_index ~selector ~max_bytes =
    let t = { current; statement_index; selector; max_bytes } in
    if statement_index < 0 || statement_index > Authoring_codec.maximum_wire_integer
    then C.invalid "invalid REPL statement index"
    else if
      match selector with
      | Type_selector.Expression -> false
      | Binding_index index -> index < 0 || index > Authoring_codec.maximum_wire_integer
    then C.invalid "invalid REPL binding index"
    else if max_bytes <= 0 || max_bytes > limits.Limits.max_output_bytes
    then C.invalid "invalid REPL type display bound"
    else
      Result.map (C.validate ~limits:limits.Limits.request (to_json t)) ~f:(fun () -> t)
  ;;

  let current t = t.current
  let statement_index t = t.statement_index
  let selector t = t.selector
  let max_bytes t = t.max_bytes

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields =
      C.fields ~allowed:[ "current"; "statement_index"; "selector"; "max_bytes" ] json
    in
    let* current = get fields "current" (Current_epoch.of_json ~limits) in
    let* statement_index = get fields "statement_index" C.nonnegative in
    let* selector = get fields "selector" Type_selector.of_json in
    let* max_bytes = get fields "max_bytes" C.positive in
    create ~limits ~current ~statement_index ~selector ~max_bytes
  ;;
end

module Reference_page_request = struct
  type t =
    { current : Current_epoch.t
    ; after : string option
    ; limit : int
    }

  let to_json t =
    `Object
      ([ "current", Current_epoch.to_json t.current; "limit", C.integer_json t.limit ]
       @ C.optional_field "after" t.after (fun x -> `String x))
  ;;

  let create ~limits ~current ~after ~limit =
    let t = { current; after; limit } in
    if limit <= 0 || limit > limits.Limits.max_page_items
    then C.invalid "invalid REPL page item bound"
    else
      let* () =
        match after with
        | None -> Ok ()
        | Some value ->
          let* _ =
            C.string ~max_bytes:limits.Limits.max_reference_bytes (`String value)
          in
          Ok ()
      in
      Result.map (C.validate ~limits:limits.Limits.request (to_json t)) ~f:(fun () -> t)
  ;;

  let current t = t.current
  let after t = t.after
  let limit t = t.limit

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields = C.fields ~allowed:[ "current"; "after"; "limit" ] json in
    let* current = get fields "current" (Current_epoch.of_json ~limits) in
    let* after =
      optional fields "after" (C.string ~max_bytes:limits.Limits.max_reference_bytes)
    in
    let* limit = get fields "limit" C.positive in
    create ~limits ~current ~after ~limit
  ;;
end

module Task_submit = struct
  type t =
    { key : Idempotency_key.t
    ; reference : string
    ; requested_execution : Common.Execution_limits.t option
    ; requested_max_output_bytes : int option
    ; fresh_epoch : (Wire_id.Instance.t * Wire_id.Epoch.t) option
    }

  let to_json t =
    `Object
      ([ "key", Idempotency_key.to_json t.key; "reference", `String t.reference ]
       @ C.optional_field
           "requested_execution"
           t.requested_execution
           Common.Execution_limits.to_json
       @ C.optional_field
           "requested_max_output_bytes"
           t.requested_max_output_bytes
           C.integer_json
       @ C.optional_field "fresh_epoch" t.fresh_epoch (fun (instance, epoch) ->
         `Object
           [ "instance", `String (Wire_id.Instance.to_string instance)
           ; "epoch", `String (Wire_id.Epoch.to_string epoch)
           ]))
  ;;

  let create
        ~limits
        ~key
        ~reference
        ~requested_execution
        ~requested_max_output_bytes
        ~fresh_epoch
    =
    let t =
      { key; reference; requested_execution; requested_max_output_bytes; fresh_epoch }
    in
    if
      String.is_empty reference
      || String.length reference > limits.Limits.max_reference_bytes
    then C.invalid "invalid REPL Task reference bound"
    else
      let* () =
        match requested_max_output_bytes with
        | Some n when n < 256 || n > limits.Limits.max_output_bytes ->
          C.invalid "invalid requested Task output bound"
        | None | Some _ -> Ok ()
      in
      Result.map (C.validate ~limits:limits.Limits.request (to_json t)) ~f:(fun () -> t)
  ;;

  let key t = t.key
  let reference t = t.reference
  let requested_execution t = t.requested_execution
  let requested_max_output_bytes t = t.requested_max_output_bytes
  let fresh_epoch t = t.fresh_epoch

  let of_json ~limits json =
    let* () = C.validate ~limits:limits.Limits.request json in
    let* fields =
      C.fields
        ~allowed:
          [ "key"
          ; "reference"
          ; "requested_execution"
          ; "requested_max_output_bytes"
          ; "fresh_epoch"
          ]
        json
    in
    let* key = get fields "key" Idempotency_key.of_json in
    let* reference =
      get fields "reference" (C.string ~max_bytes:limits.Limits.max_reference_bytes)
    in
    let* requested_execution =
      optional fields "requested_execution" Common.Execution_limits.of_json
    in
    let* requested_max_output_bytes =
      optional fields "requested_max_output_bytes" C.positive
    in
    let* fresh_epoch =
      optional fields "fresh_epoch" (fun json ->
        let* fields = C.fields ~allowed:[ "instance"; "epoch" ] json in
        let* instance =
          get fields "instance" (fun json ->
            let* value = C.string ~max_bytes:96 json in
            Wire_id.Instance.of_string value)
        in
        let* epoch =
          get fields "epoch" (fun json ->
            let* value = C.string ~max_bytes:96 json in
            Wire_id.Epoch.of_string value)
        in
        Ok (instance, epoch))
    in
    create
      ~limits
      ~key
      ~reference
      ~requested_execution
      ~requested_max_output_bytes
      ~fresh_epoch
  ;;
end
