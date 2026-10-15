open! Core
module P = Agent_protocol
module Capture = Chatml_developer_capture
module Developer = Authoring_context_service.Developer

module Route = struct
  type t =
    | Local_project of
        { root_index : int
        ; dependency_root_indices : int list
        ; project : Source.Portable_path.t
        ; selection : Chatml_project.Selection.request
        }
    | Local_source of
        { root_index : int
        ; member : Source.Portable_path.t
        }
    | Authoring_project of { expected : P.Authoring.Context.Expected.t }
    | Document_script of
        { expected : P.Authoring.Context.Expected.t
        ; selector : P.Authoring.Analysis.Target.selector
        }
end

module Fresh_source = struct
  type t =
    | Local_project of
        { root : Authoring_source_host.Root.t
        ; dependency_roots : Authoring_source_host.Root.t list
        }
    | Local_source of { root : Authoring_source_host.Root.t }
    | Authoring_project of { connection : Connection_context.t }
    | Document_script of { connection : Connection_context.t }
end

type t =
  { route : Route.t
  ; kind : Agent_store.Developer_command_receipt.Kind.t
  ; prompt : P.Id.Prompt_definition.t option
  ; workspace : P.Id.Workspace_definition.t option
  ; request : Developer.Request.t
  ; requested_execution : Chatml_execution.limits option
  ; requested_max_output_bytes : int option
  ; fingerprint : Chatml_source.Digest.t
  }

let invalid () = P.Error.invalid_request "invalid original developer command request"

let rec canonical ~checkpoint = function
  | `Object fields ->
    `Object
      (List.sort fields ~compare:(fun (left, _) (right, _) ->
         checkpoint ();
         String.compare left right)
       |> List.map ~f:(fun (key, value) ->
         checkpoint ();
         key, canonical ~checkpoint value))
  | `Array values ->
    `Array
      (List.map values ~f:(fun value ->
         checkpoint ();
         canonical ~checkpoint value))
  | (`Null | `True | `False | `String _ | `Number _) as value ->
    checkpoint ();
    value
;;

(* Match Jsonaf.Serializer's compact encoding exactly. In particular its
   string escaping is byte-local, so serializing bounded slices and removing
   their outer quotes preserves the same bytes, even across UTF-8 code points.
   Jsonaf's number serializer writes the retained number text verbatim. *)
let canonical_string_with_checkpoint ~checkpoint json =
  let buffer = Buffer.create 4096 in
  let chunk_size = 65_536 in
  let visits = ref 0 in
  let poll () =
    Int.incr visits;
    if !visits land 0xff = 0 then checkpoint ()
  in
  let add_chunks text =
    let rec loop offset =
      if offset < String.length text
      then (
        let length = Int.min chunk_size (String.length text - offset) in
        Buffer.add_substring buffer text ~pos:offset ~len:length;
        checkpoint ();
        loop (offset + length))
    in
    loop 0
  in
  let add_string text =
    Buffer.add_char buffer '"';
    let rec loop offset =
      if offset < String.length text
      then (
        let length = Int.min chunk_size (String.length text - offset) in
        let piece = String.sub text ~pos:offset ~len:length in
        let encoded = Jsonaf.to_string (`String piece) in
        Buffer.add_substring buffer encoded ~pos:1 ~len:(String.length encoded - 2);
        checkpoint ();
        loop (offset + length))
    in
    loop 0;
    Buffer.add_char buffer '"'
  in
  let rec write = function
    | `Null -> Buffer.add_string buffer "null"
    | `True -> Buffer.add_string buffer "true"
    | `False -> Buffer.add_string buffer "false"
    | `Number value -> add_chunks value
    | `String value -> add_string value
    | `Array values ->
      Buffer.add_char buffer '[';
      List.iteri values ~f:(fun index value ->
        if index > 0 then Buffer.add_char buffer ',';
        poll ();
        write value);
      Buffer.add_char buffer ']'
    | `Object fields ->
      Buffer.add_char buffer '{';
      List.iteri fields ~f:(fun index (key, value) ->
        if index > 0 then Buffer.add_char buffer ',';
        poll ();
        add_string key;
        Buffer.add_char buffer ':';
        write value);
      Buffer.add_char buffer '}'
  in
  write json;
  checkpoint ();
  Buffer.contents buffer
;;

let string value = `String value
let number value = `Number (Int.to_string value)
let float value = `Number (Float.to_string value)

let option f = function
  | None -> `Null
  | Some value -> f value
;;

(* Host request bound before building a fingerprint, separate from the
   selected project's own name/environment validity. *)
let max_request_selector_bytes = 1024

let selection = function
  | Chatml_project.Selection.Entry { name; environment } ->
    `Object
      [ "kind", string "entry"
      ; "name", string name
      ; "environment", option string environment
      ]
  | Library { environment } ->
    `Object [ "kind", string "library"; "environment", option string environment ]
;;

let route = function
  | Route.Local_project
      { root_index; dependency_root_indices; project; selection = choice } ->
    `Object
      [ "kind", string "local_project"
      ; "root_index", number root_index
      ; "dependency_root_indices", `Array (List.map dependency_root_indices ~f:number)
      ; "project", string (Source.Portable_path.to_string project)
      ; "selection", selection choice
      ]
  | Local_source { root_index; member } ->
    `Object
      [ "kind", string "local_source"
      ; "root_index", number root_index
      ; "member", string (Source.Portable_path.to_string member)
      ]
  | Authoring_project { expected } ->
    `Object
      [ "kind", string "authoring_project"
      ; "expected", P.Authoring.Context.Expected.to_json expected
      ]
  | Document_script { expected; selector } ->
    `Object
      [ "kind", string "document_script"
      ; "expected", P.Authoring.Context.Expected.to_json expected
      ; "selector", P.Authoring.Analysis.Target.selector_to_json selector
      ]
;;

let kind = function
  | Agent_store.Developer_command_receipt.Kind.Check -> "check"
  | Fixture_run -> "fixture_run"
  | Live_run -> "live_run"
;;

let execution (limits : Chatml_execution.limits) =
  `Object
    [ "fuel", number limits.fuel
    ; "max_tasks", number limits.max_tasks
    ; "wall_seconds", float limits.wall_seconds
    ; "max_value_bytes", number limits.max_value_bytes
    ; "max_array_items", number limits.max_array_items
    ; "max_depth", number limits.max_depth
    ; "allocation_bytes", number limits.allocation_bytes
    ; "max_calls", number limits.max_calls
    ; "max_invocation_depth", number limits.max_invocation_depth
    ]
;;

let bounded_selection = function
  | Chatml_project.Selection.Entry { name; environment } ->
    (not (String.is_empty name))
    && String.length name <= max_request_selector_bytes
    && Option.for_all environment ~f:(fun value ->
      String.length value <= max_request_selector_bytes)
  | Library { environment } ->
    Option.for_all environment ~f:(fun value ->
      String.length value <= max_request_selector_bytes)
;;

let validate_route = function
  | Route.Local_project { root_index; dependency_root_indices; selection; _ } ->
    let max_roots = Project_source_configuration.maximum_external_roots in
    root_index >= 0
    && root_index < max_roots
    && List.length dependency_root_indices < max_roots
    && List.for_all dependency_root_indices ~f:(fun index ->
      index >= 0 && index < max_roots && not (Int.equal index root_index))
    && (not (List.contains_dup dependency_root_indices ~compare:Int.compare))
    && bounded_selection selection
  | Local_source { root_index; _ } ->
    root_index >= 0 && root_index < Project_source_configuration.maximum_external_roots
  | Authoring_project _ | Document_script _ -> true
;;

let create
      ~route:source_route
      ~kind:command_kind
      ~prompt
      ~workspace
      ~entrypoint
      ~input
      ~fixtures
      ~requested_execution
      ~requested_max_output_bytes
      ~bounds
      ~checkpoint
  =
  let open Result.Let_syntax in
  let valid_resources =
    match command_kind, prompt, workspace with
    | Agent_store.Developer_command_receipt.Kind.Live_run, Some _, Some _ -> true
    | (Check | Fixture_run), None, None -> true
    | Live_run, None, _
    | Live_run, Some _, None
    | (Check | Fixture_run), Some _, _
    | (Check | Fixture_run), None, Some _ -> false
  in
  if
    (not (validate_route source_route))
    || (not valid_resources)
    || Option.exists requested_execution ~f:(fun limits ->
      not (Chatml_execution.valid_limits limits))
    || Option.exists requested_max_output_bytes ~f:(fun bytes -> bytes <= 0)
  then Error (invalid ())
  else (
    let%bind request =
      Developer.Request.create
        ~entrypoint
        ~input
        ~fixtures
        ~json_bounds:bounds
        ~checkpoint
    in
    let%bind () =
      Document_schema.Json.validate_and_measure_with_checkpoint
        ~limits:Document_schema.Limits.default
        ~checkpoint
        input
      |> Result.map_error ~f:(fun _ -> invalid ())
      |> Result.map ~f:ignore
    in
    let visits = ref 0 in
    let poll () =
      Int.incr visits;
      if !visits land 0xff = 0 then checkpoint ()
    in
    let fingerprint =
      `Object
        [ "version", number 1
        ; "kind", string (kind command_kind)
        ; "prompt", option (fun id -> string (P.Id.Prompt_definition.to_string id)) prompt
        ; ( "workspace"
          , option (fun id -> string (P.Id.Workspace_definition.to_string id)) workspace )
        ; "route", route source_route
        ; "entrypoint", string entrypoint
        ; "input", canonical ~checkpoint:poll input
        ; ( "fixtures"
          , option
              (fun fixture ->
                 string (Chatml_source.Digest.to_hex (Capture.Fixture.digest fixture)))
              fixtures )
        ; "requested_execution", option execution requested_execution
        ; "requested_max_output_bytes", option number requested_max_output_bytes
        ]
      |> canonical_string_with_checkpoint ~checkpoint
      |> Chatml_source.Digest.of_contents ~checkpoint
    in
    Ok
      { route = source_route
      ; kind = command_kind
      ; prompt
      ; workspace
      ; request
      ; requested_execution
      ; requested_max_output_bytes
      ; fingerprint
      })
;;

let kind t = t.kind
let route t = t.route
let prompt t = t.prompt
let workspace t = t.workspace
let requested_execution t = t.requested_execution
let requested_max_output_bytes t = t.requested_max_output_bytes
let fingerprint t = t.fingerprint
let capture_request t = Ok t.request
