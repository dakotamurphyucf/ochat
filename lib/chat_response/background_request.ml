open Core
module C = Tool_capability
module P = One_off_request
module J = Agent_protocol.Json_codec
module Schema = Chatmd_shell_spec.Tool_schema

module Agent_artifact = struct
  type t =
    { revision_id : Agent_protocol.Id.Prompt_revision.t
    ; manifest_sha256 : Chatml_source.Digest.t
    ; root_relative_path : Source.Portable_path.t
    }

  let create ~revision_id ~manifest_sha256 ~root_relative_path =
    { revision_id; manifest_sha256; root_relative_path }
  ;;

  let revision_id t = t.revision_id
  let manifest_sha256 t = t.manifest_sha256
  let root_relative_path t = t.root_relative_path
end

type target =
  | Tool_target of string
  | Script_target of
      { source : string
      ; compiler_contract : string
      }

type t =
  { target : target
  ; input : Jsonaf.t
  ; pins : (string * string) list
  ; policy : P.policy
  ; agent_artifact : Agent_artifact.t option
  }

type execution =
  | Tool of
      { capabilities : C.t
      ; reference : C.reference
      ; input : Jsonaf.t
      ; policy : P.policy
      }
  | Script of
      { prepared : One_off_script.t
      ; input : Jsonaf.t
      ; policy : P.policy
      }

let invalid message = Error (Agent_protocol.Error.invalid_request message)
let policy t = t.policy
let agent_artifact t = t.agent_artifact
let digest = Chatmd_shell_spec.Source_ref.digest
let number value = `Number (Int.to_string value)
let seconds value = Jsonaf.Export.jsonaf_of_float value

let valid_digest value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let integer_policy (policy : P.policy) =
  let x = policy.execution in
  [ "max_source_bytes", policy.compilation.max_source_bytes
  ; "max_output_bytes", policy.max_output_bytes
  ; "fuel", x.fuel
  ; "max_tasks", x.max_tasks
  ; "max_calls", x.max_calls
  ; "max_invocation_depth", x.max_invocation_depth
  ; "allocation_bytes", x.allocation_bytes
  ; "max_value_bytes", x.max_value_bytes
  ; "max_array_items", x.max_array_items
  ; "max_depth", x.max_depth
  ]
;;

let policy_to_json (policy : P.policy) =
  `Object
    (("compile_seconds", seconds policy.compilation.wall_seconds)
     :: ("wall_seconds", seconds policy.execution.wall_seconds)
     :: List.map (integer_policy policy) ~f:(fun (name, value) -> name, number value))
;;

let strict_fields names json =
  let open Result.Let_syntax in
  (* Admit fixed field count and known keys before the duplicate checker allocates names.
     Stop after the first excess cell, even for an arbitrarily large input. *)
  let rec within remaining = function
    | [] -> true
    | (name, _) :: rest ->
      remaining > 0
      && List.mem names name ~equal:String.equal
      && within (remaining - 1) rest
  in
  let%bind () =
    match json with
    | `Object fields when within (List.length names) fields -> Ok ()
    | `Object _ -> invalid "unknown background request field"
    | _ -> invalid "expected a JSON object"
  in
  J.fields json
;;

let agent_artifact_to_json t =
  `Object
    [ ( "revision_id"
      , Agent_protocol.Id.Prompt_revision.to_json (Agent_artifact.revision_id t) )
    ; ( "manifest_sha256"
      , `String (Chatml_source.Digest.to_hex (Agent_artifact.manifest_sha256 t)) )
    ; ( "root_relative_path"
      , `String (Source.Portable_path.to_string (Agent_artifact.root_relative_path t)) )
    ]
;;

let agent_artifact_of_json json =
  let open Result.Let_syntax in
  let%bind fields =
    strict_fields [ "revision_id"; "manifest_sha256"; "root_relative_path" ] json
  in
  let%bind revision_id =
    J.required_as fields "revision_id" Agent_protocol.Id.Prompt_revision.of_json
  in
  let%bind manifest_sha256 = J.required_as fields "manifest_sha256" J.string in
  let%bind manifest_sha256 =
    Chatml_source.Digest.of_hex manifest_sha256
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "invalid selected artifact digest")
  in
  let%bind root_relative_path = J.required_as fields "root_relative_path" J.string in
  let%map root_relative_path =
    Source.Portable_path.create root_relative_path
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "invalid selected artifact root member")
  in
  Agent_artifact.create ~revision_id ~manifest_sha256 ~root_relative_path
;;

let with_agent_artifact t selected =
  match t.target, t.agent_artifact with
  | Tool_target _, None -> Ok { t with agent_artifact = Some selected }
  | Tool_target _, Some _ -> invalid "background request already selects agent code"
  | Script_target _, _ -> invalid "background script cannot select agent code"
;;

let policy_of_json json =
  let open Result.Let_syntax in
  let%bind fields =
    strict_fields
      ("compile_seconds"
       :: "wall_seconds"
       :: List.map (integer_policy P.default_policy) ~f:fst)
      json
  in
  let int ?(min = 1) name =
    J.required_as fields name (J.bounded_int ~min ~max:Int.max_value)
  in
  let duration name =
    J.required_as fields name (function
      | `Number encoded ->
        (match Float.of_string_opt encoded with
         | Some value when Float.is_finite value && Float.(value > 0.) -> Ok value
         | _ -> invalid "invalid background duration")
      | _ -> invalid "invalid background duration")
  in
  let%bind max_source_bytes = int "max_source_bytes" in
  let%bind compile_seconds = duration "compile_seconds" in
  let%bind max_output_bytes = int "max_output_bytes" in
  let%bind wall_seconds = duration "wall_seconds" in
  let%bind fuel = int "fuel" in
  let%bind max_tasks = int ~min:0 "max_tasks" in
  let%bind max_calls = int ~min:0 "max_calls" in
  let%bind max_invocation_depth = int "max_invocation_depth" in
  let%bind allocation_bytes = int "allocation_bytes" in
  let%bind max_value_bytes = int "max_value_bytes" in
  let%bind max_array_items = int "max_array_items" in
  let%map max_depth = int "max_depth" in
  P.
    { compilation = { max_source_bytes; wall_seconds = compile_seconds }
    ; execution =
        { fuel
        ; max_tasks
        ; wall_seconds
        ; max_value_bytes
        ; max_array_items
        ; max_depth
        ; allocation_bytes
        ; max_calls
        ; max_invocation_depth
        }
    ; max_output_bytes
    }
;;

let validate_policy ~(ceiling : P.policy) (policy : P.policy) =
  let open Result.Let_syntax in
  let valid policy =
    P.decode
      ~policy
      (`Object [ "source", `String ""; "input", `Null; "tools", `Array [] ])
    |> Result.map ~f:(fun _ -> ())
    |> Result.map_error ~f:(fun _ ->
      Agent_protocol.Error.invalid_request "invalid background resource policy")
  in
  let%bind () = valid ceiling in
  let%bind () = valid policy in
  match
    List.for_all2_exn
      (integer_policy policy)
      (integer_policy ceiling)
      ~f:(fun (_, requested) (_, maximum) -> requested <= maximum)
    && Float.(policy.compilation.wall_seconds <= ceiling.compilation.wall_seconds)
    && Float.(policy.execution.wall_seconds <= ceiling.execution.wall_seconds)
  with
  | true -> Ok ()
  | false -> invalid "background resource policy exceeds current host ceiling"
;;

let compiler_contract () =
  Chatml_compilation.contract One_off_v1 |> Sexp.to_string |> digest
;;

let capture_pins capabilities =
  List.map (C.references capabilities) ~f:(fun reference ->
    C.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
    |> Result.map ~f:(fun binding -> reference.name, C.permission_fingerprint binding))
  |> Result.all
  |> Result.map_error ~f:(fun error ->
    Agent_protocol.Error.invalid_request error.C.message)
;;

let target_to_json = function
  | Tool_target name -> `Object [ "kind", `String "tool"; "name", `String name ]
  | Script_target { source; compiler_contract } ->
    `Object
      [ "kind", `String "script"
      ; "source", `String source
      ; "compiler_contract", `String compiler_contract
      ]
;;

let to_json t =
  `Object
    ([ "version", number (if Option.is_some t.agent_artifact then 2 else 1)
     ; "target", target_to_json t.target
     ; "input", t.input
     ; "pins", `Object (List.map t.pins ~f:(fun (name, pin) -> name, `String pin))
     ; "policy", policy_to_json t.policy
     ]
     @ Option.to_list
         (Option.map t.agent_artifact ~f:(fun artifact ->
            "agent_artifact", agent_artifact_to_json artifact)))
;;

let source = function
  | Tool_target _ -> ""
  | Script_target script -> script.source
;;

let validate_pin_count (policy : P.policy) pins =
  match List.length pins <= policy.execution.max_array_items with
  | true -> Ok ()
  | false -> invalid "background capability selection exceeds its item limit"
;;

let pin_fields (policy : P.policy) json =
  let open Result.Let_syntax in
  (* Admit count and key bytes before [J.fields] retains duplicate-check keys. *)
  let rec admit remaining = function
    | [] -> Ok ()
    | _ :: _ when remaining <= 0 ->
      invalid "background capability selection exceeds its item limit"
    | (name, _) :: rest ->
      if String.is_empty name || String.length name > 256
      then invalid "invalid background capability pin"
      else admit (remaining - 1) rest
  in
  let%bind () =
    match json with
    | `Object fields -> admit policy.execution.max_array_items fields
    | _ -> invalid "expected a JSON object"
  in
  J.fields json
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = validate_pin_count t.policy t.pins in
  P.decode
    ~policy:t.policy
    (`Object
        [ "source", `String (source t.target)
        ; "input", t.input
        ; "tools", `Array (List.map t.pins ~f:(fun (name, _) -> `String name))
        ])
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:(fun _ ->
    Agent_protocol.Error.invalid_request "background request exceeds its resource policy")
;;

let validate_input reference input =
  Result.bind (Schema.compile reference.C.input_schema) ~f:(fun schema ->
    Schema.validate schema input)
  |> Result.map_error ~f:(fun _ ->
    Agent_protocol.Error.invalid_request "background tool input does not match its schema")
;;

let tool ~capabilities ~(reference : C.reference) ~input ~policy =
  let open Result.Let_syntax in
  let%bind binding =
    C.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  let%bind () = validate_input (C.reference binding) input in
  let t =
    { target = Tool_target reference.name
    ; input
    ; pins = [ reference.name, C.permission_fingerprint binding ]
    ; policy
    ; agent_artifact = None
    }
  in
  let%map () = validate t in
  t
;;

let script ~prepared ~input ~policy =
  let open Result.Let_syntax in
  let%bind pins = capture_pins (One_off_script.capabilities prepared) in
  let t =
    { target =
        Script_target
          { source = One_off_script.source prepared
          ; compiler_contract = compiler_contract ()
          }
    ; input
    ; pins
    ; policy
    ; agent_artifact = None
    }
  in
  let%map () = validate t in
  t
;;

let of_json ~policy json =
  let open Result.Let_syntax in
  let%bind () = validate_policy ~ceiling:policy policy in
  let%bind fields =
    strict_fields
      [ "version"; "target"; "input"; "pins"; "policy"; "agent_artifact" ]
      json
  in
  let%bind version = J.required_as fields "version" (J.bounded_int ~min:1 ~max:2) in
  let%bind agent_artifact =
    match version, J.optional fields "agent_artifact" with
    | 1, None -> Ok None
    | 2, Some json -> agent_artifact_of_json json |> Result.map ~f:Option.some
    | 1, Some _ -> invalid "version 1 cannot select agent code"
    | 2, None -> invalid "version 2 requires selected agent code"
    | _ -> assert false
  in
  let%bind stored_policy = J.required_as fields "policy" policy_of_json in
  let%bind () = validate_policy ~ceiling:policy stored_policy in
  let%bind target =
    J.required_as
      fields
      "target"
      (strict_fields [ "kind"; "name"; "source"; "compiler_contract" ])
  in
  let%bind kind = J.required_as target "kind" J.string in
  let%bind target =
    let strict names = strict_fields names (`Object (J.to_alist target)) in
    match kind with
    | "tool" ->
      let%bind fields = strict [ "kind"; "name" ] in
      let%map name = J.required_as fields "name" J.string in
      Tool_target name
    | "script" ->
      let%bind fields = strict [ "kind"; "source"; "compiler_contract" ] in
      let%bind source = J.required_as fields "source" J.string in
      let%bind compiler_contract = J.required_as fields "compiler_contract" J.string in
      let%map () =
        match valid_digest compiler_contract with
        | true -> Ok ()
        | false -> invalid "invalid background compiler contract pin"
      in
      Script_target { source; compiler_contract }
    | _ -> invalid "unknown background target kind"
  in
  let%bind pins = J.required_as fields "pins" (pin_fields stored_policy) in
  let%bind pins =
    List.map (J.to_alist pins) ~f:(fun (name, json) ->
      let%bind pin = J.string json in
      match String.length name > 0 && String.length name <= 256 && valid_digest pin with
      | true -> Ok (name, pin)
      | false -> invalid "invalid background capability pin")
    |> Result.all
    |> Result.map ~f:(List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b))
  in
  let%bind () =
    match target, pins with
    | Tool_target name, [ (selected, _) ] when String.equal name selected -> Ok ()
    | Tool_target _, _ -> invalid "background tool requires exactly its pinned binding"
    | Script_target _, _ -> Ok ()
  in
  let%bind input = J.required fields "input" in
  let%bind () =
    match target, agent_artifact with
    | Tool_target _, _ | Script_target _, None -> Ok ()
    | Script_target _, Some _ -> invalid "background script cannot select agent code"
  in
  let t = { target; input; pins; policy = stored_policy; agent_artifact } in
  let%map () = validate t in
  t
;;

let fingerprint t =
  to_json t
  |> J.canonical_string
  |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
  |> Result.ok_or_failwith
  |> fun encoded ->
  let prefix =
    if Option.is_some t.agent_artifact
    then "ochat.background-request.v2\n"
    else "ochat.background-request.v1\n"
  in
  digest (prefix ^ encoded)
;;

let rebind_capabilities ~pins ~capabilities:current =
  let open Result.Let_syntax in
  let%bind selected =
    C.select current ~names:(List.map pins ~f:fst)
    |> Result.map_error ~f:(fun error ->
      Agent_protocol.Error.invalid_request error.C.message)
  in
  let%bind current_pins = capture_pins selected in
  match
    List.equal (Tuple2.equal ~eq1:String.equal ~eq2:String.equal) pins current_pins
  with
  | true -> Ok selected
  | false -> invalid "background capability configuration changed; re-admission required"
;;

let rebind t current = rebind_capabilities ~pins:t.pins ~capabilities:current
let capability_pins = capture_pins
let validate_capabilities t ~capabilities = Result.map (rebind t capabilities) ~f:ignore

let prepare ~env ~current_capabilities ~policy t =
  let open Result.Let_syntax in
  let%bind () = validate_policy ~ceiling:policy t.policy in
  let%bind selected = rebind t (current_capabilities ()) in
  match t.target with
  | Tool_target name ->
    let%bind binding =
      C.find selected ~name
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
    in
    let reference = C.reference binding in
    let%map () = validate_input reference t.input in
    Tool { capabilities = selected; reference; input = t.input; policy = t.policy }
  | Script_target { source; compiler_contract = stored_contract } ->
    let%bind () =
      match String.equal stored_contract (compiler_contract ()) with
      | true -> Ok ()
      | false -> invalid "background compiler contract changed; re-admission required"
    in
    let%bind prepared =
      One_off_script.prepare_in_domain
        ~env
        ~limits:t.policy.compilation
        ~capabilities:selected
        ~tools:(List.map t.pins ~f:fst)
        ~source
        ()
      |> Result.map_error ~f:(fun _ ->
        Agent_protocol.Error.invalid_request
          "background script no longer compiles under its pinned contract")
    in
    let%map () =
      One_off_script.revalidate prepared ~capabilities:(current_capabilities ())
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request error.C.message)
    in
    Script { prepared; input = t.input; policy = t.policy }
;;
