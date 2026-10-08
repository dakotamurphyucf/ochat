open Core
module P = History_entry.Payload
module J = Json_codec

let optional = Projection_codec.optional
let checked = Projection_codec.string_result

let nonempty name value =
  if String.is_empty value
  then Error (Protocol_error.invalid_request (name ^ " must be nonempty"))
  else Projection_codec.validate (`String value)
;;

module Key = struct
  type parent =
    { scope : Transcript.Scope.Key.t
    ; call_alias : string
    }
  [@@deriving compare, equal, hash, sexp_of]

  type t =
    { scope : Transcript.Scope.Key.t
    ; call_alias : string
    ; parent : parent option
    }
  [@@deriving compare, equal, hash, sexp_of]

  let create ~scope ~call_alias ~(parent : parent option) =
    let open Result.Let_syntax in
    let%bind () = nonempty "call alias" call_alias in
    let%bind () =
      match parent with
      | None -> Ok ()
      | Some parent -> nonempty "parent call alias" parent.call_alias
    in
    if
      Option.exists parent ~f:(fun parent ->
        Transcript.Scope.Key.equal scope parent.scope
        && String.equal call_alias parent.call_alias)
    then Error (Protocol_error.invalid_request "tool cannot parent itself")
    else Ok { scope; call_alias; parent }
  ;;

  let fields scope call_alias =
    [ "source", `String (Transcript.Source_id.to_string scope.Transcript.Scope.Key.source)
    ; "attempt", `String (Transcript.Attempt_id.to_string scope.attempt)
    ; "call_alias", `String call_alias
    ]
  ;;

  let to_json t =
    `Object
      (fields t.scope t.call_alias
       @ optional "parent" t.parent (fun (parent : parent) ->
         `Object (fields parent.scope parent.call_alias)))
  ;;

  let reference_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind source = J.required_as fields "source" J.string in
    let%bind source = checked (Transcript.Source_id.of_string source) in
    let%bind attempt = J.required_as fields "attempt" J.string in
    let%bind attempt = checked (Transcript.Attempt_id.of_string attempt) in
    let%map call_alias = J.required_as fields "call_alias" J.string in
    ({ scope = { source; attempt }; call_alias } : parent)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind reference = reference_of_json json in
    let%bind fields = J.fields json in
    let%bind parent = J.optional_as fields "parent" reference_of_json in
    create ~scope:reference.scope ~call_alias:reference.call_alias ~parent
  ;;
end

module Progress = struct
  type channel =
    | Assistant
    | Reasoning
    | Stdout
    | Stderr
    | Activity
  [@@deriving compare, equal, sexp_of]

  type update =
    | Append of string
    | Replace of string
  [@@deriving equal, sexp_of]

  type t =
    { channel : channel
    ; update : update
    }
  [@@deriving equal, sexp_of]

  let channels =
    [ "assistant", Assistant
    ; "reasoning", Reasoning
    ; "stdout", Stdout
    ; "stderr", Stderr
    ; "activity", Activity
    ]
  ;;

  let channel_to_json channel =
    `String (fst (List.find_exn channels ~f:(fun (_, v) -> equal_channel v channel)))
  ;;

  let channel_of_json = J.enum ~name:"progress channel" channels

  let to_json t =
    let kind, text =
      match t.update with
      | Append s -> "append", s
      | Replace s -> "replace", s
    in
    `Object
      [ "channel", channel_to_json t.channel
      ; "update", `String kind
      ; "text", `String text
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind channel = J.required_as fields "channel" channel_of_json in
    let%bind update =
      J.required_as
        fields
        "update"
        (J.enum ~name:"progress update" [ "append", `Append; "replace", `Replace ])
    in
    let%map text = J.required_as fields "text" J.string in
    { channel
    ; update =
        (match update with
         | `Append -> Append text
         | `Replace -> Replace text)
    }
  ;;
end

module Tool = struct
  type classification =
    | Subagent
    | Shell_script
  [@@deriving equal, sexp_of]

  type outcome =
    | Returned
    | Raised
    | Cancelled
  [@@deriving equal, sexp_of]

  type descriptor =
    { key : Key.t
    ; call_entry_id : History_entry.Id.t option
    ; name : string
    ; kind : P.Call_kind.t
    ; input : string
    ; classification : classification option
    }
  [@@deriving sexp_of]

  let kind_to_json = function
    | P.Call_kind.Function -> `String "function"
    | Custom -> `String "custom"
  ;;

  let kind_of_json =
    J.enum
      ~name:"tool kind"
      [ "function", P.Call_kind.Function; "custom", P.Call_kind.Custom ]
  ;;

  let classification_to_json = function
    | Subagent -> `String "subagent"
    | Shell_script -> `String "shell_script"
  ;;

  let classification_of_json =
    J.enum
      ~name:"tool classification"
      [ "subagent", Subagent; "shell_script", Shell_script ]
  ;;

  let outcome_to_json = function
    | Returned -> `String "returned"
    | Raised -> `String "raised"
    | Cancelled -> `String "cancelled"
  ;;

  let outcome_of_json =
    J.enum
      ~name:"tool outcome"
      [ "returned", Returned; "raised", Raised; "cancelled", Cancelled ]
  ;;

  let output_of_json json =
    checked (P.Output.of_json json ~limits:Projection_codec.limits)
  ;;

  let descriptor_to_json t =
    `Object
      ([ "key", Key.to_json t.key
       ; "name", `String t.name
       ; "kind", kind_to_json t.kind
       ; "input", `String t.input
       ]
       @ optional "call_entry_id" t.call_entry_id History.Id.to_json
       @ optional "classification" t.classification classification_to_json)
  ;;

  let descriptor key ~call_entry_id ~name ~kind ~input ~classification =
    let open Result.Let_syntax in
    let%bind () = nonempty "tool name" name in
    let t = { key; call_entry_id; name; kind; input; classification } in
    let%map () = Projection_codec.validate (descriptor_to_json t) in
    t
  ;;

  let descriptor_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind key = J.required_as fields "key" Key.of_json in
    let%bind call_entry_id = J.optional_as fields "call_entry_id" History.Id.of_json in
    let%bind name = J.required_as fields "name" J.string in
    let%bind kind = J.required_as fields "kind" kind_of_json in
    let%bind input = J.required_as fields "input" J.string in
    let%bind classification =
      J.optional_as fields "classification" classification_of_json
    in
    descriptor key ~call_entry_id ~name ~kind ~input ~classification
  ;;

  type event =
    | Started of descriptor
    | Progress of
        { key : Key.t
        ; progress : Progress.t
        }
    | Finished of
        { key : Key.t
        ; outcome : outcome
        ; output : P.Output.t option
        }
  [@@deriving sexp_of]

  let key = function
    | Started d -> d.key
    | Progress { key; _ } | Finished { key; _ } -> key
  ;;

  let to_json = function
    | Started descriptor ->
      `Object [ "type", `String "started"; "descriptor", descriptor_to_json descriptor ]
    | Progress { key; progress } ->
      `Object
        [ "type", `String "progress"
        ; "key", Key.to_json key
        ; "progress", Progress.to_json progress
        ]
    | Finished { key; outcome; output } ->
      `Object
        ([ "type", `String "finished"
         ; "key", Key.to_json key
         ; "outcome", outcome_to_json outcome
         ]
         @ optional "output" output P.Output.to_json)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "type" J.string in
    match kind with
    | "started" ->
      let%map d = J.required_as fields "descriptor" descriptor_of_json in
      Started d
    | "progress" ->
      let%bind key = J.required_as fields "key" Key.of_json in
      let%map progress = J.required_as fields "progress" Progress.of_json in
      Progress { key; progress }
    | "finished" ->
      let%bind key = J.required_as fields "key" Key.of_json in
      let%bind outcome = J.required_as fields "outcome" outcome_of_json in
      let%map output = J.optional_as fields "output" output_of_json in
      Finished { key; outcome; output }
    | _ -> Error (Protocol_error.invalid_request "unknown tool activity")
  ;;

  type channel_text =
    { channel : Progress.channel
    ; text : string
    ; complete : bool
    }
  [@@deriving sexp_of]

  type state =
    | Running
    | Finished of
        { outcome : outcome
        ; output : P.Output.t option
        }
  [@@deriving sexp_of]

  type summary =
    { key : Key.t
    ; descriptor : descriptor option
    ; channels : channel_text list
    ; state : state
    }
  [@@deriving sexp_of]

  let channel_to_json t =
    `Object
      [ "channel", Progress.channel_to_json t.channel
      ; "text", `String t.text
      ; ("complete", if t.complete then `True else `False)
      ]
  ;;

  let state_to_json = function
    | Running -> `Object [ "type", `String "running" ]
    | Finished { outcome; output } ->
      `Object
        ([ "type", `String "finished"; "outcome", outcome_to_json outcome ]
         @ optional "output" output P.Output.to_json)
  ;;

  let summary_to_json t =
    `Object
      ([ "key", Key.to_json t.key
       ; "channels", `Array (List.map t.channels ~f:channel_to_json)
       ; "state", state_to_json t.state
       ]
       @ optional "descriptor" t.descriptor descriptor_to_json)
  ;;

  let summary key ~(descriptor : descriptor option) ~channels ~state =
    let open Result.Let_syntax in
    if Option.exists descriptor ~f:(fun (d : descriptor) -> not (Key.equal key d.key))
    then Error (Protocol_error.invalid_request "tool summary descriptor key mismatch")
    else if
      Option.is_some
        (List.find_a_dup channels ~compare:(fun a b ->
           Progress.compare_channel a.channel b.channel))
    then Error (Protocol_error.invalid_request "duplicate tool summary channel")
    else (
      let t = { key; descriptor; channels; state } in
      let%map () = Projection_codec.validate (summary_to_json t) in
      t)
  ;;

  let channel_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind channel = J.required_as fields "channel" Progress.channel_of_json in
    let%bind text = J.required_as fields "text" J.string in
    let%map complete = J.required_as fields "complete" J.bool in
    { channel; text; complete }
  ;;

  let state_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "type" J.string in
    match kind with
    | "running" -> Ok Running
    | "finished" ->
      let%bind outcome = J.required_as fields "outcome" outcome_of_json in
      let%map output = J.optional_as fields "output" output_of_json in
      Finished { outcome; output }
    | _ -> Error (Protocol_error.invalid_request "unknown tool summary state")
  ;;

  let summary_of_json json =
    let open Result.Let_syntax in
    let%bind () = Projection_codec.validate json in
    let%bind fields = J.fields json in
    let%bind key = J.required_as fields "key" Key.of_json in
    let%bind descriptor = J.optional_as fields "descriptor" descriptor_of_json in
    let%bind channels = J.required_as fields "channels" (J.list channel_of_json) in
    let%bind state = J.required_as fields "state" state_of_json in
    summary key ~descriptor ~channels ~state
  ;;
end
