open Core
module CM = Prompt.Chat_markdown
module P = Agent_protocol

type mode =
  | One_off
  | Persistent
[@@deriving equal, sexp]

type call =
  { input : string
  ; mode : mode
  ; session_id : P.Id.Session.t option
  }
[@@deriving equal, sexp]

let description (agent : CM.agent_tool) (policy : CM.agent_persistence) =
  let authored =
    Option.value
      agent.description
      ~default:
        (sprintf
           "Run agent prompt located at %s and return its final answer."
           agent.agent)
  in
  let choice =
    match policy with
    | Persistent -> "This tool always uses a persistent session."
    | Optional ->
      "Set mode to persistent for a reusable session; omitted mode or one_off runs a \
       fresh one-off call."
  in
  String.concat
    ~sep:"\n\n"
    [ authored
    ; choice
      ^ " In persistent mode, omit session_id to create a new instance or supply the ID \
         returned by this tool to continue that instance. Results retain session_id and \
         the submission receipt, including when still pending or timed out. Use \
         agent_send, agent_read, agent_status, agent_wait and agent_stop to manage the \
         same session when those tools are available. Persistence preserves history; it \
         does not authorize continued execution after the parent stops."
    ]
;;

let parameters (policy : CM.agent_persistence) =
  let properties =
    [ "input", `Object [ "type", `String "string" ]
    ; ( "session_id"
      , `Object
          [ "type", `String "string"
          ; ( "description"
            , `String
                "Continue an existing instance of this authored tool. Omit to create a \
                 new persistent instance." )
          ] )
    ]
  in
  let properties =
    match policy with
    | Persistent -> properties
    | Optional ->
      properties
      @ [ ( "mode"
          , `Object
              [ "type", `String "string"
              ; "enum", `Array [ `String "one_off"; `String "persistent" ]
              ; "default", `String "one_off"
              ] )
        ]
  in
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array [ `String "input" ]
    ; "additionalProperties", `False
    ]
;;

let decode (policy : CM.agent_persistence) json =
  let open Result.Let_syntax in
  let%bind fields = P.Json_codec.fields json in
  let allowed =
    match policy with
    | Persistent -> [ "input"; "session_id" ]
    | Optional -> [ "input"; "session_id"; "mode" ]
  in
  let%bind () =
    match
      List.find (P.Json_codec.to_alist fields) ~f:(fun (name, _) ->
        not (List.mem allowed name ~equal:String.equal))
    with
    | None -> Ok ()
    | Some (name, _) ->
      Error (P.Error.invalid_request ("Unexpected agent tool field: " ^ name))
  in
  let%bind input = P.Json_codec.required_as fields "input" P.Json_codec.string in
  let%bind requested_mode =
    P.Json_codec.optional_as
      fields
      "mode"
      (P.Json_codec.enum
         ~name:"agent tool mode"
         [ "one_off", One_off; "persistent", Persistent ])
  in
  let mode =
    match policy with
    | Persistent -> Persistent
    | Optional -> Option.value requested_mode ~default:One_off
  in
  let%bind session_id =
    P.Json_codec.optional_as fields "session_id" P.Id.Session.of_json
  in
  match mode, session_id with
  | One_off, Some _ ->
    Error (P.Error.invalid_request "One-off agent calls cannot include session_id.")
  | _ -> Ok { input; mode; session_id }
;;
