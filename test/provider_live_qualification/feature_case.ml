open! Core
module R = Inference.Request
module P = History_entry.Payload
module O = Inference.Observation

module Case = struct
  type t =
    | Json_schema
    | Image
    | Reasoning
    | Document
    | Function_call
  [@@deriving equal, sexp_of]

  let all = [ Json_schema; Image; Reasoning; Document; Function_call ]

  let name = function
    | Json_schema -> "json_schema"
    | Image -> "image"
    | Reasoning -> "reasoning"
    | Document -> "document"
    | Function_call -> "function_call"
  ;;

  let maximum_attempts _ = 1
end

module Error = struct
  type t =
    | Invalid_fixture
    | Wrong_route
    | Outcome_not_completed
    | Missing_assistant_text
    | Output_mismatch
    | Configuration_mismatch
    | Transport_mismatch
    | Unexpected_tool_candidate
    | Evidence_limit
  [@@deriving equal, sexp_of]
end

let schema_setting =
  `Object
    [ ( "format"
      , `Object
          [ "type", `String "json_schema"
          ; "name", `String "qualification_answer"
          ; ( "schema"
            , `Object
                [ "type", `String "object"
                ; ( "properties"
                  , `Object
                      [ ( "marker"
                        , `Object
                            [ "type", `String "string"
                            ; "enum", `Array [ `String "SCHEMA_OK" ]
                            ] )
                      ] )
                ; "required", `Array [ `String "marker" ]
                ; "additionalProperties", `False
                ] )
          ; "strict", `True
          ] )
    ]
;;

let reasoning_setting = `Object [ "effort", `String "low"; "summary", `String "auto" ]

let fixed_setting = function
  | Case.Json_schema -> Some ("text", schema_setting)
  | Reasoning -> Some ("reasoning", reasoning_setting)
  | Function_call ->
    Some
      ( "tool_choice"
      , `Object [ "type", `String "function"; "name", `String "qualification_echo" ] )
  | Image | Document -> None
;;

(* Valid 32x32 RGB PNG: every pixel is red; CRCs and decompressed row size checked
   independently when authored. This is synthetic fixture data, never user media. *)
let png_base64 =
  "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAIAAAD8GO2jAAAAKElEQVR4nO3NsQ0AAAzCMP5/un0CNkuZ41wybXsHAAAAAAAAAAAAxR4yw/wuPL6QkAAAAABJRU5ErkJggg=="
;;

let pdf () =
  let stream = "BT /F1 18 Tf 40 100 Td (DOC_OK) Tj ET\n" in
  let objects =
    [ "<< /Type /Catalog /Pages 2 0 R >>"
    ; "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
    ; "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 160] /Resources << /Font << /F1 4 \
       0 R >> >> /Contents 5 0 R >>"
    ; "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
    ; sprintf "<< /Length %d >>\nstream\n%sendstream" (String.length stream) stream
    ]
  in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "%PDF-1.4\n";
  let offsets =
    List.mapi objects ~f:(fun i body ->
      let offset = Buffer.length buffer in
      Buffer.add_string buffer (sprintf "%d 0 obj\n%s\nendobj\n" (i + 1) body);
      offset)
  in
  let xref = Buffer.length buffer in
  Buffer.add_string buffer "xref\n0 6\n0000000000 65535 f \n";
  List.iter offsets ~f:(fun offset ->
    Buffer.add_string buffer (sprintf "%010d 00000 n \n" offset));
  Buffer.add_string
    buffer
    (sprintf "trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" xref);
  Buffer.contents buffer
;;

module Input = struct
  type route =
    | Session
    | Canonical_request
  [@@deriving equal, sexp_of]

  type t =
    { case : Case.t
    ; settings : R.Setting.t list
    }

  let route t =
    match t.case with
    | Document | Function_call -> Canonical_request
    | Json_schema | Image | Reasoning -> Session
  ;;

  let settings t = t.settings

  let session_content t =
    let text =
      match t.case with
      | Case.Json_schema -> Some "Return the structured result with marker SCHEMA_OK."
      | Reasoning ->
        Some "What is 17 + 25? Reply with only the integer answer and nothing else."
      | Image ->
        Some
          (sprintf
             "<user>What is the dominant color of this image? Reply with one uppercase \
              color word and nothing else.<img src=\"data:image/png;base64,%s\"/></user>"
             png_base64)
      | Document | Function_call -> None
    in
    Option.map text ~f:(fun text ->
      Agent_protocol.Session.Message_content.
        { kind = (if Case.equal t.case Image then Chatmd else Plain_text)
        ; text
        ; attachments = []
        })
  ;;

  let request t ~target ~history_id ~limits =
    if not (List.mem [ Case.Document; Function_call ] t.case ~equal:Case.equal)
    then Error Error.Wrong_route
    else
      let open Result.Let_syntax in
      let bytes = if Case.equal t.case Document then pdf () else "" in
      let%bind () =
        if String.length bytes <= 65536 then Ok () else Error Error.Evidence_limit
      in
      let%bind semantic =
        P.Semantic.create
          (Message
             { form = Input
             ; role = User
             ; phase = Absent
             ; content =
                 ([ P.Content.Text
                      { text =
                          (if Case.equal t.case Function_call
                           then
                             "Call qualification_echo exactly once with marker \
                              FUNCTION_OK. Do not emit text."
                           else
                             "Read the document. Reply with exactly its marker and \
                              nothing else.")
                      ; annotations = []
                      ; logprobs = Absent
                      }
                  ]
                  @
                  if Case.equal t.case Function_call
                  then []
                  else
                    [ P.Content.Unknown
                        { kind = "input_file"
                        ; raw =
                            `Object
                              [ "type", `String "input_file"
                              ; "filename", `String "qualification.pdf"
                              ; ( "file_data"
                                , `String
                                    ("data:application/pdf;base64,"
                                     ^ Base64.encode_exn bytes) )
                              ]
                        }
                    ])
             })
          ~metadata:P.Metadata.empty
        |> Result.map_error ~f:(fun _ -> Error.Invalid_fixture)
      in
      let%bind tools =
        if Case.equal t.case Function_call
        then
          R.Tool_spec.create
            ~name:"qualification_echo"
            ~description:(Value "Synthetic wire-only qualification; never executed")
            ~output_schema:Absent
            ~view:
              (Function
                 { parameters =
                     Value
                       (`Object
                           [ "type", `String "object"
                           ; ( "properties"
                             , `Object
                                 [ ( "marker"
                                   , `Object
                                       [ "type", `String "string"
                                       ; "enum", `Array [ `String "FUNCTION_OK" ]
                                       ] )
                                 ] )
                           ; "required", `Array [ `String "marker" ]
                           ; "additionalProperties", `False
                           ])
                 ; strict = Value true
                 })
            ~limits
          |> Result.map_error ~f:(fun _ -> Error.Invalid_fixture)
          |> Result.map ~f:List.return
        else Ok []
      in
      R.create
        ~target
        ~history:[ History_entry.create_with_id ~id:history_id (P.authored semantic) ]
        ~tools
        ~assets:[]
        ~limits
      |> Result.map_error ~f:(fun _ -> Error.Invalid_fixture)
  ;;
end

let input case =
  let open Result.Let_syntax in
  let%bind settings =
    match fixed_setting case with
    | None -> Ok []
    | Some (name, value) ->
      R.Setting.create
        ~name
        ~value:(Value value)
        ~provenance:Execution_override
        ~limits:Document_schema.Limits.default
      |> Result.map_error ~f:(fun _ -> Error.Invalid_fixture)
      |> Result.map ~f:List.return
  in
  let%bind () =
    match Base64.decode png_base64 with
    | Ok bytes when String.length bytes <= 65536 -> Ok ()
    | Ok _ | Error _ -> Error Error.Invalid_fixture
  in
  Ok { Input.case; settings }
;;

module Evidence = struct
  type t = { case : Case.t }

  let case t = t.case
  let output_validated _ = true
  let configuration_validated _ = true

  let to_json t =
    `Object
      [ "case", `String (Case.name t.case)
      ; "output_validated", `True
      ; ( "execution_kind"
        , `String
            (match t.case with
             | Function_call -> "api_function_wire"
             | Document -> "api_auxiliary_document"
             | Json_schema | Image | Reasoning -> "assistant_output") )
      ; "native_effect_proven", `False
      ; "configuration_validated", `True
      ]
  ;;
end

let validate_base
      case
      ~target
      ~outcome
      ~configuration
      ~selected_transport
      ~expected_transport
  =
  let open Result.Let_syntax in
  let%bind () =
    if Inference.Event.Terminal.equal_outcome outcome Completed
    then Ok ()
    else Error Error.Outcome_not_completed
  in
  let%bind () =
    if O.Transport_selection.equal_transport selected_transport expected_transport
    then Ok ()
    else Error Error.Transport_mismatch
  in
  let%bind () =
    match fixed_setting case with
    | None -> Ok ()
    | Some (name, expected) ->
      (match
         List.find (R.Target.settings target) ~f:(fun s ->
           String.equal (R.Setting.name s) name)
       with
       | Some setting ->
         (match R.Setting.value setting with
          | Value actual when Document_schema.Json.equal expected actual -> Ok ()
          | Absent | Null | Value _ -> Error Error.Configuration_mismatch)
       | None -> Error Error.Configuration_mismatch)
  in
  let%bind expected_configuration =
    O.Configuration.of_target
      ?transport_policy:(O.Configuration.transport_policy configuration)
      target
      ~preparation_id:(O.Configuration.preparation_id configuration)
      ~transport:(O.Configuration.transport configuration)
      ~capabilities:(O.Configuration.capabilities configuration)
      ~limits:Document_schema.Limits.default
    |> Result.map_error ~f:(fun _ -> Error.Configuration_mismatch)
  in
  let%bind () =
    if O.Configuration.equal expected_configuration configuration
    then Ok ()
    else Error Error.Configuration_mismatch
  in
  Ok ()
;;

let validate
      case
      ~target
      ~outcome
      ~assistant_text
      ~configuration
      ~selected_transport
      ~expected_transport
      ~tool_candidates
  =
  let open Result.Let_syntax in
  let%bind () =
    if Case.equal case Function_call then Error Error.Wrong_route else Ok ()
  in
  let%bind () =
    validate_base
      case
      ~target
      ~outcome
      ~configuration
      ~selected_transport
      ~expected_transport
  in
  let%bind () =
    if tool_candidates = 0 then Ok () else Error Error.Unexpected_tool_candidate
  in
  let%bind () =
    if
      List.length assistant_text > 64
      || List.fold assistant_text ~init:0 ~f:(fun n s -> n + min 8193 (String.length s))
         > 8192
    then Error Error.Evidence_limit
    else Ok ()
  in
  let text = String.concat assistant_text |> String.strip in
  let%bind () =
    if String.is_empty text then Error Error.Missing_assistant_text else Ok ()
  in
  let valid =
    match case with
    | Case.Image -> String.equal text "RED"
    | Reasoning -> String.equal text "42"
    | Document -> String.equal text "DOC_OK"
    | Function_call -> false
    | Json_schema ->
      (try
         match Jsonaf.of_string text with
         | `Object [ ("marker", `String "SCHEMA_OK") ] -> true
         | `Null | `True | `False | `Number _ | `String _ | `Array _ | `Object _ -> false
       with
       | _ -> false)
  in
  if valid then Ok { Evidence.case } else Error Error.Output_mismatch
;;

let validate_function
      ~target
      ~outcome
      ~configuration
      ~selected_transport
      ~expected_transport
      ~tool_candidates
      ~candidates
  =
  let open Result.Let_syntax in
  let%bind () =
    validate_base
      Case.Function_call
      ~target
      ~outcome
      ~configuration
      ~selected_transport
      ~expected_transport
  in
  let%bind () =
    if tool_candidates = 1 then Ok () else Error Error.Unexpected_tool_candidate
  in
  match
    List.filter candidates ~f:(fun payload ->
      match P.Semantic.view (P.semantic payload) with
      | Call _ | Unknown _ -> true
      | _ -> false)
  with
  | [ payload ] ->
    let semantic = P.semantic payload in
    (match P.Semantic.view semantic, (P.Semantic.metadata semantic).call_id with
     | ( Call
           { kind = Function
           ; name
           ; input_bytes
           ; namespace = Absent
           ; async = Absent | Value false
           }
       , Value id )
       when String.equal name "qualification_echo"
            && (not (String.is_empty id))
            && String.length id <= 1024
            && String.length input_bytes <= 1024 ->
       (match Or_error.try_with (fun () -> Jsonaf.of_string input_bytes) with
        | Ok (`Object [ ("marker", `String "FUNCTION_OK") ]) ->
          Ok { Evidence.case = Function_call }
        | Ok _ | Error _ -> Error Error.Output_mismatch)
     | _ -> Error Error.Output_mismatch)
  | _ -> Error Error.Unexpected_tool_candidate
;;
