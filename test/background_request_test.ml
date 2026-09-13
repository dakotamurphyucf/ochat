open Core
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module P = Chat_response.One_off_request
module Script = Chat_response.One_off_script

let digest = Chatmd_shell_spec.Source_ref.digest

let get result =
  Result.map_error result ~f:(fun e -> e.Agent_protocol.Error.message)
  |> Result.ok_or_failwith
;;

let cap result =
  Result.map_error result ~f:(fun e -> e.C.message) |> Result.ok_or_failwith
;;

let policy = P.default_policy

let registry
      ?(owner = "session-owner")
      ?(resource = "read-only:/workspace")
      ?(revision = "reader-v1")
      ?(result_contract = C.Native_output)
      calls
      names
  =
  let tool name =
    let module Definition = struct
      type input = string

      let name = name
      let description = Some "background request fixture"
      let type_ = "function"
      let parameters = `Object [ "type", `String "object" ]
      let input_of_string input = input
    end
    in
    Ochat_function.create_function
      (module Definition)
      (fun _ ->
         incr calls;
         failwith "request reconstruction executed a tool")
  in
  C.create
    ~owner
    ~resource_fingerprint:(digest resource)
    ~result_contracts:(List.map names ~f:(fun name -> name, result_contract))
    (List.map names ~f:(fun name -> digest revision, tool name))
  |> cap
;;

let prepare env current request =
  B.prepare ~env ~current_capabilities:(fun () -> current) ~policy request
;;

let show label result =
  print_s
    [%sexp
      (label : string)
    , ((match result with
        | Ok _ -> "ok"
        | Error e -> e.Agent_protocol.Error.message)
       : string)]
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object ((name, value) :: List.Assoc.remove fields name ~equal:String.equal)
  | _ -> assert false
;;

let%expect_test
    "durable tool requests re-admit exact configuration, never name-only authority"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    let original = registry calls [ "read_file"; "shell" ] in
    let reference = C.find original ~name:"read_file" |> cap |> C.reference in
    let captured =
      B.tool ~capabilities:original ~reference ~input:(`Object []) ~policy |> get
    in
    let restored =
      B.to_json captured
      |> Jsonaf.to_string
      |> Jsonaf.of_string
      |> B.of_json ~policy
      |> get
    in
    [%test_eq: string] (B.fingerprint captured) (B.fingerprint restored);
    let restarted = registry calls [ "read_file"; "shell"; "new_tool" ] in
    show
      "old live reference"
      (C.resolve restarted ~id:reference.id ~fingerprint:reference.fingerprint
       |> Result.map_error ~f:(fun e -> Agent_protocol.Error.invalid_request e.C.code));
    (match prepare env restarted restored |> get with
     | B.Tool { capabilities; reference = fresh; _ } ->
       print_s
         [%sexp (List.map (C.references capabilities) ~f:(fun r -> r.name) : string list)];
       assert (not (Agent_protocol.Id.Capability.equal reference.id fresh.id))
     | Script _ -> assert false);
    List.iter
      [ "owner", registry ~owner:"different-session" calls [ "read_file" ]
      ; "resource", registry ~resource:"read-write:/" calls [ "read_file" ]
      ; "revision", registry ~revision:"reader-v2" calls [ "read_file" ]
      ; "result contract", registry ~result_contract:C.Invocation_v1 calls [ "read_file" ]
      ]
      ~f:(fun (label, current) -> show label (prepare env current restored));
    show
      "wrong input"
      (B.tool ~capabilities:original ~reference ~input:(`String "bad") ~policy);
    print_s [%sexp (("tool calls", !calls) : string * int)]);
  [%expect
    {|
    ("old live reference" capability.stale_reference)
    (read_file)
    (owner "background capability configuration changed; re-admission required")
    (resource
     "background capability configuration changed; re-admission required")
    (revision
     "background capability configuration changed; re-admission required")
    ("result contract"
     "background capability configuration changed; re-admission required")
    ("wrong input" "background tool input does not match its schema")
    ("tool calls" 0)
    |}]
;;

let%expect_test
    "script restoration pins source, authority, compiler and effective budgets without \
     evaluation"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let calls = ref 0 in
    let original = registry calls [ "read_file"; "shell" ] in
    let source =
      "let poison = fail(\"initializer must not run\")\n\
       let main input = Task.bind(Tool.call(\"read_file\", input), fun result -> \
       Task.pure(input))"
    in
    let compiled =
      Script.prepare_in_domain
        ~env
        ~capabilities:original
        ~tools:[ "read_file" ]
        ~source
        ()
      |> Result.map_error ~f:(fun errors ->
        [%sexp (errors : Chatmd_shell_spec.Diagnostic.t list)] |> Sexp.to_string_hum)
      |> Result.ok_or_failwith
    in
    let lower =
      { policy with
        execution = { policy.execution with fuel = 321; wall_seconds = 0.123456 }
      ; compilation = { policy.compilation with wall_seconds = 3.234567 }
      }
    in
    let captured = B.script ~prepared:compiled ~input:(`Object []) ~policy:lower |> get in
    let encoded = B.to_json captured in
    let restored =
      encoded |> Jsonaf.to_string |> Jsonaf.of_string |> B.of_json ~policy |> get
    in
    let restarted = registry calls [ "read_file"; "shell"; "new_tool" ] in
    (match prepare env restarted restored |> get with
     | B.Script { prepared; policy = effective; _ } ->
       [%test_eq: string] source (Script.source prepared);
       [%test_eq: string] (B.fingerprint captured) (B.fingerprint restored);
       print_s
         [%sexp
           (List.map (C.references (Script.capabilities prepared)) ~f:(fun r -> r.name)
            : string list)
         , (effective.execution.fuel : int)
         , (effective.execution.wall_seconds : float)
         , (effective.compilation.wall_seconds : float)]
     | Tool _ -> assert false);
    let target =
      `Object
        [ "kind", `String "script"
        ; "source", `String source
        ; "compiler_contract", `String (digest "different compiler")
        ]
    in
    let incompatible = replace encoded "target" target |> B.of_json ~policy |> get in
    show "compiler changed" (prepare env restarted incompatible);
    let tighter = { policy with execution = { policy.execution with fuel = 320 } } in
    show "lower host ceiling" (B.of_json ~policy:tighter encoded);
    show
      "prepare rechecks host ceiling"
      (B.prepare
         ~env
         ~current_capabilities:(fun () -> restarted)
         ~policy:tighter
         restored);
    let reads = ref 0 in
    show
      "registration changed during compilation"
      (B.prepare
         ~env
         ~policy
         ~current_capabilities:(fun () ->
           incr reads;
           match !reads with
           | 1 -> restarted
           | _ -> registry calls [ "read_file" ])
         restored);
    print_s [%sexp (("tool calls", !calls) : string * int)]);
  [%expect
    {|
    ((read_file) 321 0.123456 3.234567)
    ("compiler changed"
     "background compiler contract changed; re-admission required")
    ("lower host ceiling"
     "background resource policy exceeds current host ceiling")
    ("prepare rechecks host ceiling"
     "background resource policy exceeds current host ceiling")
    ("registration changed during compilation"
     "selected one-off tool bindings changed")
    ("tool calls" 0)
    |}]
;;

let%expect_test "durable decoding rejects ambiguous authority and policy envelopes" =
  Mirage_crypto_rng_unix.use_default ();
  let calls = ref 0 in
  let capabilities = registry calls [ "read_file" ] in
  let reference = C.find capabilities ~name:"read_file" |> cap |> C.reference in
  let encoded =
    B.tool ~capabilities ~reference ~input:(`Object []) ~policy |> get |> B.to_json
  in
  let fields =
    match encoded with
    | `Object fields -> fields
    | _ -> assert false
  in
  List.iter
    [ "version", replace encoded "version" (`Number "2")
    ; "unknown field", `Object (("borrow", `String "expired") :: fields)
    ; "duplicate field", `Object (("input", `Null) :: fields)
    ; ( "extra authority"
      , replace
          encoded
          "pins"
          (`Object
              [ "read_file", `String (digest "read"); "shell", `String (digest "shell") ])
      )
    ; ( "duplicate authority"
      , replace
          encoded
          "pins"
          (`Object
              [ "read_file", `String (digest "a"); "read_file", `String (digest "b") ]) )
    ; ( "changed target"
      , replace
          encoded
          "target"
          (`Object [ "kind", `String "tool"; "name", `String "shell" ]) )
    ]
    ~f:(fun (label, json) ->
      print_s [%sexp (label : string), (Result.is_error (B.of_json ~policy json) : bool)]);
  [%expect
    {|
    (version true)
    ("unknown field" true)
    ("duplicate field" true)
    ("extra authority" true)
    ("duplicate authority" true)
    ("changed target" true)
    |}]
;;
