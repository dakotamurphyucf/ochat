open! Core
module Request = Openai.Responses_request
module Field = Request.Field

let json = Jsonaf.of_string
let get = Or_error.ok_exn
let emit result = print_endline (Jsonaf.to_string (Request.to_jsonaf (get result)))
let message = json {|{"role":"user","content":"hello"}|}

let base fields =
  `Object
    ([ "model", `String "future-model"; "input", `Array [ message ]; "store", `False ]
     @ fields)
;;

let admission label request =
  printf
    "%s: %s\n"
    label
    (match Request.of_jsonaf request with
     | Ok _ -> "accepted"
     | Error _ -> "rejected")
;;

let%expect_test "minimal independent request and no implicit output defaults" =
  emit (Request.create ~model:"future-model" ~input:[ message ] ~stream:true ());
  let value = get (Request.of_jsonaf (base [])) in
  printf "model=%s stream=%b\n" (Request.model value) (Request.stream value);
  print_s (Request.field value "instructions" |> Field.sexp_of_t Jsonaf.sexp_of_t);
  [%expect
    {|
    {"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"truncation":"disabled","stream":true,"include":["reasoning.encrypted_content"]}
    model=future-model stream=false
    Absent
    |}]
;;

let%expect_test "presence-valued instructions and generation settings encode exact keys" =
  emit (Request.create ~model:"m" ~input:[] ~stream:false ~instructions:Field.Absent ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:false
       ~instructions:Field.Null
       ~max_output_tokens:Field.Null
       ~parallel_tool_calls:Field.Null
       ~temperature:Field.Null
       ~top_p:Field.Null
       ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:false
       ~instructions:(Field.Value "")
       ~max_output_tokens:(Field.Value 16)
       ~parallel_tool_calls:(Field.Value false)
       ~temperature:(Field.Value 0.)
       ~top_p:(Field.Value 1.)
       ());
  [%expect
    {|
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":false,"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":false,"instructions":null,"max_output_tokens":null,"parallel_tool_calls":null,"temperature":null,"top_p":null,"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":false,"instructions":"","max_output_tokens":16,"parallel_tool_calls":false,"temperature":0.0,"top_p":1.0,"include":["reasoning.encrypted_content"]}
    |}]
;;

let%expect_test "independent nullable field decoding preserves three states" =
  List.iter
    [ "instructions"
    ; "max_output_tokens"
    ; "parallel_tool_calls"
    ; "temperature"
    ; "top_p"
    ; "reasoning"
    ; "prompt_cache_key"
    ; "prompt_cache_retention"
    ; "include"
    ]
    ~f:(fun key ->
      let value = get (Request.of_jsonaf (base [ key, `Null ])) in
      printf "%s=%s\n" key (Jsonaf.to_string (Request.to_jsonaf value)));
  [%expect
    {|
    instructions={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"instructions":null}
    max_output_tokens={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"max_output_tokens":null}
    parallel_tool_calls={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"parallel_tool_calls":null}
    temperature={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"temperature":null}
    top_p={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"top_p":null}
    reasoning={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"reasoning":null}
    prompt_cache_key={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"prompt_cache_key":null}
    prompt_cache_retention={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"prompt_cache_retention":null}
    include={"model":"future-model","input":[{"role":"user","content":"hello"}],"store":false,"include":null}
    |}]
;;

let%expect_test "reasoning effort summary and output-format presence are independent" =
  let text format verbosity = Field.Value Request.Text.{ format; verbosity } in
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~reasoning:
         (Field.Value
            Request.Reasoning.
              { effort = Field.Value Xhigh; summary = Field.Value Concise })
       ~text:(text Field.Absent (Field.Value High))
       ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~text:(text (Field.Value Text) Field.Absent)
       ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~text:(text (Field.Value Json_object) Field.Null)
       ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~reasoning:
         (Field.Value Request.Reasoning.{ effort = Field.Null; summary = Field.Absent })
       ~text:
         (text
            (Field.Value
               (Json_schema
                  { name = "answer"
                  ; schema =
                      json
                        {|{"type":"object","properties":{},"additionalProperties":false}|}
                  ; description = Field.Absent
                  ; strict = Field.Value true
                  }))
            Field.Absent)
       ());
  [%expect
    {|
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"reasoning":{"effort":"xhigh","summary":"concise"},"text":{"verbosity":"high"},"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"text":{"format":{"type":"text"}},"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"text":{"format":{"type":"json_object"},"verbosity":null},"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"reasoning":{"effort":null},"text":{"format":{"type":"json_schema","name":"answer","schema":{"type":"object","properties":{},"additionalProperties":false},"strict":true}},"include":["reasoning.encrypted_content"]}
    |}]
;;

let%expect_test "full raw local history survives without narrowing exact call strings" =
  let input =
    List.map
      ~f:json
      [ {|{"type":"message","role":"developer","content":[{"type":"input_text","text":"rules","prompt_cache_breakpoint":{"mode":"explicit"}}],"extra":{"a":null}}|}
      ; {|{"type":"message","role":"user","content":[{"type":"input_image","image_url":"data:image/png;base64,eA==","detail":"original"},{"type":"input_file","filename":"a.pdf","file_data":"data:application/pdf;base64,eA=="}]}|}
      ; {|{"type":"reasoning","id":"rs_1","summary":[],"encrypted_content":"opaque-secret-bytes","provider_extra":{"x":1}}|}
      ; {|{"type":"function_call","name":"f","call_id":"same","namespace":"local","arguments":" {\"x\":1.00} \n"}|}
      ; {|{"type":"function_call_output","call_id":"same","output":"ok"}|}
      ; {|{"type":"custom_tool_call","name":"c","call_id":"c1","input":"x\n  y"}|}
      ; {|{"type":"custom_tool_call_output","call_id":"c1","output":[{"type":"input_text","text":"done"}]}|}
      ; {|{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done","annotations":[]},{"type":"refusal","refusal":"no"}],"phase":"final_answer"}|}
      ]
  in
  let value =
    get
      (Request.create ~model:"m" ~input ~stream:true ~include_encrypted_reasoning:true ())
  in
  printf
    "raw_input_equal=%b\n"
    (List.equal Jsonaf.exactly_equal input (Request.input value));
  List.iter (Request.input value) ~f:(fun item -> print_endline (Jsonaf.to_string item));
  print_endline (Jsonaf.to_string (Request.to_jsonaf value));
  [%expect
    {|
    raw_input_equal=true
    {"type":"message","role":"developer","content":[{"type":"input_text","text":"rules","prompt_cache_breakpoint":{"mode":"explicit"}}],"extra":{"a":null}}
    {"type":"message","role":"user","content":[{"type":"input_image","image_url":"data:image/png;base64,eA==","detail":"original"},{"type":"input_file","filename":"a.pdf","file_data":"data:application/pdf;base64,eA=="}]}
    {"type":"reasoning","id":"rs_1","summary":[],"encrypted_content":"opaque-secret-bytes","provider_extra":{"x":1}}
    {"type":"function_call","name":"f","call_id":"same","namespace":"local","arguments":" {\"x\":1.00} \n"}
    {"type":"function_call_output","call_id":"same","output":"ok"}
    {"type":"custom_tool_call","name":"c","call_id":"c1","input":"x\n  y"}
    {"type":"custom_tool_call_output","call_id":"c1","output":[{"type":"input_text","text":"done"}]}
    {"type":"message","role":"assistant","content":[{"type":"output_text","text":"done","annotations":[]},{"type":"refusal","refusal":"no"}],"phase":"final_answer"}
    {"model":"m","input":[{"type":"message","role":"developer","content":[{"type":"input_text","text":"rules","prompt_cache_breakpoint":{"mode":"explicit"}}],"extra":{"a":null}},{"type":"message","role":"user","content":[{"type":"input_image","image_url":"data:image/png;base64,eA==","detail":"original"},{"type":"input_file","filename":"a.pdf","file_data":"data:application/pdf;base64,eA=="}]},{"type":"reasoning","id":"rs_1","summary":[],"encrypted_content":"opaque-secret-bytes","provider_extra":{"x":1}},{"type":"function_call","name":"f","call_id":"same","namespace":"local","arguments":" {\"x\":1.00} \n"},{"type":"function_call_output","call_id":"same","output":"ok"},{"type":"custom_tool_call","name":"c","call_id":"c1","input":"x\n  y"},{"type":"custom_tool_call_output","call_id":"c1","output":[{"type":"input_text","text":"done"}]},{"type":"message","role":"assistant","content":[{"type":"output_text","text":"done","annotations":[]},{"type":"refusal","refusal":"no"}],"phase":"final_answer"}],"store":false,"truncation":"disabled","stream":true,"include":["reasoning.encrypted_content"]}
    |}]
;;

let%expect_test "local tools exact flat schema custom grammar and allowed-tools choice" =
  let function_tool =
    get
      (Request.Tool.function_
         ~name:"f"
         ~parameters:Field.Null
         ~strict:Field.Null
         ~description:Field.Null
         ())
  in
  let custom_tool =
    get
      (Request.Tool.custom
         ~name:"c"
         ~format:(Field.Value (Grammar { syntax = `Regex; definition = "x+" }))
         ~async:(Field.Value true)
         ())
  in
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~tools:(Field.Value [ function_tool; custom_tool ])
       ~tool_choice:
         (Field.Value (Allowed { mode = Required; tools = [ Function "f"; Custom "c" ] }))
       ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:false
       ~tools:(Field.Value [ function_tool ])
       ~tool_choice:(Field.Value (Named (Function "f")))
       ());
  [%expect
    {|
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"tools":[{"type":"function","name":"f","parameters":null,"strict":null,"description":null},{"type":"custom","name":"c","format":{"type":"grammar","syntax":"regex","definition":"x+"},"async":true}],"tool_choice":{"type":"allowed_tools","mode":"required","tools":[{"type":"function","name":"f"},{"type":"custom","name":"c"}]},"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":false,"tools":[{"type":"function","name":"f","parameters":null,"strict":null,"description":null}],"tool_choice":{"type":"function","name":"f"},"include":["reasoning.encrypted_content"]}
    |}]
;;

let%expect_test "cache options remain independent of deprecated retention" =
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~prompt_cache_key:(Field.Value "tenant")
       ~prompt_cache_retention:(Field.Value Hours_24)
       ~prompt_cache_options:
         (Field.Value
            Request.Cache.Options.
              { mode = Field.Value Explicit; ttl = Field.Value `Minutes_30 })
       ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:false
       ~prompt_cache_key:Field.Null
       ~prompt_cache_retention:Field.Null
       ~prompt_cache_options:(Field.Value { mode = Field.Absent; ttl = Field.Absent })
       ());
  [%expect
    {|
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"prompt_cache_key":"tenant","prompt_cache_retention":"24h","prompt_cache_options":{"mode":"explicit","ttl":"30m"},"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":false,"prompt_cache_key":null,"prompt_cache_retention":null,"prompt_cache_options":{},"include":["reasoning.encrypted_content"]}
    |}]
;;

let%expect_test "independent malformed dangerous and unsupported request fields reject" =
  List.iter
    [ "store true", json {|{"model":"m","input":[],"store":true}|}
    ; "automatic truncation", base [ "truncation", `String "auto" ]
    ; "null truncation", base [ "truncation", `Null ]
    ; "store missing", json {|{"model":"m","input":[]}|}
    ; "model null", json {|{"model":null,"input":[],"store":false}|}
    ; "model whitespace", json {|{"model":"  ","input":[],"store":false}|}
    ; "input string", json {|{"model":"m","input":"hi","store":false}|}
    ; "duplicate model", json {|{"model":"m","model":"n","input":[],"store":false}|}
    ; "previous response", base [ "previous_response_id", `Null ]
    ; "conversation", base [ "conversation", json {|{"id":"remote"}|} ]
    ; "compaction", base [ "context_management", `Array [] ]
    ; "background", base [ "background", `True ]
    ; "unknown extension", base [ "future_execute", `True ]
    ; "stream null", base [ "stream", `Null ]
    ; "text null", base [ "text", `Null ]
    ; "format null", base [ "text", json {|{"format":null}|} ]
    ; "tools null", base [ "tools", `Null ]
    ; "choice null", base [ "tool_choice", `Null ]
    ; "cache null", base [ "prompt_cache_options", `Null ]
    ; "cache mode null", base [ "prompt_cache_options", json {|{"mode":null}|} ]
    ; "cache ttl24h", base [ "prompt_cache_options", json {|{"ttl":"24h"}|} ]
    ; "cache prewarm", base [ "prompt_cache_options", json {|{"prewarm":true}|} ]
    ; "wrong summary", base [ "reasoning", json {|{"summary":"consise"}|} ]
    ; "wrong effort", base [ "reasoning", json {|{"effort":"infinite"}|} ]
    ; "tokens too small", base [ "max_output_tokens", `Number "15" ]
    ; "tokens fractional", base [ "max_output_tokens", `Number "16.5" ]
    ; "tokens string", base [ "max_output_tokens", `String "16" ]
    ; "temperature range", base [ "temperature", `Number "2.1" ]
    ; "top_p negative", base [ "top_p", `Number "-0.1" ]
    ; "invalid number", base [ "temperature", `Number "NaN" ]
    ; "hosted include", base [ "include", json {|["file_search_call.results"]|} ]
    ; "required empty", base [ "tool_choice", `String "required" ]
    ]
    ~f:(fun (label, value) -> admission label value);
  [%expect
    {|
    store true: rejected
    automatic truncation: rejected
    null truncation: rejected
    store missing: rejected
    model null: rejected
    model whitespace: rejected
    input string: rejected
    duplicate model: rejected
    previous response: rejected
    conversation: rejected
    compaction: rejected
    background: rejected
    unknown extension: rejected
    stream null: rejected
    text null: rejected
    format null: rejected
    tools null: rejected
    choice null: rejected
    cache null: rejected
    cache mode null: rejected
    cache ttl24h: rejected
    cache prewarm: rejected
    wrong summary: rejected
    wrong effort: rejected
    tokens too small: rejected
    tokens fractional: rejected
    tokens string: rejected
    temperature range: rejected
    top_p negative: rejected
    invalid number: rejected
    hosted include: rejected
    required empty: rejected
    |}]
;;

let%expect_test "encrypted replay baseline and explicit compatible-profile omission" =
  emit (Request.create ~model:"m" ~input:[] ~stream:true ());
  emit
    (Request.create
       ~model:"m"
       ~input:[]
       ~stream:true
       ~include_encrypted_reasoning:false
       ());
  [%expect
    {|
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true,"include":["reasoning.encrypted_content"]}
    {"model":"m","input":[],"store":false,"truncation":"disabled","stream":true}
    |}]
;;

let%expect_test "independent complete selected enum vocabularies and boundaries" =
  List.iter
    [ "none"; "minimal"; "low"; "medium"; "high"; "xhigh"; "max" ]
    ~f:(fun effort ->
      admission
        ("effort " ^ effort)
        (base [ "reasoning", `Object [ "effort", `String effort ] ]));
  List.iter [ "auto"; "concise"; "detailed" ] ~f:(fun summary ->
    admission
      ("summary " ^ summary)
      (base [ "reasoning", `Object [ "summary", `String summary ] ]));
  List.iter [ "low"; "medium"; "high" ] ~f:(fun verbosity ->
    admission
      ("verbosity " ^ verbosity)
      (base [ "text", `Object [ "verbosity", `String verbosity ] ]));
  List.iter [ "in_memory"; "24h" ] ~f:(fun retention ->
    admission retention (base [ "prompt_cache_retention", `String retention ]));
  admission "disabled truncation" (base [ "truncation", `String "disabled" ]);
  admission "temperature0" (base [ "temperature", `Number "0" ]);
  admission "temperature2" (base [ "temperature", `Number "2" ]);
  admission "top_p0" (base [ "top_p", `Number "0" ]);
  admission "top_p1" (base [ "top_p", `Number "1" ]);
  admission
    "schema null"
    (base [ "text", json {|{"format":{"type":"json_schema","name":"f","schema":null}}|} ]);
  admission
    "schema description null"
    (base
       [ ( "text"
         , json
             {|{"format":{"type":"json_schema","name":"f","schema":{},"description":null}}|}
         )
       ]);
  admission
    "schema strict null"
    (base
       [ ( "text"
         , json {|{"format":{"type":"json_schema","name":"f","schema":{},"strict":null}}|}
         )
       ]);
  [%expect
    {|
    effort none: accepted
    effort minimal: accepted
    effort low: accepted
    effort medium: accepted
    effort high: accepted
    effort xhigh: accepted
    effort max: accepted
    summary auto: accepted
    summary concise: accepted
    summary detailed: accepted
    verbosity low: accepted
    verbosity medium: accepted
    verbosity high: accepted
    in_memory: accepted
    24h: accepted
    disabled truncation: accepted
    temperature0: accepted
    temperature2: accepted
    top_p0: accepted
    top_p1: accepted
    schema null: rejected
    schema description null: rejected
    schema strict null: accepted
    |}]
;;

let%expect_test "tool result media nullability follows the actual content context" =
  let item kind parts =
    `Object [ "type", `String kind; "call_id", `String "c"; "output", json parts ]
  in
  let request item =
    `Object [ "model", `String "m"; "store", `False; "input", `Array [ item ] ]
  in
  admission
    "function image nullable detail"
    (request
       (item
          "function_call_output"
          {|[{"type":"input_image","image_url":"data:image/png;base64,eA==","detail":null}]|}));
  admission
    "custom image nullable detail"
    (request
       (item
          "custom_tool_call_output"
          {|[{"type":"input_image","image_url":"data:image/png;base64,eA==","detail":null}]|}));
  admission
    "function file nullable alternatives"
    (request
       (item
          "function_call_output"
          {|[{"type":"input_file","file_data":null,"file_url":"https://example.com/a.pdf","filename":null}]|}));
  admission
    "custom file nullable alternatives"
    (request
       (item
          "custom_tool_call_output"
          {|[{"type":"input_file","file_data":null,"file_url":"https://example.com/a.pdf","filename":null}]|}));
  admission
    "custom image explicit detail"
    (request
       (item
          "custom_tool_call_output"
          {|[{"type":"input_image","image_url":"data:image/png;base64,eA==","detail":"auto"}]|}));
  admission
    "function output-text wrong kind"
    (request (item "function_call_output" {|[{"type":"output_text","text":"hello"}]|}));
  [%expect
    {|
    function image nullable detail: accepted
    custom image nullable detail: rejected
    function file nullable alternatives: accepted
    custom file nullable alternatives: rejected
    custom image explicit detail: accepted
    function output-text wrong kind: rejected
    |}]
;;

let%expect_test "known optional replay fields and resource limits are validated" =
  let request item =
    `Object [ "model", `String "m"; "store", `False; "input", `Array [ json item ] ]
  in
  List.iter
    [ ( "call namespace null"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","namespace":null}|}
      )
    ; ( "call namespace number"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","namespace":17}|}
      )
    ; ( "call async string"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","async":"yes"}|}
      )
    ; ( "call caller boolean"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","caller":false}|}
      )
    ; ( "program caller missing identity"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","caller":{"type":"program"}}|}
      )
    ; ( "program caller empty identity"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","caller":{"type":"program","caller_id":""}}|}
      )
    ; ( "program caller valid shape"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","caller":{"type":"program","caller_id":"p"}}|}
      )
    ; ( "future caller raw shape"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","caller":{"type":"future","opaque":true}}|}
      )
    ; ( "result caller malformed"
      , {|{"type":"function_call_output","call_id":"c","output":"x","caller":false}|} )
    ; ( "call async null"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","async":null}|}
      )
    ; ( "call invalid status"
      , {|{"type":"function_call","call_id":"c","name":"f","arguments":"{}","status":"finished"}|}
      )
    ; "assistant null phase", {|{"role":"assistant","content":"x","phase":null}|}
    ; ( "assistant invalid phase"
      , {|{"role":"assistant","content":"x","phase":"intermediate"}|} )
    ; ( "function result nullable metadata"
      , {|{"type":"function_call_output","call_id":"c","output":"x","id":null,"status":null,"namespace":null}|}
      )
    ; ( "custom result id null"
      , {|{"type":"custom_tool_call_output","call_id":"c","output":"x","id":null}|} )
    ]
    ~f:(fun (label, item) -> admission label (request item));
  admission "byte limit" (base [ "instructions", `String (String.make 16_777_217 'x') ]);
  admission
    "node limit"
    (base [ "reasoning", `Array (List.init 100_001 ~f:(fun _ -> `Null)) ]);
  admission "negative tokens" (base [ "max_output_tokens", `Number "-16" ]);
  printf
    "nonfinite constructor=%s\n"
    (match
       Request.create
         ~model:"m"
         ~input:[]
         ~stream:true
         ~temperature:(Field.Value Float.nan)
         ()
     with
     | Ok _ -> "accepted"
     | Error _ -> "rejected");
  [%expect
    {|
    call namespace null: rejected
    call namespace number: rejected
    call async string: rejected
    call caller boolean: rejected
    program caller missing identity: rejected
    program caller empty identity: rejected
    program caller valid shape: accepted
    future caller raw shape: accepted
    result caller malformed: rejected
    call async null: rejected
    call invalid status: rejected
    assistant null phase: accepted
    assistant invalid phase: rejected
    function result nullable metadata: accepted
    custom result id null: rejected
    byte limit: rejected
    node limit: rejected
    negative tokens: rejected
    nonfinite constructor=rejected
    |}]
;;

let%expect_test "independent local-tool nullable and nonnullable cases" =
  List.iter
    [ ( "function nullable"
      , {|{"type":"function","name":"f","parameters":null,"strict":null}|} )
    ; ( "function explicit"
      , {|{"type":"function","name":"f","parameters":{},"strict":false,"output_schema":null,"async":false}|}
      )
    ; "function parameters absent", {|{"type":"function","name":"f","strict":null}|}
    ; "function strict absent", {|{"type":"function","name":"f","parameters":null}|}
    ; "custom omitted", {|{"type":"custom","name":"c"}|}
    ; "custom text", {|{"type":"custom","name":"c","format":{"type":"text"}}|}
    ; "custom description null", {|{"type":"custom","name":"c","description":null}|}
    ; "custom format null", {|{"type":"custom","name":"c","format":null}|}
    ; "custom async null", {|{"type":"custom","name":"c","async":null}|}
    ; ( "function async null"
      , {|{"type":"function","name":"f","parameters":null,"strict":null,"async":null}|} )
    ; ( "custom grammar invalid"
      , {|{"type":"custom","name":"c","format":{"type":"grammar","syntax":"unknown","definition":"x"}}|}
      )
    ; ( "deferred tool"
      , {|{"type":"function","name":"f","parameters":null,"strict":null,"defer_loading":true}|}
      )
    ; "hosted tool", {|{"type":"web_search"}|}
    ]
    ~f:(fun (label, value) -> admission label (base [ "tools", `Array [ json value ] ]));
  admission
    "duplicate names"
    (base
       [ "tools", json {|[{"type":"custom","name":"c"},{"type":"custom","name":"c"}]|} ]);
  admission
    "missing chosen tool"
    (base [ "tool_choice", json {|{"type":"custom","name":"c"}|} ]);
  admission
    "wrong chosen kind"
    (base
       [ "tools", json {|[{"type":"custom","name":"c"}]|}
       ; "tool_choice", json {|{"type":"function","name":"c"}|}
       ]);
  [%expect
    {|
    function nullable: accepted
    function explicit: accepted
    function parameters absent: rejected
    function strict absent: rejected
    custom omitted: accepted
    custom text: accepted
    custom description null: rejected
    custom format null: rejected
    custom async null: rejected
    function async null: rejected
    custom grammar invalid: rejected
    deferred tool: rejected
    hosted tool: rejected
    duplicate names: rejected
    missing chosen tool: rejected
    wrong chosen kind: rejected
    |}]
;;

let%expect_test
    "unsupported input and nested ambiguity reject instead of replaying remote state"
  =
  List.iter
    [ "item reference", {|{"type":"item_reference","id":"x"}|}
    ; "compaction item", {|{"type":"compaction","id":"x","encrypted_content":"x"}|}
    ; "hosted call", {|{"type":"web_search_call","id":"x"}|}
    ; "unknown role", {|{"role":"tool","content":"x"}|}
    ; "call missing identity", {|{"type":"function_call","name":"f","arguments":"{}"}|}
    ; ( "call wrong input"
      , {|{"type":"custom_tool_call","name":"c","call_id":"c","input":null}|} )
    ; "result null", {|{"type":"function_call_output","call_id":"c","output":null}|}
    ; "duplicate nested", {|{"role":"user","content":"x","extra":{"a":1,"a":2}}|}
    ; ( "provider image"
      , {|{"role":"user","content":[{"type":"input_image","file_id":"remote"}]}|} )
    ; ( "provider file"
      , {|{"role":"user","content":[{"type":"input_file","file_id":"remote"}]}|} )
    ; ( "ambiguous file"
      , {|{"role":"user","content":[{"type":"input_file","file_data":"x","file_url":"https://example.com"}]}|}
      )
    ; ( "opaque summary malformed"
      , {|{"type":"reasoning","id":"rs","summary":[{"type":"summary_text","text":1}]}|} )
    ]
    ~f:(fun (label, value) ->
      admission
        label
        (`Object [ "model", `String "m"; "store", `False; "input", `Array [ json value ] ]));
  let deep =
    List.fold (List.range 0 70) ~init:`Null ~f:(fun value _ -> `Array [ value ])
  in
  admission "nesting budget" (base [ "instructions", deep ]);
  [%expect
    {|
    item reference: rejected
    compaction item: rejected
    hosted call: rejected
    unknown role: rejected
    call missing identity: rejected
    call wrong input: rejected
    result null: rejected
    duplicate nested: rejected
    provider image: rejected
    provider file: rejected
    ambiguous file: rejected
    opaque summary malformed: rejected
    nesting budget: rejected
    |}]
;;
