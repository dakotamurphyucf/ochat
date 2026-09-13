(* Private offline answers. Never link into the installed authoring corpus. *)
open Core
open Authoring_evaluation

let delta =
  `Object
    [ "version", `Number "1"
    ; "target", `String "standalone_tool"
    ; ( "source"
      , `String
          ([%blob "fixtures/string-order.chatml"] ^ "\n" ^ [%blob "fixtures/delta.chatml"])
      )
    ; "tools", `Array []
    ; "input_schema", Jsonaf.of_string [%blob "fixtures/delta-input.json"]
    ; "output_schema", Jsonaf.of_string [%blob "fixtures/delta-output.json"]
    ]
;;

let reconciliation =
  `Object
    [ "version", `Number "1"
    ; "target", `String "one_off_script"
    ; ( "source"
      , `String
          ([%blob "fixtures/string-order.chatml"]
           ^ "\n"
           ^ [%blob "fixtures/reconcile.chatml"]) )
    ; "tools", `Array [ `String "read_file" ]
    ]
;;

let quota =
  `Object
    [ "source", `String [%blob "fixtures/quota.chatml"]
    ; "binding", `String [%blob "fixtures/quota-binding.chatmd"]
    ; "input_schema", Jsonaf.of_string [%blob "fixtures/quota-input.json"]
    ; "output_schema", Jsonaf.of_string [%blob "fixtures/quota-output.json"]
    ]
;;

let background =
  `Object
    [ "source", `String [%blob "fixtures/observe.chatml"]
    ; "binding", `String [%blob "fixtures/observe-binding.chatmd"]
    ; "input_schema", Jsonaf.of_string {|{"type":"object","additionalProperties":false}|}
    ; ( "output_schema"
      , Jsonaf.of_string
          {|{"type":"object","required":["job_id","status"],"properties":{"job_id":{"type":"string"},"status":{"const":"accepted"}},"additionalProperties":false}|}
      )
    ]
;;

let child =
  `Object
    [ ( "create"
      , `Object
          [ "version", `Number "1"
          ; "root_file", `String "reviewer.chatmd"
          ; ( "sources"
            , `Array
                [ `Object
                    [ "path", `String "reviewer.chatmd"
                    ; "text", `String [%blob "fixtures/reviewer.chatmd"]
                    ]
                ; `Object
                    [ "path", `String "instructions.chatmd"
                    ; "text", `String [%blob "fixtures/reviewer-instructions.chatmd"]
                    ]
                ] )
          ; "tools", `Array [ `String "read_file" ]
          ; "start_immediately", `True
          ; "lifetime", `String "owned"
          ; "idempotency_key", `String "evidence-review-child"
          ] )
    ; ( "send"
      , `Object
          [ "session_id", `String "$session_id"
          ; "message", `String "$message"
          ; "idempotency_key", `String "$key"
          ] )
    ; "read", `Object [ "session_id", `String "$session_id"; "cursor", `String "$cursor" ]
    ]
;;

let count source =
  `Object
    [ "version", `Number "1"
    ; "target", `String "one_off_script"
    ; "source", `String source
    ; "tools", `Array []
    ]
;;

let moderator ~id ~name ~source ~input ~output =
  `Object
    [ "source", `String source
    ; ( "binding"
      , `String
          (sprintf
             {|<tool name="%s" type="moderator" moderator="%s" input_schema="input.json" output_schema="output.json"/>|}
             name
             id) )
    ; "input_schema", input
    ; "output_schema", output
    ]
;;

let tally =
  moderator
    ~id:"tally"
    ~name:"tally"
    ~source:[%blob "fixtures/tally.chatml"]
    ~input:
      (Jsonaf.of_string
         {|{"type":"object","required":["amount"],"properties":{"amount":{"type":"integer"}},"additionalProperties":false}|})
    ~output:(Jsonaf.of_string {|{"type":"integer"}|})
;;

let digest =
  moderator
    ~id:"digest_owner"
    ~name:"hash"
    ~source:[%blob "fixtures/digest.chatml"]
    ~input:Digest_cases.input_schema
    ~output:(Jsonaf.of_string {|{"type":"string"}|})
;;
