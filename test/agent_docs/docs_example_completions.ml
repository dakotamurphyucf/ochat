open! Core
module C = Openai.Completions

let tools : C.tool list =
  [ { type_ = "function"
    ; function_ =
        { name = "echo"
        ; description = Some "Return the supplied text"
        ; parameters =
            Jsonaf.of_string
              {|{"type":"object","properties":{"text":{"type":"string"}},"required":["text"],"additionalProperties":false}|}
        ; strict = true
        }
    }
  ]
;;

let user text : C.chat_message =
  { role = "user"
  ; content = Some (C.Text text)
  ; name = None
  ; tool_call_id = None
  ; function_call = None
  ; tool_calls = None
  }
;;

let ai_echo env text =
  C.post_chat_completion
    C.Default
    ~tools
    ~dir:(Eio.Stdenv.cwd env)
    (Eio.Stdenv.net env)
    ~inputs:[ user text ]
;;
