open Core
open Runner
module E = Execution_cases
module H = Execution_host

let count_cases =
  [ E.{ id = "empty"; input = `Array []; expected = Value (`Number "0") }
  ; E.
      { id = "mixed"
      ; input = `Array [ `Null; `False; `String "x" ]
      ; expected = Value (`Number "3")
      }
  ; E.
      { id = "nested"
      ; input = `Array [ `Array [ `Null; `Null ]; `Object [] ]
      ; expected = Value (`Number "2")
      }
  ; E.{ id = "not-array"; input = `Object []; expected = Operation_failure }
  ]
;;

let execute_count ?audit ~env candidate =
  let calls =
    List.map count_cases ~f:(fun case ->
      ( case.id
      , "run_chatml"
      , `Object
          [ "source", E.field candidate "source"
          ; "tools", E.field candidate "tools"
          ; "input", case.input
          ] ))
  in
  H.run
    ?audit
    ~env
    ~sources:
      [ "agent.chatmd", {|<authoring_context policy="manual"/><tool name="run_chatml"/>|}
      ]
    ~workspace_files:[]
    ~calls
    ()
  |> E.assess count_cases
;;

let tally_cases =
  List.map
    [ "first", `Number "5", E.Value (`Number "5")
    ; "negative", `Number "-2", E.Value (`Number "3")
    ; "fraction", `Number "0.5", E.Invalid_input
    ; "zero", `Number "0", E.Value (`Number "3")
    ; "cancel-total", `Number "-3", E.Value (`Number "0")
    ; "next", `Number "7", E.Value (`Number "7")
    ]
    ~f:(fun (id, amount, expected) ->
      E.{ id; input = `Object [ "amount", amount ]; expected })
;;

let execute_tally ?audit ~env candidate =
  match Moderator_cases.binding_validation ~id:"tally" ~name:"tally" ~env candidate with
  | Invalid (kind, message) -> Failed (kind, message)
  | Valid ->
    H.run
      ?audit
      ~sequential:true
      ~env
      ~sources:(Moderator_cases.sources ~id:"tally" candidate)
      ~workspace_files:[]
      ~calls:(List.map tally_cases ~f:(fun case -> case.id, "tally", case.input))
      ()
    |> E.assess tally_cases
;;
