open Core
open Runner
module I = Agent_protocol.Invocation

type expectation =
  | Value of Jsonaf.t
  | Invalid_input
  | Operation_failure

type case =
  { id : string
  ; input : Jsonaf.t
  ; expected : expectation
  }

let strings xs = `Array (List.map xs ~f:(fun s -> `String s))
let pair before after = `Object [ "before", strings before; "after", strings after ]
let delta added removed = `Object [ "added", strings added; "removed", strings removed ]

let standalone =
  [ { id = "duplicates"
    ; input = pair [ "z"; "a"; "a"; "c" ] [ "b"; "c"; "b"; "d" ]
    ; expected = Value (delta [ "b"; "d" ] [ "a"; "z" ])
    }
  ; { id = "empty"; input = pair [] []; expected = Value (delta [] []) }
  ; { id = "same-set"
    ; input = pair [ "b"; "a" ] [ "a"; "b"; "a" ]
    ; expected = Value (delta [] [])
    }
  ; { id = "add-all"
    ; input = pair [] [ "z"; "c"; "a" ]
    ; expected = Value (delta [ "a"; "c"; "z" ] [])
    }
  ; { id = "invalid"
    ; input = `Object [ "before", `True; "after", strings [] ]
    ; expected = Invalid_input
    }
  ; { id = "unicode-prefixes"
    ; input = pair [] [ "😀"; "é"; "a"; ""; "Ω"; "aa"; "é" ]
    ; expected = Value (delta [ ""; "a"; "aa"; "é"; "Ω"; "😀" ] [])
    }
  ]
;;

let field candidate name = Jsonaf.member_exn name candidate

let assess cases snapshot =
  List.find_map cases ~f:(fun case ->
    let outcome = Execution_host.outcome snapshot case.id in
    match case.expected, outcome with
    | Value expected, I.Complete actual when Jsonaf.exactly_equal actual expected -> None
    | Invalid_input, I.Fail { code = "invocation.invalid_input"; _ } -> None
    | Operation_failure, I.Fail _ -> None
    | _ ->
      let category =
        match outcome with
        | I.Fail { code = "chatml.parse_error"; _ } -> Syntax
        | I.Fail { code = "capability.not_selected"; _ }
        | I.Fail { message = "invocation.unselected_tool"; _ } -> Capability
        | _ -> Semantics
      in
      Some
        (Failed
           ( category
           , "case "
             ^ case.id
             ^ " did not satisfy its runtime contract: "
             ^ Sexp.to_string_hum (I.sexp_of_outcome outcome) )))
  |> Option.value ~default:Passed
;;

let standalone_sources candidate =
  [ ( "agent.chatmd"
    , {|<developer>Execute only the evaluation tool.</developer>
<authoring_context policy="manual"/>
<script id="delta" language="chatml" kind="tool" src="candidate.chatml"/>
<tool name="delta" type="chatml" script="delta" entrypoint="run"
      input_schema="input.json" output_schema="output.json"/>|}
    )
  ; "candidate.chatml", field candidate "source" |> Jsonaf.string_exn
  ; "input.json", field candidate "input_schema" |> Jsonaf.to_string
  ; "output.json", field candidate "output_schema" |> Jsonaf.to_string
  ]
;;

let execute_standalone ~env candidate =
  let snapshot =
    Execution_host.run
      ~env
      ~sources:(standalone_sources candidate)
      ~workspace_files:[]
      ~calls:(List.map standalone ~f:(fun c -> c.id, "delta", c.input))
      ()
  in
  assess standalone snapshot
;;

let balances accounts =
  `Array
    (List.map accounts ~f:(fun (account, balance) ->
       `Object [ "account", `String account; "balance", `Number (Int.to_string balance) ]))
;;

let ledgers =
  [ "a.json", balances [ "z", 3; "a", 5; "a", -2; "b", 0 ] |> Jsonaf.to_string
  ; "b.json", balances [ "z", -3; "c", 4; "a", -3 ] |> Jsonaf.to_string
  ; "broken.json", "this is not JSON"
  ]
;;

let one_off =
  [ { id = "merge"
    ; input = strings [ "a.json"; "b.json" ]
    ; expected = Value (balances [ "c", 4 ])
    }
  ; { id = "reverse"
    ; input = strings [ "b.json"; "a.json" ]
    ; expected = Value (balances [ "c", 4 ])
    }
  ; { id = "one"
    ; input = strings [ "a.json" ]
    ; expected = Value (balances [ "a", 3; "z", 3 ])
    }
  ; { id = "empty"; input = strings []; expected = Value (balances []) }
  ; { id = "missing"; input = strings [ "missing.json" ]; expected = Operation_failure }
  ; { id = "broken"; input = strings [ "broken.json" ]; expected = Operation_failure }
  ; { id = "escape"; input = strings [ "../private.json" ]; expected = Operation_failure }
  ]
;;

let read_declaration =
  {|<tool name="read_file"><read id="ledgers" path="${workspace}"/></tool>|}
;;

let execute_one_off ~env candidate =
  let sources =
    [ ( "agent.chatmd"
      , {|<developer>Run the ledger evaluation.</developer>
<authoring_context policy="manual"/>
<tool name="run_chatml"/>
|}
        ^ read_declaration )
    ; "private.json", "PRIVATE-EVALUATION-SENTINEL"
    ]
  in
  let calls =
    List.map one_off ~f:(fun c ->
      ( c.id
      , "run_chatml"
      , `Object
          [ "source", field candidate "source"
          ; "input", c.input
          ; "tools", field candidate "tools"
          ] ))
  in
  let snapshot = Execution_host.run ~env ~sources ~workspace_files:ledgers ~calls () in
  match
    String.is_substring
      (Agent_protocol.Snapshot.to_json snapshot |> Jsonaf.to_string)
      ~substring:"PRIVATE-EVALUATION-SENTINEL"
  with
  | true -> Failed (Capability, "confined source content appeared in session output")
  | false -> assess one_off snapshot
;;
