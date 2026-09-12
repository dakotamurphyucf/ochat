open Core
open Runner
module CM = Prompt.Chat_markdown
module V = Chat_response.Authoring_validation
module Spec = Chatmd_shell_spec.Extension_spec

let root ?(id = "quota") binding =
  "<developer>Execute the moderator evaluation.</developer>\n"
  ^ "<authoring_context policy=\"manual\"/>\n<script id=\""
  ^ id
  ^ "\" language=\"chatml\" kind=\"moderator\" api=\"extensibility-v1\" \
     src=\"candidate.chatml\"/>\n"
  ^ binding
;;

let field = Execution_cases.field

let sources ?id candidate =
  [ "agent.chatmd", root ?id (field candidate "binding" |> Jsonaf.string_exn)
  ; "candidate.chatml", field candidate "source" |> Jsonaf.string_exn
  ; "input.json", field candidate "input_schema" |> Jsonaf.to_string
  ; "output.json", field candidate "output_schema" |> Jsonaf.to_string
  ]
;;

(* This is a harness candidate envelope, not another shape for ochat_validate.
   Parse captured bytes without preprocessing, then admit only the requested
   ghost binding. Candidate tags can never install native/file/shell authority. *)
let binding_validation ?(id = "quota") ?(name = "reserve") ~env candidate =
  let fields = [ "source"; "binding"; "input_schema"; "output_schema" ] in
  match candidate with
  | `Object entries
    when List.equal
           String.equal
           (List.sort fields ~compare:String.compare)
           (List.map entries ~f:fst |> List.sort ~compare:String.compare) ->
    (match
       let bundle =
         Chatmd_source_bundle.create
           ~root_file:"agent.chatmd"
           ~sources:(sources ~id candidate)
           ()
         |> Result.ok_or_failwith
       in
       CM.parse_source_bundle ~dir:(Eio.Stdenv.cwd env) bundle
     with
     | parsed ->
       (match parsed.root with
        | [ CM.Developer _
          ; Authoring_context _
          ; Extension_script _
          ; Tool (Extension tool)
          ] ->
          (match tool.implementation, tool.uses, tool.completion_schema with
           | Spec.Moderator owner, [], None
             when String.equal owner id && String.equal tool.name name -> Valid
           | _ ->
             Invalid
               ( Capability
               , "binding must be synchronous "
                 ^ name
                 ^ " owned by "
                 ^ id
                 ^ " with no dependencies" ))
        | _ ->
          Invalid
            (Capability, "only the " ^ name ^ " moderator tool binding may be supplied"))
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn -> Invalid (Semantics, Exn.to_string exn))
  | _ ->
    Invalid (Semantics, "expected exactly source, binding, input_schema and output_schema")
;;

let validate ?id ?name ~env ~host ~capabilities candidate =
  match binding_validation ?id ?name ~env candidate with
  | Invalid _ as failure -> failure
  | Valid ->
    V.validate
      ~env
      ~host
      ~capabilities
      (`Object
          [ "version", `Number "1"
          ; "target", `String "moderator"
          ; "source", field candidate "source"
          ; "tools", `Array []
          ])
    |> Reference_backend.classification
;;

type expected =
  | Remaining of int
  | Rejected
  | Invalid_input

type case =
  { id : string
  ; amount : Jsonaf.t
  ; expected : expected
  }

let cases =
  [ { id = "first"; amount = `Number "4"; expected = Remaining 7 }
  ; { id = "over-budget"; amount = `Number "8"; expected = Rejected }
  ; { id = "zero"; amount = `Number "0"; expected = Rejected }
  ; { id = "negative"; amount = `Number "-3"; expected = Rejected }
  ; { id = "fraction"; amount = `Number "0.5"; expected = Invalid_input }
  ; { id = "wrong-type"; amount = `String "7"; expected = Invalid_input }
  ; { id = "exact"; amount = `Number "7"; expected = Remaining 0 }
  ; { id = "exhausted"; amount = `Number "1"; expected = Rejected }
  ]
;;

let execute ~env candidate =
  match binding_validation ~env candidate with
  | Invalid (kind, message) -> Failed (kind, message)
  | Valid ->
    let snapshot =
      Execution_host.run
        ~sequential:true
        ~env
        ~sources:(sources candidate)
        ~workspace_files:[]
        ~calls:
          (List.map cases ~f:(fun c -> c.id, "reserve", `Object [ "amount", c.amount ]))
        ()
    in
    List.find_map cases ~f:(fun c ->
      let outcome = Execution_host.outcome snapshot c.id in
      let correct =
        match c.expected, outcome with
        | Remaining expected, Complete (`Object [ ("remaining", `Number amount) ]) ->
          Float.equal (Float.of_string amount) (Float.of_int expected)
        | Rejected, Fail { code = "quota.rejected"; _ } -> true
        | Invalid_input, Fail { code = "invocation.invalid_input"; _ } -> true
        | _ -> false
      in
      match correct with
      | true -> None
      | false ->
        Some
          (Failed
             ( Semantics
             , "quota case "
               ^ c.id
               ^ " failed: "
               ^ Sexp.to_string_hum (Agent_protocol.Invocation.sexp_of_outcome outcome) )))
    |> Option.value ~default:Passed
;;
