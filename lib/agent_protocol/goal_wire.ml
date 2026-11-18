open! Core
module J = Json_codec

let invalid message = Error (Protocol_error.invalid_request message)
let counter_json n = `String (Int64.to_string n)
let generation_json n = counter_json (Int64.of_int n)
let bool_json value = if value then `True else `False
let option_json f value = Option.value_map value ~default:`Null ~f

let nullable decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let counter = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal (Int64.to_string value) encoded
       -> Ok value
     | Some _ | None -> invalid "invalid Goal decimal counter")
  | _ -> invalid "Goal counters must be decimal strings"
;;

let generation json =
  let%bind.Result value = counter json in
  Result.of_option
    (Int64.to_int value)
    ~error:(Protocol_error.invalid_request "Goal generation exceeds host range")
;;

let fields json names =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%map () = Extension_codec.closed fields names in
  fields
;;

let session json =
  let%bind.Result _ = fields json [ "server_id"; "session_id" ] in
  Session_ref.of_json json
;;

let source json =
  let%bind.Result _ =
    fields json [ "script_id"; "source_sha256"; "generation"; "installation_epoch" ]
  in
  Run_source.of_json json
;;

let tagged kind fields = `Object (("kind", `String kind) :: fields)

let tag json =
  let%bind.Result fields = J.fields json in
  let%map.Result kind = J.required_as fields "kind" J.string in
  kind, fields
;;

(* The persistence codecs preserve compatibility with stored Goal documents.
   Public wire shapes also require that every nested member is recognized. *)
let budget json =
  let open Result.Let_syntax in
  let%bind members =
    fields json [ "admitted_turns"; "admitted_attempts"; "observed_tokens"; "deadline" ]
  in
  let%bind _ =
    J.required_as
      members
      "observed_tokens"
      (nullable (fun json -> fields json [ "component"; "ceiling" ]))
  in
  Goal_budget.of_json json
;;

let hold json =
  let open Result.Let_syntax in
  let%bind kind, members = tag json in
  let extra =
    match kind with
    | "needs_input" -> [ "reason" ]
    | "budget_gate" -> [ "dimension" ]
    | "usage_uncertain" -> [ "component" ]
    | _ -> []
  in
  let%bind () = Extension_codec.closed members ("kind" :: extra) in
  Goal.Hold.of_json json
;;
