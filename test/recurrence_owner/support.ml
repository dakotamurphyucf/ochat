open! Core
include Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module C = A.Recurrence_catalog
module E = A.Recurrence_entry
module F = A.Recurrence_definition
module Command = A.Recurrence_command
module Lifecycle = P.Session_lifecycle.Revision

let server_id = P.Id.Server.of_string "srv_recurrence_owner" |> protocol_ok
let session = P.Session_ref.create ~server_id ~session_id
let id name = P.Id.Recurrence.of_string ("rec_" ^ name) |> protocol_ok

let source ?(generation = 0) epoch =
  P.Run_source.create
    ~observer:{ script_id = "recurrence-owner"; source_sha256 = String.make 64 'a' }
    ~generation
    ~installation_epoch:epoch
  |> protocol_ok
;;

let capture ?(session = session) ?(source = source 1L) () =
  let target =
    A.Recurrence_target.create_existing
      ~session
      ~source
      ~configuration_revision:0L
      ~input:
        P.Session.Message_content.
          { kind = Plain_text; text = "bounded recurring input"; attachments = [] }
    |> protocol_ok
  in
  let period =
    Recurrence_rules.Rule.Period.create_ms 1000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  Command.Capture.
    { schedule =
        Interval
          { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
          ; first_due = P.Timestamp.to_time_ns timestamp
          }
    ; target
    ; policy = A.Recurrence_policy.default
    }
;;

let request
      catalog
      ?(session = session)
      ?(source = source 1L)
      ?(lifecycle = Lifecycle.zero)
      key
      mutation
  =
  let expected_control_revision =
    Option.map (C.find catalog (Command.Mutation.id mutation)) ~f:E.control_revision
  in
  let needs_source = Command.Mutation.needs_source mutation in
  Command.create
    ~session_id:(P.Session_ref.session_id session)
    ~generation:source.generation
    ~source:(if needs_source then Some source else None)
    ~expected_lifecycle_revision:(if needs_source then Some lifecycle else None)
    ~expected_catalog_revision:(C.revision catalog)
    ~expected_control_revision
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~mutation
  |> protocol_ok
;;

let apply
      catalog
      ?(session = session)
      ?(source = source 1L)
      ?(lifecycle = Lifecycle.zero)
      request
  =
  A.Recurrence_command_transition.prepare
    catalog
    ~request
    ~principal_id
    ~session
    ~generation:source.P.Run_source.generation
    ~current_source:(Some source)
    ~current_lifecycle_revision:(Some lifecycle)
    ~now:timestamp
;;

let committed = function
  | Ok (A.Recurrence_command_transition.Commit { catalog; receipt }) -> catalog, receipt
  | Ok (Retained _) -> failwith "expected fresh recurrence command"
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let fresh catalog ?session ?source ?lifecycle key mutation =
  request catalog ?session ?source ?lifecycle key mutation
  |> apply catalog ?session ?source ?lifecycle
  |> committed
;;

let create catalog ?session ?source ?lifecycle name =
  fresh
    catalog
    ?session
    ?source
    ?lifecycle
    ("create-" ^ name)
    (Create (id name, capture ?session ?source ()))
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected object"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing fixture field " ^ name)
;;

let append json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith "expected object"
;;

let entry catalog name = C.find catalog (id name) |> Option.value_exn
let roundtrip catalog = C.to_jsonaf catalog |> protocol_ok |> C.of_jsonaf |> protocol_ok
