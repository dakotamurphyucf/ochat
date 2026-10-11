open! Core
module P = Agent_protocol
module D = P.Prompt_dependency_selection

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let json = Jsonaf.of_string
let generation value = D.Generation.of_int value |> checked
let definition value = P.Id.Prompt_definition.of_string value |> checked
let revision value = P.Id.Prompt_revision.of_string value |> checked

let request_json =
  json
    {|{"definitions":["prd_b","prd_a"],"expected_generation":7,"idempotency_key":"update:original"}|}
;;

let request () = D.Update_request.of_json request_json |> checked

let result_json =
  json
    {|{"previous_generation":7,"generation":8,"targets":[{"definition_id":"prd_b","outcome":{"state":"unavailable","reason":"source unavailable"}},{"definition_id":"prd_a","outcome":{"state":"ready","revision":"prv_a"}}]}|}
;;

let result () = D.Update_result.of_json result_json |> checked

let public value =
  P.Public.Result.Non_history.of_internal value
  |> Result.map ~f:(fun value -> P.Public.Result.Non_history value)
;;

let initialization () =
  P.Initialize.Response.create
    ~protocol_name:"ochat.agent"
    ~selected_version:P.Version.current
    ~implementation:
      (P.Initialize.Implementation.create ~name:"dependencies-fixture" ~version:"1"
       |> checked)
    ~server_id:(P.Id.Server.of_string "srv_dependencies" |> checked)
    ~enabled_features:[]
    ~extensions:None
    ~principal:
      (P.Principal.create
         ~id:(P.Id.Principal.of_string "pri_dependencies" |> checked)
         ~authentication_kind:"test"
         ~scopes:(P.Scope.Set.of_list [ Administer_configuration ])
         ~attributes:[]
       |> checked)
    ~limits:
      { max_request_bytes = 1_048_576
      ; max_event_bytes = 1_048_576
      ; max_page_size = 1000
      ; max_attachments_per_connection = 8
      }
    ~event_retention:
      { minimum_age_ms = 1000; maximum_events = 1000; oldest_replayable_sequence = None }
    ~timing:
      { heartbeat_interval_ms = 1000
      ; owner_lease_duration_ms = 60_000
      ; owner_renew_after_ms = 30_000
      ; disconnect_grace_default_ms = 1000
      }
    ~server_time:(P.Timestamp.of_string "2026-10-11T12:00:00Z" |> checked)
  |> checked
;;

let initialize connection =
  Agent_client.Session_handle.initialize
    connection
    ~implementation_name:"dependencies-client"
    ~implementation_version:"1"
  |> checked
  |> ignore
;;
