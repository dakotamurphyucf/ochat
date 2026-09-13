open Core
module P = Agent_protocol
module N = Native_tool_invocation
module Channel = Shell_access.Request_channel

type grant =
  { tool_name : string
  ; policy_revision : string
  ; allowed : Session_management.operation list
  ; limits : Channel.limits
  ; authorize : Shell_access.Context.t -> (unit, string) result
  }

let grant ~tool_name ~policy_revision ~allowed ~limits ~authorize =
  let open Result.Let_syntax in
  let%bind _ = Channel.create ~limits ~check:(fun () -> false) ~handle:Fn.id in
  match
    ( String.is_empty (String.strip tool_name)
      || String.is_empty (String.strip policy_revision)
    , allowed )
  with
  | false, _ :: _ -> Ok { tool_name; policy_revision; allowed; limits; authorize }
  | _ ->
    Error
      "session helper grants require a tool name, policy revision and at least one \
       operation"
;;

let policy_fingerprint = function
  | [] -> None
  | grants ->
    let policies =
      List.map grants ~f:(fun grant ->
        [%sexp
          (grant.tool_name : string)
        , (grant.policy_revision : string)
        , (List.map grant.allowed ~f:Session_management.operation_to_string
           |> List.dedup_and_sort ~compare:String.compare
           : string list)
        , (grant.limits.max_request_bytes : int)
        , (grant.limits.max_response_bytes : int)
        , (grant.limits.max_requests : int)]
        |> Sexp.to_string_mach)
      |> List.sort ~compare:String.compare
    in
    Some
      (Chatmd_shell_spec.Source_ref.digest
         ([%sexp ("ochat.session-helper-policy.v1" : string), (policies : string list)]
          |> Sexp.to_string_mach))
;;

let prepare_executor ~grants ~creation ~sessions ~authoring config =
  match grants with
  | [] -> Ok config
  | _ ->
    let open Result.Let_syntax in
    let%bind borrowed =
      N.borrow () |> Result.map_error ~f:(fun _ -> "session helper caller is unavailable")
    in
    let invocation = N.borrowed_invocation borrowed in
    (match
       List.filter grants ~f:(fun grant ->
         String.equal grant.tool_name invocation.context.tool_name)
     with
     | [] -> Ok config
     | _ :: _ :: _ -> Error "ambiguous session helper grants for this tool"
     | [ grant ] ->
       let adapter =
         Session_management.create
           ~borrowed
           ~allowed:grant.allowed
           ~creation
           ~sessions
           ~authoring
       in
       let final_context = ref None in
       let authorize context =
         let%map () = grant.authorize context in
         final_context := Some context
       in
       let check () =
         Option.exists !final_context ~f:(fun context ->
           (* A host authorizer may yield. Check the expiring invocation after
              that callback, including on the final response-disclosure path. *)
           match grant.authorize context with
           | Error _ -> false
           | Ok () -> Result.is_ok (N.borrowed_capabilities borrowed))
       in
       let handle source =
         let outcome =
           match Jsonaf.of_string source with
           | json -> Session_management.dispatch adapter json
           | exception _ ->
             P.Invocation.Fail
               { code = "agent.bridge.invalid_request"
               ; message = "Expected a JSON session-management envelope."
               ; retryable = false
               ; details = `Null
               }
         in
         P.Invocation.outcome_to_json outcome |> Jsonaf.to_string
       in
       let%bind channel = Channel.create ~limits:grant.limits ~check ~handle in
       Shell_access.Executor.with_request_channel config ~channel ~authorize)
;;
