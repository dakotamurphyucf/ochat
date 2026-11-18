open! Core
module P = Agent_protocol
module C = Chat_response.Tool_capability

type t = Chat_response.Moderation.Tool_call.t

module Step = struct
  type t =
    | Rejected
    | Continue of Chat_response.Moderation.Tool_call.t
end

let unavailable code message = P.Error.create code ~message ~retryable:false ()
let reject message = Error (unavailable Permission_denied message)

let rewrite
      (call : t)
      ~(request : Agent_session.Script_tool_calls.preparation)
      ~name
      ~args
  =
  let open Result.Let_syntax in
  let%bind binding =
    C.find request.selected ~name
    |> Result.map_error ~f:(fun e -> unavailable Permission_denied e.C.message)
  in
  let kind =
    if String.equal (C.descriptor binding).type_ "custom"
    then Chat_response.Moderation.Tool_call.Custom
    else Function
  in
  let%bind () =
    match request.owner, request.call.kind, kind with
    | Model_call _, Function, Custom | Model_call _, Custom, Function ->
      reject "delegation.candidate_kind: model tool kind cannot change"
    | _ -> Ok ()
  in
  let%bind schema =
    Chatmd_shell_spec.Tool_schema.compile (C.reference binding).input_schema
    |> Result.map_error ~f:(fun _ ->
      unavailable Permission_denied "invalid delegated target schema")
  in
  let%bind () =
    Chatmd_shell_spec.Tool_schema.validate schema args
    |> Result.map_error ~f:(fun _ ->
      unavailable Permission_denied "invalid delegated target arguments")
  in
  let payload_text =
    match kind, args with
    | Custom, `String text -> text
    | _ -> Jsonaf.to_string args
  in
  Ok { call with name; args; kind; payload_text }
;;

let validate t ~request = rewrite t ~request ~name:t.name ~args:t.args

let apply t ~request ~(decision : P.Moderator_execution.Decision.t) =
  let continue ~name ~args =
    rewrite t ~request ~name ~args |> Result.map ~f:(fun call -> Step.Continue call)
  in
  match decision with
  | Reject _ -> Ok Step.Rejected
  | Approve -> validate t ~request |> Result.map ~f:(fun call -> Step.Continue call)
  | Rewrite_args args -> continue ~name:t.name ~args
  | Redirect (name, args) -> continue ~name ~args
;;

let finish (t : t) ~(original : t) =
  if not (String.equal t.name original.name)
  then Some (Chat_response.Moderation.Tool_moderation.Redirect (t.name, t.args))
  else if Jsonaf.exactly_equal t.args original.args
  then None
  else Some (Chat_response.Moderation.Tool_moderation.Rewrite_args t.args)
;;
