open! Core
module P = Agent_protocol
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability

let invalid message = P.Error.invalid_request message

let diagnostics errors =
  List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
  |> String.concat ~sep:"\n"
  |> invalid
;;

let tool ~selected ~policy ~prepare_source ~name ~input ~checkpoint =
  let open Result.Let_syntax in
  let%bind binding =
    C.find selected ~name |> Result.map_error ~f:(fun error -> invalid error.C.message)
  in
  let%bind request =
    B.tool ~capabilities:selected ~reference:(C.reference binding) ~input ~policy
  in
  checkpoint ();
  let%bind artifact = prepare_source ~binding ~request ~checkpoint in
  checkpoint ();
  match artifact with
  | None -> Ok request
  | Some artifact -> B.with_agent_artifact request artifact
;;

let script ~env ~selected ~policy json =
  let open Result.Let_syntax in
  let%bind request =
    Chat_response.One_off_request.decode ~policy json |> Result.map_error ~f:diagnostics
  in
  let%bind prepared =
    Chat_response.One_off_script.prepare_in_domain
      ~env
      ~capabilities:selected
      ~tools:request.tools
      ~source:request.source
      ~limits:request.policy.compilation
      ()
    |> Result.map_error ~f:diagnostics
  in
  B.script ~prepared ~input:request.input ~policy:request.policy
;;
