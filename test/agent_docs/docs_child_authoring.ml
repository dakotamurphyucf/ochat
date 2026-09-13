open! Core
module V = Chat_response.Authoring_validation
module C = Chat_response.Tool_capability

let guide = "guide/chatml-authoring-children.md"
let fixture = "test/chatml_extensibility_fixtures/x05-child-session/create.json"

let fixture_capabilities () =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "Non-executing documentation capability"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun _ -> failwith "child documentation validation invoked a tool")
  in
  C.create
    ~owner:"child-documentation"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "no-documentation-io")
    [ Chatmd_shell_spec.Source_ref.digest "documentation-reader-v1", implementation ]
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let run env root =
  let load path = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / path) in
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Authoring_corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let text = load ("docs-src/" ^ guide) in
  Docs_chatml_authoring.check_topic_coverage corpus ~path:guide ~text;
  let request_text = load fixture in
  let displayed = "```json\n" ^ String.rstrip request_text ^ "\n```" in
  (match
     String.is_substring text ~substring:displayed
     && List.count (String.split_lines text) ~f:(String.is_prefix ~prefix:"```") = 2
   with
   | true -> ()
   | false -> failwith "child authoring guide must display its exact checked request");
  let request = Jsonaf.of_string request_text in
  let decoded =
    Agent_session.Generated_session_request.decode
      ~limits:Chatmd_source_bundle.default_limits
      request
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Invocation.message)
    |> Result.ok_or_failwith
  in
  (* A named capability is needed for inherited-reference admission. This runner
     is deliberately unusable: documentation checks cannot read a user's files. *)
  let capabilities = fixture_capabilities () in
  let host =
    V.create_host
      ~runtime_identity:"child-documentation-check-v1"
      ~targets:[ Generated_chatmd ]
      ~moderator_surface:Delegated
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let validation_request =
    `Object
      [ "version", `Number "1"
      ; "target", `String "generated_chatmd"
      ; "root_file", `String (Chatmd_source_bundle.root_file decoded.bundle)
      ; ( "sources"
        , `Array
            (List.map
               (Chatmd_source_bundle.sources decoded.bundle)
               ~f:(fun (path, text) ->
                 `Object [ "path", `String path; "text", `String text ])) )
      ; "tools", `Array (List.map decoded.tools ~f:(fun name -> `String name))
      ]
  in
  let report = V.validate ~env ~host ~capabilities validation_request in
  (match V.valid report with
   | true -> ()
   | false ->
     failwith ("invalid child authoring example: " ^ Jsonaf.to_string (V.to_json report)));
  Eio.Flow.copy_string
    "Child authoring reference: captured request, inherited imports and delegated \
     moderator static checks PASS\n"
    (Eio.Stdenv.stdout env)
;;
