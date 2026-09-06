open! Core

let register env =
  let tool =
    Webpage_markdown.Tool.register
      ~env
      ~dir:(Eio.Stdenv.cwd env)
      ~net:(Eio.Stdenv.net env)
  in
  let metadata, dispatch = Ochat_function.functions [ tool ] in
  let run = Hashtbl.find_exn dispatch "webpage_to_markdown" in
  let fetch url =
    let arguments = Jsonaf.to_string (`Object [ "url", `String url ]) in
    run ~invocation:Ochat_function.Invocation.silent arguments
  in
  metadata, fetch
;;

let fetch_text env url =
  Webpage_markdown.Driver.fetch_and_convert ~env ~net:(Eio.Stdenv.net env) url
  |> Webpage_markdown.Driver.Markdown.to_string
;;
