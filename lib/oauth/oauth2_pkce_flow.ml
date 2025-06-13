open Core
open Eio
module T = Oauth2_types

module Result = struct
  include Result

  module Let_syntax = struct
    let ( let* ) res f = bind res ~f
  end
end

let open_browser ~env (url : string) : unit =
  (* best-effort – on macOS/Linux try xdg-open/open, ignore failures *)
  (* In CI or test environments we must not launch an external browser – it
     would block the test runner waiting for user interaction.  Detect
     non-interactive mode via the conventional [CI] or
     [OAUTH_NO_BROWSER] environment variables. *)
  let no_browser =
    Sys.getenv "CI" |> Option.is_some || Sys.getenv "OAUTH_NO_BROWSER" |> Option.is_some
  in
  if no_browser
  then Logs.warn (fun f -> f "[oauth2] Skipping browser launch for PKCE flow: %s" url)
  else (
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let cmd =
      if Sys.win32
      then [ "start"; Printf.sprintf "'%s'" url ]
      else [ "xdg-open"; Printf.sprintf "'%s'" url; "&>/dev/null" ]
    in
    try Eio.Process.run proc_mgr cmd with
    | Eio.Io _ as e ->
      (* fallback to open command on macOS *)
      (match Sys.unix with
       | true -> Eio.Process.run proc_mgr [ "open"; Printf.sprintf "%s" url ]
       | false -> raise e))
;;

let run ~env ~sw ~(meta : T.Metadata.t) ~(client_id : string) =
  let verifier = Oauth2_pkce.gen_code_verifier () in
  let challenge = Oauth2_pkce.challenge_of_verifier verifier in
  (* Deterministically pick a port in [8800, 9799] using cryptographically
     secure randomness.  This avoids the use of [Random.self_init], which is
     forbidden inside inline tests (it raises to keep the test output
     deterministic). *)
  let port = 8876 in
  let redirect = Printf.sprintf "http://127.0.0.1:%d/cb" port in
  (* promise for code *)
  let code_p, code_rf = Promise.create () in
  Fiber.fork ~sw (fun () ->
    let sock =
      Eio.Net.listen
        ~sw
        ~reuse_addr:true
        ~reuse_port:true
        ~backlog:1
        env#net
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    (* run connection handling; we ignore errors and let the listener be
         garbage-collected when the fibre terminates. *)
    (fun () ->
       let flow, _ = Eio.Net.accept ~sw sock in
       let req = Eio.Flow.read_all flow in
       (* if true then failwith "Mcp_transport_http.connect: store nots supported"; *)
       (match String.lsplit2 req ~on:'?' with
        | Some (_, rest) ->
          let query = String.prefix rest (String.index_exn rest ' ') in
          let params = Uri.query_of_encoded query in
          (match List.Assoc.find params ~equal:String.equal "code" with
           | Some [ code ] -> Promise.resolve code_rf code
           | _ -> ())
        | None -> ());
       ignore
         (Eio.Flow.write
            flow
            [ Cstruct.of_string "HTTP/1.1 200 OK\r\n\r\nYou may close this tab" ]);
       Eio.Flow.close flow)
      ());
  let auth_uri =
    Uri.add_query_params'
      (Uri.of_string meta.authorization_endpoint)
      [ "response_type", "code"
      ; "client_id", client_id
      ; "redirect_uri", redirect
      ; "code_challenge", challenge
      ; "code_challenge_method", "S256"
      ]
  in
  open_browser ~env (Uri.to_string auth_uri);
  (* if true then failwith "Mcp_transport_http.connect: store nots supported"; *)
  (* wait for code *)
  let code = Promise.await code_p in
  code, verifier, redirect
;;

let exchange_token
      ~env
      ~sw
      ~(meta : T.Metadata.t)
      ~(client_id : string)
      ~(code : string)
      ~(code_verifier : string)
      ~(redirect_uri : string)
  : (T.Token.t, string) Result.t
  =
  let open Result.Let_syntax in
  let params =
    [ "grant_type", "authorization_code"
    ; "code", code
    ; "client_id", client_id
    ; "code_verifier", code_verifier
    ; "redirect_uri", redirect_uri
    ]
  in
  let* json = Oauth2_http.post_form ~env ~sw meta.token_endpoint params in
  Ok
    T.Token.
      { (T.Token.t_of_jsonaf json) with
        obtained_at = Eio.Time.now (Eio.Stdenv.clock env)
      }
;;
