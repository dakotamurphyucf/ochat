open Core

(* Separate from the existing logging client: evaluation requests contain private
   held-out source. Authenticate TLS using the system trust store and never log
   credentials, requests, provider error bodies or transport exceptions. *)
let with_transport ~env ~api_key f =
  (match String.is_empty api_key with
   | true -> invalid_arg "OPENAI_API_KEY must be set for an authorized evaluation"
   | false -> ());
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok authenticator -> authenticator
    | Error _ -> failwith "could not load the system TLS trust store"
  in
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Ok config -> config
    | Error _ -> failwith "could not configure provider TLS"
  in
  let https uri raw =
    let host =
      Uri.host uri
      |> Option.value_exn
      |> Domain_name.of_string_exn
      |> Domain_name.host_exn
    in
    Tls_eio.client_of_flow ~host tls_config raw
  in
  let client = Cohttp_eio.Client.make ~https:(Some https) (Eio.Stdenv.net env) in
  let endpoint = Uri.of_string "https://api.openai.com/v1/responses" in
  let headers =
    Http.Header.of_list
      [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json" ]
  in
  let post request =
    Eio.Switch.run (fun sw ->
      let response, body =
        Cohttp_eio.Client.post
          ~sw
          ~headers
          ~body:(Cohttp_eio.Body.of_string (Jsonaf.to_string request))
          client
          endpoint
      in
      Responses_provider.read_response ~status:(Http.Status.to_int response.status) body)
  in
  f post
;;
