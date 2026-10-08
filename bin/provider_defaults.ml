open! Core

let responses_endpoint ~api_url =
  let base =
    Option.value api_url ~default:"https://api.openai.com"
    |> String.chop_suffix_if_exists ~suffix:"/"
  in
  let base =
    if Option.is_some (Uri.scheme (Uri.of_string base)) then base else "https://" ^ base
  in
  base ^ "/v1/responses"
;;

let capabilities =
  Provider_runtime_host.Profile_policy.capabilities
    Public_api
    ~endpoint:(Provider_runtime_host.Profile_policy.endpoint Public_api)
  |> Or_error.ok_exn
;;
