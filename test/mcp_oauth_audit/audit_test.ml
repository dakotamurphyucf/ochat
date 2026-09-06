open Core

let () =
  Eio_main.run (fun env ->
    match Array.to_list (Sys.get_argv ()) with
    | [ _; "--peer"; mode ] -> Mcp_peer.run env mode
    | _ ->
      Mirage_crypto_rng_unix.use_default ();
      List.iter
        (Mcp_cases.cases
         @ Oauth_cases.cases
         @ Cache_cases.cases
         @ Http_cases.cases
         @ Teardown_cases.cases)
        ~f:(Fixture.run env))
;;
