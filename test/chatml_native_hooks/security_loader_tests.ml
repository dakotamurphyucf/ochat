open Core

exception Loader_cancelled of int ref

let%test_unit "security policy file reads preserve actual caller cancellation" =
  Eio_main.run (fun env ->
    let fs = Eio.Stdenv.cwd env in
    let path = "och235-security-loader.json" in
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(fs / path) "{}";
    Exn.protect
      ~f:(fun () ->
        let loads =
          [ (fun () ->
              ignore
                (Shell_runtime.Manifest_signature.load_signature ~fs ~path
                 : (_, _) Result.t))
          ; (fun () ->
              ignore
                (Shell_runtime.Manifest_signature.load_public_keys ~fs ~path
                 : (_, _) Result.t))
          ; (fun () ->
              ignore (Shell_runtime.Admin_policy_loader.load ~fs ~path : (_, _) Result.t))
          ; (fun () ->
              ignore (Shell_runtime.Trusted_source.load ~fs ~path : (_, _) Result.t))
          ]
        in
        List.iteri loads ~f:(fun i load ->
          let identity = ref i in
          let cancelled = Loader_cancelled identity in
          match
            Eio.Cancel.sub (fun cancel ->
              Eio.Cancel.cancel cancel cancelled;
              load ();
              failwith "cancelled policy read returned an ordinary load result")
          with
          | _ -> assert false
          | exception Eio.Cancel.Cancelled actual -> assert (phys_equal cancelled actual)))
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () -> Eio.Path.unlink Eio.Path.(fs / path))))
;;

let%test_unit "expected missing and malformed policy files retain load diagnostics" =
  Eio_main.run (fun env ->
    let fs = Eio.Stdenv.cwd env in
    let missing = "och235-security-file-must-be-absent.json" in
    let signature_code path =
      match Shell_runtime.Manifest_signature.load_signature ~fs ~path with
      | Error error -> error.code
      | Ok _ -> failwith "invalid signature fixture was admitted"
    in
    [%test_eq: string] "shell.signature_load_failed" (signature_code missing);
    let path = "och235-malformed-security-loader.json" in
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(fs / path) "{";
    Exn.protect
      ~f:(fun () ->
        [%test_eq: string] "shell.signature_load_failed" (signature_code path);
        (match Shell_runtime.Admin_policy_loader.load ~fs ~path with
         | Error error -> [%test_eq: string] "shell.admin_policy_load_failed" error.code
         | Ok _ -> failwith "invalid admin JSON was admitted");
        match Shell_runtime.Trusted_source.load ~fs ~path with
        | Error [ error ] ->
          [%test_eq: string] "shell.trusted_source_load_failed" error.code
        | Error _ | Ok _ -> failwith "invalid trusted source JSON diagnostic changed")
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () -> Eio.Path.unlink Eio.Path.(fs / path))))
;;
