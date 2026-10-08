open! Core
module R = Credential_registry
module M = Credential_registry_model
module S = Private_storage
module B = Provider_secret_store

let ok = function
  | Ok value -> value
  | Error _ -> failwith "synthetic registry probe error"
;;

let id value = M.Id.create value |> ok
let secret value = B.Secret.of_bytes (Bytes.of_string value) |> ok

let () =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let arguments = Sys.get_argv () in
      let anchor = Eio.Path.(Eio.Stdenv.fs env / arguments.(1)) in
      let directory =
        S.Directory.open_or_create
          ~sw
          ~anchor
          ~components:[ S.Name.create "private" |> ok ]
        |> ok
      in
      let secrets =
        B.open_private_files
          ~sw
          ~directory
          ~namespace:(B.Namespace.create "synthetic" |> ok)
        |> ok
      in
      let registry =
        R.open_existing
          ~sw
          ~wall_clock:(Eio.Stdenv.clock env)
          ~new_operation:(fun () -> id "child_refresh")
          ~directory
          ~secrets
          ~environment:None
          ~host:(id "synthetic_host")
        |> ok
      in
      let renewal =
        R.Renewal.create ~exchange:(fun ~sw:_ ~identity ~grant _ ->
          Eio.Flow.copy_string "external-rotation-started\n" (Eio.Stdenv.stdout env);
          ignore
            (Eio.Buf_read.line (Eio.Buf_read.of_flow ~max_size:64 (Eio.Stdenv.stdin env))
             : string);
          let expiry =
            Int64.of_float ((Eio.Time.now (Eio.Stdenv.clock env) +. 3600.) *. 1000.)
          in
          let effective = M.Grant.effective grant in
          let grant =
            M.Grant.create
              ~identity
              ~scopes:(Value effective.scopes)
              ~expires_at_ms:(Value expiry)
              ~refresh_policy:Require_rotated
              ~effective:
                { effective with
                  scopes_provenance = Declared
                ; expiry = Known { at_ms = expiry; provenance = Declared }
                }
            |> ok
          in
          let verified =
            R.Verified.create
              ~identity
              ~grant:(Some grant)
              ~material:
                (R.Material.oauth
                   ~continuity:Absent
                   ~access:(secret "synthetic-new-access")
                   ~refresh:(Value (secret "synthetic-new-refresh")))
            |> ok
          in
          R.Renewal.Verified verified)
      in
      match
        R.refresh
          registry
          ~sw
          ~clock:(Eio.Stdenv.mono_clock env)
          ~maximum_wait:(Time_ns.Span.of_sec 1.)
          ~binding:(id "synthetic_binding")
          ~renewal
      with
      | Ok () -> Eio.Flow.copy_string "committed\n" (Eio.Stdenv.stdout env)
      | Error (Model Stale_epoch) ->
        Eio.Flow.copy_string "stale-epoch\n" (Eio.Stdenv.stdout env)
      | Error _ -> failwith "unexpected synthetic refresh outcome"))
;;
