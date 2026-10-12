open Core
module M = Chatmd_shell_spec.Manifest
module H = Shell_runtime.Chatml_extension
module Spec = Chatmd_shell_spec.Chatmd_script_spec

let checkpoint () = ()
let digest = String.make 64 'a'

let raw script =
  M.create
    { encoding_version = "ochat.shell.manifest.v1"
    ; platform = Macos
    ; runtimes = []
    ; tools = []
    ; moderator_runtime = None
    ; extension_scripts =
        [ { id = script.Spec.id
          ; kind = script.kind
          ; source_sha256 = script.source_sha256
          ; surface_version = "ochat.shell.chatml.surface.v1"
          ; entrypoint = H.default_entrypoint script.kind
          ; limits = script.limits
          }
        ]
    ; dependencies = []
    ; required_features = []
    }
;;

let fact script entrypoint fingerprint =
  M.Prepared_hook.create
    ~id:script.Spec.id
    ~kind:script.kind
    ~entrypoint
    ~program_fingerprint:fingerprint
    ~checkpoint
  |> Or_error.ok_exn
;;

let project manifest hooks =
  M.with_prepared_hooks manifest hooks ~max_bytes:(8 * 1024 * 1024) ~checkpoint
  |> Or_error.ok_exn
;;

let%test_unit
    "execution encoding agrees with independent JSON oracle and preserves legacy bytes"
  =
  let script = Support.script "let initial_state=0" in
  let manifest = raw script in
  let empty = project manifest [] in
  assert (phys_equal empty manifest);
  [%test_eq: string] manifest.canonical_json empty.canonical_json;
  [%test_eq: string] manifest.sha256 (M.resource_sha256 empty);
  List.iter
    [ "plain"
    ; "with\"quote\\newline\n"
    ; String.init 255 ~f:(fun i -> Char.of_int_exn (i + 1))
    ; String.make 10000 'x'
    ]
    ~f:(fun entrypoint ->
      let default = fact script "match_command" digest in
      let alternative = fact script entrypoint (String.make 64 'b') in
      let hooks = [ alternative; default ] in
      let complete = project manifest hooks in
      let sorted =
        List.sort hooks ~compare:(fun a b ->
          String.compare (M.Prepared_hook.entrypoint a) (M.Prepared_hook.entrypoint b))
      in
      let expected =
        `Object
          [ "encoding_version", `String "ochat.shell.execution.v2"
          ; ( "prepared_hooks"
            , `Array
                (List.map sorted ~f:(fun hook ->
                   `Object
                     [ "entrypoint", `String (M.Prepared_hook.entrypoint hook)
                     ; "id", `String (M.Prepared_hook.id hook)
                     ; "kind", Spec.jsonaf_of_kind (M.Prepared_hook.kind hook)
                     ; ( "program_fingerprint"
                       , `String (M.Prepared_hook.program_fingerprint hook) )
                     ])) )
          ; "resource_manifest", Jsonaf.of_string manifest.canonical_json
          ]
        |> Jsonaf.to_string
      in
      [%test_eq: string] expected complete.canonical_json;
      [%test_eq: string] complete.sha256 Digestif.SHA256.(to_hex (digest_string expected));
      [%test_eq: string] complete.sha256 (project manifest (List.rev hooks)).sha256;
      let cap = String.length expected in
      assert (
        Result.is_ok (M.with_prepared_hooks manifest hooks ~max_bytes:cap ~checkpoint));
      assert (
        Result.is_error
          (M.with_prepared_hooks manifest hooks ~max_bytes:(cap - 1) ~checkpoint)))
;;

let%test_unit
    "projection admission and every observed checkpoint fail without a partial identity"
  =
  let script = Support.script "let initial_state=0" in
  let manifest = raw script in
  let default = fact script "match_command" digest in
  let other = fact script "alternative" digest in
  let foreign = fact { script with id = "foreign" } "match_command" digest in
  List.iter
    [ [ other ]; [ default; default ]; [ foreign ] ]
    ~f:(fun hooks ->
      assert (
        Result.is_error
          (M.with_prepared_hooks manifest hooks ~max_bytes:100000 ~checkpoint)));
  let count = ref 0 in
  let poll () = incr count in
  ignore
    (M.with_prepared_hooks manifest [ default; other ] ~max_bytes:100000 ~checkpoint:poll
     |> Or_error.ok_exn
     : M.t);
  assert (!count > 0);
  let total = !count in
  for selected = 1 to total do
    let sentinel = Failure "original metadata callback" in
    let observed = ref 0 in
    let checkpoint () =
      incr observed;
      if !observed = selected then raise sentinel
    in
    match
      M.with_prepared_hooks manifest [ default; other ] ~max_bytes:100000 ~checkpoint
    with
    | _ -> failwith "callback cancellation returned a projected manifest"
    | exception exn -> assert (phys_equal sentinel exn)
  done
;;

let compiled env body seed entrypoint =
  let contract =
    H.compiler_contract Shell_matcher ~entrypoint ~checkpoint |> Support.ok
  in
  H.compile_prepared
    (Support.input
       contract
       [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
       ; "counter.chatml", "let seed=" ^ Int.to_string seed, []
       ])
    ~script:(Support.script body)
    ~entrypoint
    ~limits:Support.compiler_limits
    ~max_dependency_depth:16
    ~env
    ()
  |> Support.ok
;;

let%expect_test
    "same hook body cannot reuse a grant after dependency or selected entrypoint changes"
  =
  Eio_main.run (fun env ->
    let body =
      {|let initial_state = fail("initializer stays inert during inspection")
let match_command ev state = Task.bind(Match.yes(to_string(Counter.seed)),fun ignored -> Task.pure(state))
let selective ev state = Task.bind(Match.no(to_string(Counter.seed)),fun ignored -> Task.pure(state))|}
    in
    let manifest = raw (Support.script body) in
    let left = compiled env body 1 "match_command" in
    let right = compiled env body 2 "match_command" in
    let alternative = compiled env body 1 "selective" in
    let combined =
      H.with_prepared_entrypoints left ~alternatives:[ alternative ] ~checkpoint
      |> Support.ok
    in
    let identity compiled = project manifest (H.prepared_hooks compiled ~checkpoint) in
    let a = identity left
    and b = identity right
    and c = identity combined in
    [%test_eq: string] (H.source_sha256 left) (H.source_sha256 right);
    [%test_eq: string] (M.resource_sha256 a) (M.resource_sha256 b);
    assert (not (String.equal a.sha256 b.sha256));
    assert (not (String.equal a.sha256 c.sha256));
    [%test_eq: int] 2 (List.length (H.prepared_hooks combined ~checkpoint));
    let signature =
      Shell_runtime.Manifest_signature.payload
        ~manifest:c
        ~issuer:"operator"
        ~audience:[ "ochat" ]
        ~issued_at_unix:0L
        ~expires_at_unix:None
    in
    [%test_eq: string] "ochat.shell.execution.v2" signature.encoding_version;
    [%test_eq: string] c.sha256 signature.canonical_manifest_sha256;
    let module Signing = Mirage_crypto_ec.Ed25519 in
    let module Signature = Shell_runtime.Manifest_signature in
    let private_key = Signing.priv_of_octets (String.make 32 'k') |> Support.ok in
    let signed_payload =
      Signature.{ signature with canonical_manifest_sha256 = a.sha256 }
    in
    let signed =
      Signature.
        { key_id = "operator-key"
        ; algorithm = "ed25519"
        ; payload = signed_payload
        ; signature_base64 =
            Signing.sign ~key:private_key (canonical_payload signed_payload)
            |> Base64.encode_exn
        }
    in
    let public_keys =
      Signature.
        [ { key_id = "operator-key"
          ; public_key_base64 =
              Signing.pub_of_priv private_key
              |> Signing.pub_to_octets
              |> Base64.encode_exn
          }
        ]
    in
    let verify manifest =
      Signature.verify ~now_unix:0L ~audience:"ochat" ~public_keys ~manifest signed
    in
    assert (Result.is_ok (verify a));
    assert (Result.is_error (verify b));
    assert (Result.is_error (verify manifest));
    let oldgrant =
      Shell_runtime.Manifest_authorizer.authorize
        Shell_runtime.Manifest_authorizer.assume_authorized
        manifest
      |> Support.ok
    in
    assert (Result.is_error (Shell_runtime.Manifest_authorizer.verify oldgrant c));
    print_s
      [%sexp
        { same_body = (String.equal (H.source_sha256 left) (H.source_sha256 right) : bool)
        ; same_resources =
            (String.equal (M.resource_sha256 a) (M.resource_sha256 b) : bool)
        ; dependency_changes_grant = (not (String.equal a.sha256 b.sha256) : bool)
        ; custom_entrypoints = (List.length (H.prepared_hooks combined ~checkpoint) : int)
        }]);
  [%expect
    {|
    ((same_body true) (same_resources true) (dependency_changes_grant true)
     (custom_entrypoints 2))
    |}]
;;

let%test_unit
    "prepared fact decoders cannot admit malformed digests or unordered duplicate \
     projections"
  =
  let script = Support.script "let initial_state=0" in
  let hook = fact script "match_command" digest in
  let complete = project (raw script) [ hook ] in
  let invalid =
    `Object
      [ "id", `String script.id
      ; "kind", Spec.jsonaf_of_kind script.kind
      ; "entrypoint", `String "match_command"
      ; "program_fingerprint", `String "claimed"
      ]
  in
  assert (Option.is_none (Option.try_with (fun () -> M.Prepared_hook.t_of_jsonaf invalid)));
  let duplicate =
    `Object
      [ ( "prepared_hooks"
        , `Array [ M.Prepared_hook.jsonaf_of_t hook; M.Prepared_hook.jsonaf_of_t hook ] )
      ; "resource_sha256", `String digest
      ]
  in
  assert (
    Option.is_none (Option.try_with (fun () -> M.Authorization.t_of_jsonaf duplicate)));
  let binary = Bin_prot.Utils.bin_dump M.Prepared_hook.bin_writer_t hook in
  let last = Bigstring.length binary - 1 in
  Bigstring.set binary last 'g';
  assert (
    Option.is_none
      (Option.try_with (fun () -> M.Prepared_hook.bin_read_t binary ~pos_ref:(ref 0))));
  let admitted = Option.value_exn complete.authorization in
  [%test_eq: M.Authorization.t]
    admitted
    (M.Authorization.t_of_jsonaf (M.Authorization.jsonaf_of_t admitted));
  [%test_eq: M.Authorization.t]
    admitted
    (M.Authorization.t_of_sexp (M.Authorization.sexp_of_t admitted))
;;
