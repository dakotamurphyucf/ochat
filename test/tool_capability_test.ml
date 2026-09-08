open Core
module C = Chat_response.Tool_capability

let digest = Chatmd_shell_spec.Source_ref.digest

let get = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : C.error)]
;;

let text = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | _ -> assert false
;;

let tool name calls =
  let module Definition = struct
    type input = string

    let name = name
    let description = Some "fixture"
    let type_ = "function"
    let parameters = `Object [ "type", `String "object" ]
    let input_of_string input = input
  end
  in
  Ochat_function.create_function
    (module Definition)
    (fun input ->
       incr calls;
       Openai.Responses.Tool_output.Output.Text input)
;;

let registry ?(owner = "owner") ?(resource = "root=allowed") pairs =
  C.create
    ~owner
    ~resource_fingerprint:(digest resource)
    (List.map pairs ~f:(fun implementation ->
       digest "fixture implementation v1", implementation))
  |> get
;;

let%test_unit "selection retains actual registered implementations and only narrows" =
  Mirage_crypto_rng_unix.use_default ();
  let calls = ref 0 in
  let original = tool "selected" calls in
  let other = tool "unselected" calls in
  let all = registry [ original; other ] in
  let selected = C.select all ~names:[ "selected" ] |> get in
  let binding = C.find selected ~name:"selected" |> get in
  assert (phys_equal (C.implementation binding) original);
  let reference = C.reference binding in
  assert (
    phys_equal
      (get (C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint))
      binding);
  assert (Result.is_error (C.find selected ~name:"unselected"));
  assert (Result.is_error (C.select selected ~names:[ "unselected" ]));
  assert (Result.is_error (C.select all ~names:[ "selected"; "selected" ]));
  let empty = C.select selected ~names:[] |> get in
  assert (List.is_empty (C.references empty));
  assert (
    Result.is_error (C.resolve empty ~id:reference.id ~fingerprint:reference.fingerprint));
  assert (!calls = 0);
  assert (String.equal ((C.implementation binding).run "test" |> text) "test");
  assert (!calls = 1);
  assert (
    String.equal
      ((C.implementation binding).run_with_progress
         ~invocation:Ochat_function.Invocation.silent
         "observed"
       |> text)
      "observed");
  assert (!calls = 2)
;;

let%test_unit "foreign reconfigured and stale capability references never rebind by name" =
  Mirage_crypto_rng_unix.use_default ();
  let calls = ref 0 in
  let first = registry [ tool "shared" calls; tool "extra" calls ] in
  let binding = C.find first ~name:"shared" |> get in
  let reference = C.reference binding in
  List.iter
    [ registry ~owner:"different-owner" [ tool "shared" calls ]
    ; registry ~resource:"root=broader" [ tool "shared" calls ]
    ; registry [ tool "shared" calls ]
    ]
    ~f:(fun replacement ->
      assert (
        Result.is_error
          (C.resolve replacement ~id:reference.id ~fingerprint:reference.fingerprint)));
  assert (
    Result.is_error (C.resolve first ~id:reference.id ~fingerprint:(digest "forged")));
  let a = C.select first ~names:[ "extra"; "shared" ] |> get in
  let b = C.select first ~names:[ "shared"; "extra" ] |> get in
  assert (String.equal (C.fingerprint a) (C.fingerprint b));
  assert (
    not
      (String.equal
         (C.fingerprint a)
         (C.fingerprint (C.select a ~names:[ "shared" ] |> get))));
  assert (!calls = 0)
;;

let%test_unit
    "registration rejects ambiguous names and invalid metadata without execution"
  =
  Mirage_crypto_rng_unix.use_default ();
  let calls = ref 0 in
  let implementation = tool "name" calls in
  let make registrations =
    C.create ~owner:"owner" ~resource_fingerprint:(digest "resources") registrations
  in
  assert (
    Result.is_error (make [ digest "a", implementation; digest "b", implementation ]));
  assert (Result.is_error (make [ "not-a-digest", implementation ]));
  let bad_schema =
    { implementation with
      info =
        { implementation.info with
          function_ =
            { implementation.info.function_ with
              parameters = `Object [ "duplicate", `True; "duplicate", `False ]
            }
        }
    }
  in
  assert (Result.is_error (make [ digest "a", bad_schema ]));
  assert (!calls = 0)
;;
