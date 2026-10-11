open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module R = Chatml_host_runtime

let context = L.VRecord (String.Map.singleton "phase" (L.VString "turn_start"))

let session ~surface ?(handlers = R.default_handlers) source =
  let compiled = R.compile_script ~surface ~source () |> Result.ok_or_failwith in
  R.instantiate_session
    (R.default_runtime_config ~surface ~handlers ())
    compiled
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
  |> Result.ok_or_failwith
;;

let%expect_test "array traversal resumes after a suspended callback without replay" =
  let session =
    session
      ~surface:Chatml.Chatml_builtin_surface.ui_moderator_surface
      {|let initial_state = [""]
let calls = ref(0)
let on_event ctx state event =
  Task.map_array([1, 2, 3], fun n ->
    let ignored = calls := !calls + 1 in
    if n == 2 then Approval.ask_text("continue?")
    else Task.pure(to_string(n) ++ ":" ++ to_string(!calls)))|}
  in
  R.handle_event session ~context ~event:L.VUnit |> Result.ok_or_failwith;
  [%test_eq: bool] true (Option.is_some (R.pending_ui_request session));
  print_endline (B.value_to_string (R.current_state session));
  R.resume_ui_request session ~response:"yes" |> Result.ok_or_failwith;
  [%test_eq: bool] false (Option.is_some (R.pending_ui_request session));
  print_endline (B.value_to_string (R.current_state session));
  [%expect
    {|
    [||]
    [|1:1, yes, 3:3|]
    |}]
;;

let%expect_test
    "catch rolls back staged traversal effects but not completed external calls"
  =
  let external_calls = ref [] in
  let handlers =
    { R.default_handlers with
      on_tool_call =
        (fun _ ~name ~args:_ ->
          external_calls := name :: !external_calls;
          Ok (L.VVariant ("Ok", [ L.VVariant ("Null", []) ])))
    }
  in
  let session =
    session
      ~surface:Chatml.Chatml_builtin_surface.moderator_surface
      ~handlers
      {|let initial_state = 0
let on_event ctx state event =
  let* ignored = Task.catch(
    Task.iter_array([1, 2, 3], fun n ->
      let* ignored = Tool.call(to_string(n), `Null) in
      let* ignored = Turn.prepend_system(to_string(n)) in
      if n == 2 then Task.fail("stop") else Task.pure(())),
    fun error -> Turn.prepend_system("recovered")) in
  Task.pure(state + 1)|}
  in
  R.handle_event session ~context ~event:L.VUnit |> Result.ok_or_failwith;
  print_s [%sexp (List.rev !external_calls : string list)];
  let effects =
    R.committed_local_effects session
    |> List.map ~f:(fun (operation : L.eff) ->
      operation.op, List.map operation.args ~f:B.value_to_string)
  in
  print_s [%sexp (effects : (string * string list) list)];
  print_endline (B.value_to_string (R.current_state session));
  [%expect
    {|
    (1 2)
    ((Turn.prepend_system (recovered)))
    1
    |}]
;;
