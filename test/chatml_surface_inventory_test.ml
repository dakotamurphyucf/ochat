open Core
open Chatml
module I = Chatml_surface_inventory
module B = Chatml_builtin_spec
module S = Chatml_builtin_surface

let%expect_test
    "authoring inventory preserves distinct compiler surfaces and entrypoint arity"
  =
  let inventories = I.standard () |> Result.ok_or_failwith in
  List.iter inventories ~f:(fun inventory ->
    let selected =
      List.filter_map inventory.items ~f:(fun item ->
        match
          List.mem
            [ "print"
            ; "Process.run"
            ; "Model.call"
            ; "Tool.call"
            ; "Job.start_tool"
            ; "Invocation.resolve"
            ; "Notification.publish"
            ; "Ui.notify"
            ]
            item.name
            ~equal:String.equal
        with
        | true -> Some item.name
        | false -> None)
      |> List.sort ~compare:String.compare
    in
    let entrypoints =
      List.filter_map inventory.items ~f:(fun item ->
        match item.kind with
        | Entrypoint ->
          let arity =
            match item.scheme with
            | B.TFun (args, _) -> Int.to_string (List.length args)
            | _ -> "value"
          in
          Some (item.name ^ ":" ^ arity)
        | _ -> None)
    in
    print_s
      [%sexp
        (inventory.surface_id : string)
      , (selected : string list)
      , (entrypoints : string list)]);
  [%expect
    {|
    (core (print) ())
    (moderator (Model.call Process.run Tool.call print) ())
    (ui_moderator (Model.call Process.run Tool.call Ui.notify print) ())
    (shell_context () ())
    (shell_matcher () ())
    (shell_reviewer () ())
    (shell_before_interceptor () ())
    (shell_after_interceptor () ())
    (shell_effect () ())
    (shell_audit () ())
    (one_off_v1 (Job.start_tool Tool.call) (main:1))
    (tool_v1 (Job.start_tool Tool.call) (run:2))
    (moderator_v1
     (Invocation.resolve Job.start_tool Model.call Notification.publish
      Process.run Tool.call print)
     (initial_state:value on_event:3))
    (delegated_moderator_v1
     (Invocation.resolve Job.start_tool Notification.publish Tool.call)
     (initial_state:value on_event:3))
    |}]
;;

let%expect_test "readable signatures preserve recursive JSON, call arity and row tails" =
  let inventory =
    I.of_surface
      ~surface_id:"reference-fixture"
      ~entrypoints:
        [ ( "inspect"
          , B.TFun
              ( [ B.TRecord (B.TRow_extend ([ "value", B.TVar "a" ], B.TRow_var "fields"))
                ; B.TFun ([], B.TVar "a")
                ]
              , B.TVariant
                  (B.TRow_extend
                     ( [ "Pair", B.TTuple [ B.TVar "a"; B.json_ty ]; "Done", B.TUnit ]
                     , B.TRow_var "cases" )) ) )
        ]
      S.core_surface
    |> Result.ok_or_failwith
  in
  I.reference_items inventory
  |> List.iter ~f:(fun item ->
    let name = Jsonaf.member_exn "name" item |> Jsonaf.string_exn in
    match name with
    | "inspect" | "json" | "Task.bind" ->
      printf "%s: %s\n" name (Jsonaf.member_exn "signature" item |> Jsonaf.string_exn)
    | _ -> ());
  [%expect
    {|
    inspect: ({ value: 'a; ..'fields }, () -> 'a) -> [ `Pair('a, json) | `Done | ..'cases ]
    Task.bind: (task<'a>, ('a) -> task<'b>) -> task<'b>
    json: mu rec0. [ `Null | `Bool(bool) | `Number(float) | `String(string) | `Array(array<rec0>) | `Object(array<{ key: string; value: rec0 }>) ]
    |}]
;;

let%expect_test "inventory ignores declaration ordering but rejects ambiguous namespaces" =
  let snapshot surface = I.of_surface ~surface_id:"fixture" ~entrypoints:[] surface in
  let original = snapshot S.moderator_surface |> Result.ok_or_failwith in
  let reordered : S.surface =
    { globals = List.rev S.moderator_surface.globals
    ; modules =
        List.rev_map S.moderator_surface.modules ~f:(fun (m : B.builtin_module) ->
          { m with exports = List.rev m.exports })
    ; type_aliases = List.rev S.moderator_surface.type_aliases
    }
  in
  assert (
    Jsonaf.exactly_equal
      (I.to_json original)
      (snapshot reordered |> Result.ok_or_failwith |> I.to_json));
  let duplicate = List.hd_exn S.moderator_surface.modules in
  (match
     snapshot
       { S.moderator_surface with modules = duplicate :: S.moderator_surface.modules }
   with
   | Error message -> print_endline message
   | Ok _ -> failwith "ambiguous module inventory accepted");
  let alias = List.hd_exn S.moderator_surface.type_aliases in
  (match
     snapshot
       { S.moderator_surface with
         type_aliases = alias :: S.moderator_surface.type_aliases
       }
   with
   | Error message -> print_endline message
   | Ok _ -> failwith "ambiguous type inventory accepted");
  [%expect
    {|
    duplicate value/module name: String
    duplicate type alias name: json
    |}]
;;
