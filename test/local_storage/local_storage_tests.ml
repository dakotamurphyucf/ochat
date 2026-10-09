open! Core
module Storage = Agent_server.Local_storage

let status = function
  | Ok root ->
    Option.value_map root ~default:"transient" ~f:(fun root -> Storage.Root.path root)
  | Error (error : Agent_protocol.Error.t) ->
    Agent_protocol.Error.code_to_string error.code
;;

let%expect_test
    "default durable storage requires HOME and explicit storage is independent"
  =
  let durable =
    Storage.Root.create ~name:"work" ~path:"/selected/store" ()
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    |> Result.ok_or_failwith
  in
  List.iter
    [ Storage.Default, Some "/home/local"
    ; Default, None
    ; Default, Some "relative-home"
    ; Durable durable, None
    ; Transient, None
    ; Transient, Some "relative-home"
    ]
    ~f:(fun (storage, home) ->
      print_endline (status (Storage.durable_root storage ~home)));
  [%expect
    {|
    /home/local/.ochat/agent-store
    invalid_request
    invalid_request
    /selected/store
    transient
    transient
    |}]
;;

let%expect_test "root names cannot redirect filesystem ownership" =
  let accepted = Storage.Root.create ~name:"work.v2-01" ~path:"/selected/store" () in
  let same_path =
    Result.map accepted ~f:Storage.Root.path
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    |> Result.ok_or_failwith
  in
  let invalid =
    List.map
      [ ""; "../foreign"; "two stores"; String.make 129 'a' ]
      ~f:(fun name ->
        Result.is_error (Storage.Root.create ~name ~path:"/selected/store" ()))
  in
  let invalid_paths =
    List.map [ ""; "relative/store"; "/store\000other" ] ~f:(fun path ->
      Result.is_error (Storage.Root.create ~path ()))
  in
  print_s [%sexp ((same_path, invalid, invalid_paths) : string * bool list * bool list)];
  [%expect {| (/selected/store (true true true true) (true true true)) |}]
;;
