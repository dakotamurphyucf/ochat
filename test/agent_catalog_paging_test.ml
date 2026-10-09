open! Core
module P = Agent_protocol
module C = Agent_client

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let cursor name = P.Page.Cursor.of_string name |> checked
let page = P.Page.Request.create ~limit:1 () |> checked

let public result =
  P.Public.Result.Non_history.of_internal result
  |> Result.map ~f:(fun value -> P.Public.Result.Non_history value)
;;

let prompt name =
  P.Prompt.
    { id = P.Id.Prompt_definition.of_string ("prd_" ^ name) |> checked
    ; name
    ; description = None
    ; enabled = true
    ; availability = Available
    ; current_revision = None
    ; allowed_workspaces = []
    ; permission_profile = "default"
    ; runtime_policy = None
    }
;;

let workspace name =
  P.Workspace.
    { id = P.Id.Workspace_definition.of_string ("wsd_" ^ name) |> checked
    ; name
    ; kind = Physical
    ; temporary_location = None
    ; cleanup = None
    ; access = Read_only
    ; conflict_domain = None
    ; prompt_limits = []
    ; availability = Available
    }
;;

let connection ~request =
  C.Transport.create
    ~request
    ~next_notification:(fun () -> failwith "catalog must not drain notifications")
    ~close:ignore
  |> C.Connection.create
;;

let%expect_test
    "filtered prompt traversal follows empty partial pages without changing filters"
  =
  Eio_main.run (fun _ ->
    let calls = ref 0 in
    let connection =
      connection ~request:(function
        | P.Command.Prompt_list request ->
          incr calls;
          assert (Option.equal Bool.equal request.enabled (Some true));
          assert (Option.equal Bool.equal request.available (Some false));
          assert (Int.equal request.page.limit 1);
          let items, next_cursor =
            match Option.map request.page.cursor ~f:P.Page.Cursor.to_string with
            | None -> [], Some (cursor "second")
            | Some "second" -> [ prompt "middle" ], Some (cursor "third")
            | Some "third" -> [ prompt "last" ], None
            | Some _ -> assert false
          in
          public (Prompt_list { items; next_cursor })
        | _ -> assert false)
    in
    let query =
      P.Prompt.List_request.{ page; enabled = Some true; available = Some false }
    in
    let values =
      C.Catalog.enumerate_prompts connection ~query ~max_prompts:2 ~max_pages:3 |> checked
    in
    print_s
      [%sexp
        (List.map values ~f:(fun value -> value.P.Prompt.name) : string list)
      , (!calls : int)];
    C.Connection.close connection);
  [%expect {| ((middle last) 3) |}]
;;

let%expect_test "workspace name resolution sees later pages and cross-page ambiguity" =
  Eio_main.run (fun _ ->
    let resolve duplicate =
      let connection =
        connection ~request:(function
          | P.Command.Workspace_list request ->
            let items, next_cursor =
              match request.page.cursor with
              | None ->
                ( [ workspace (if duplicate then "target" else "first") ]
                , Some (cursor "later") )
              | Some _ -> [ workspace "target" ], None
            in
            public (Workspace_list { items; next_cursor })
          | _ -> assert false)
      in
      let result = C.Catalog.resolve_workspace connection ~name:"target" in
      C.Connection.close connection;
      result
    in
    let found = resolve false |> checked in
    let ambiguous = Result.is_error (resolve true) in
    print_s [%sexp (found.P.Workspace.name : string), (ambiguous : bool)]);
  [%expect {| (target true) |}]
;;

let%expect_test "enumeration bounds and failures never return a successful prefix" =
  let calls = ref 0 in
  let read _ =
    incr calls;
    Ok P.Page.{ items = [ 1 ]; next_cursor = Some (cursor "same") }
  in
  let collect ?(initial = page) ~max_items ~max_pages () =
    C.Page_enumeration.collect initial ~max_items ~max_pages ~read |> Result.is_error
  in
  let invalid = collect ~max_items:0 ~max_pages:1 () in
  assert (Int.equal !calls 0);
  let continued =
    collect
      ~initial:{ page with cursor = Some (cursor "old") }
      ~max_items:2
      ~max_pages:2
      ()
  in
  assert (Int.equal !calls 0);
  let pages = collect ~max_items:10 ~max_pages:1 () in
  let items = collect ~max_items:1 ~max_pages:10 () in
  let repeated = collect ~max_items:10 ~max_pages:10 () in
  let failure = P.Error.create Conflict ~message:"catalog changed" ~retryable:false () in
  let result =
    C.Page_enumeration.collect page ~max_items:2 ~max_pages:2 ~read:(fun request ->
      match request.P.Page.Request.cursor with
      | None -> Ok P.Page.{ items = [ 1 ]; next_cursor = Some (cursor "next") }
      | Some _ -> Error failure)
  in
  let same_failure =
    match result with
    | Error error ->
      P.Error.equal_code error.code Conflict && String.equal error.message failure.message
    | Ok _ -> false
  in
  print_s
    [%sexp
      (invalid : bool)
    , (continued : bool)
    , (pages : bool)
    , (items : bool)
    , (repeated : bool)
    , (same_failure : bool)];
  [%expect {| (true true true true true true) |}]
;;
