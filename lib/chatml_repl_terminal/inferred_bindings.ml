open! Core
module P = Agent_protocol
module W = P.Developer_repl_wire
module Client = Agent_client.Developer_repl_connection

type entry =
  { statement_index : int
  ; binding_index : int
  ; inspection : W.Type_inspection.t
  }

type t =
  { entries : entry list
  ; limited : bool
  ; error : P.Error.t option
  }

let empty = { entries = []; limited = false; error = None }
let entries t = t.entries
let limited t = t.limited
let error t = t.error
let max_bindings = 16
let max_bytes = 4_096

let selections submission =
  let rec statements statement_index remaining acc = function
    | [] -> List.rev acc, false
    | summary :: rest ->
      let count =
        match W.Submission.Summary.outcome summary with
        | Succeeded -> W.Submission.Summary.binding_count summary
        | Failed _ -> 0
      in
      let selected = Int.min count remaining in
      let acc =
        List.init selected ~f:(fun binding_index -> statement_index, binding_index)
        |> List.fold ~init:acc ~f:(fun acc selection -> selection :: acc)
      in
      if count > remaining
      then List.rev acc, true
      else statements (statement_index + 1) (remaining - selected) acc rest
  in
  statements 0 max_bindings [] (W.Submission.summaries submission)
;;

let read_with ~limits ~inspect ~current ~submission =
  let selections, limited = selections submission in
  let rec loop remaining acc = function
    | [] -> { entries = List.rev acc; limited; error = None }
    | _ when remaining < 65 -> { entries = List.rev acc; limited = true; error = None }
    | (statement_index, binding_index) :: rest ->
      let result =
        W.Inspect.create
          ~limits
          ~current
          ~statement_index
          ~selector:(Binding_index binding_index)
          ~max_bytes:(Int.min remaining (W.Limits.max_output_bytes limits))
        |> Result.bind ~f:inspect
      in
      (match result with
       | Error error -> { entries = List.rev acc; limited; error = Some error }
       | Ok inspection ->
         if
           not
             (W.Wire_id.Revision.equal
                (W.Type_inspection.revision inspection)
                (W.Submission.revision submission))
         then
           { entries = List.rev acc
           ; limited
           ; error = Some (P.Error.invalid_request "type inspection revision changed")
           }
         else (
           let bytes =
             String.length
               (W.Submission.Rendered_type.text (W.Type_inspection.rendered inspection))
             + Option.value_map
                 (W.Type_inspection.binding_name inspection)
                 ~default:0
                 ~f:String.length
           in
           if bytes > remaining
           then { entries = List.rev acc; limited = true; error = None }
           else
             loop
               (remaining - bytes)
               ({ statement_index; binding_index; inspection } :: acc)
               rest))
  in
  loop max_bytes [] selections
;;

let read client ~current ~submission =
  read_with
    ~limits:(W.Capabilities.limits (Client.capabilities client))
    ~inspect:(Client.inspect_type client)
    ~current
    ~submission
;;

let%expect_test "automatic binding inspection bounds, partial failure and exact identity" =
  let must = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : P.Error.t)]
  in
  let document =
    Document_schema.Limits.create
      ~max_bytes:16_384
      ~max_depth:32
      ~max_fields:1_024
      ~max_nodes:2_048
    |> function
    | Ok value -> value
    | Error _ -> failwith "invalid test document bounds"
  in
  let limits =
    W.Limits.create
      ~request:document
      ~result:document
      ~page:document
      ~response:document
      ~max_source_bytes:512
      ~max_reference_bytes:384
      ~max_output_bytes:512
      ~max_page_items:16
      ~max_event_bytes:256
      ~max_registry_entries:16
    |> must
  in
  let instance = W.Wire_id.Instance.of_string "ri_types" |> must in
  let epoch = W.Wire_id.Epoch.of_string "re_types" |> must in
  let revision = W.Wire_id.Revision.of_string "rv_types" |> must in
  let other_revision = W.Wire_id.Revision.of_string "rv_other" |> must in
  let key = P.Idempotency_key.of_string "exact-types-submission" |> must in
  let current = W.Current_epoch.create ~instance ~epoch ~key in
  let summary ~binding_count ~failed =
    W.Submission.Summary.create
      ~limits
      ~outcome:(if failed then Failed Runtime else Succeeded)
      ~value_preview:None
      ~diagnostic:None
      ~expression_type:None
      ~binding_count
      ~task_retention:Not_a_task
      ~text_limited:false
    |> must
  in
  let run label ~binding_count ~failed ~stop ~stale ~large =
    let submission =
      W.Submission.create
        ~limits
        ~status:Finished
        ~details:Available
        ~completed_statements:2
        ~statement_count:2
        ~epoch
        ~revision
        ~summaries:
          [ summary ~binding_count:0 ~failed:false; summary ~binding_count ~failed ]
        ~task_reference_count:0
        ~task_references:[]
      |> must
    in
    let requests = ref 0 in
    let inspect request =
      let captured = W.Inspect.current request in
      if
        not
          (W.Wire_id.Instance.equal (W.Current_epoch.instance captured) instance
           && W.Wire_id.Epoch.equal (W.Current_epoch.epoch captured) epoch
           && P.Idempotency_key.equal (W.Current_epoch.key captured) key)
      then failwith "type read changed the exact submission identity";
      if not (Int.equal (W.Inspect.statement_index request) 1)
      then failwith "binding read lost its summary ordinal";
      (match W.Inspect.selector request with
       | Binding_index index when Int.equal index !requests -> ()
       | Expression | Binding_index _ -> failwith "binding order changed");
      Int.incr requests;
      if stop && Int.equal !requests 3
      then Error (P.Error.invalid_request "inspection unavailable")
      else
        W.Type_inspection.create
          ~limits
          ~revision:(if stale && Int.equal !requests 3 then other_revision else revision)
          ~binding_name:(Some "x")
          ~rendered:
            (W.Submission.Rendered_type.create
               ~limits
               ~kind:Type
               ~completeness:Complete
               ~text:
                 (if large
                  then String.make (Int.min 300 (W.Inspect.max_bytes request - 65)) 'a'
                  else "int")
             |> must)
    in
    let result = read_with ~limits ~inspect ~current ~submission in
    let retained_bytes =
      List.sum
        (module Int)
        (entries result)
        ~f:(fun entry ->
          1
          + String.length
              (W.Submission.Rendered_type.text
                 (W.Type_inspection.rendered entry.inspection)))
    in
    print_s
      [%sexp
        (label : string)
      , (!requests : int)
      , (List.length (entries result) : int)
      , (retained_bytes : int)
      , (limited result : bool)
      , (Option.is_some (error result) : bool)]
  in
  run "count" ~binding_count:20 ~failed:false ~stop:false ~stale:false ~large:false;
  run "bytes" ~binding_count:20 ~failed:false ~stop:false ~stale:false ~large:true;
  run "failure" ~binding_count:8 ~failed:false ~stop:true ~stale:false ~large:false;
  run "stale" ~binding_count:8 ~failed:false ~stop:false ~stale:true ~large:false;
  run
    "failed-statement"
    ~binding_count:8
    ~failed:true
    ~stop:false
    ~stale:false
    ~large:false;
  [%expect
    {|
    (count 16 16 64 true false)
    (bytes 14 14 4032 true false)
    (failure 3 2 8 false true)
    (stale 3 2 8 false true)
    (failed-statement 0 0 0 false false)
    |}]
;;
