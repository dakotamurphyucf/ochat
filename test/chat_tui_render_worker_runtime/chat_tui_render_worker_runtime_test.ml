open Core
module Job = Chat_tui.Chat_message_render_job
module Runtime = Chat_tui.Chat_render_worker_runtime
module Source = Chat_tui.Highlight_grammar_discovery.Source

let grammar =
  {|{
    "name": "Worker private",
    "scopeName": "source.worker-private",
    "fileTypes": ["worker-private"],
    "patterns": [
      { "match": "[0-9]+", "name": "constant.numeric.worker-private" }
    ]
  }|}
  |> Jsonaf.parse
  |> Or_error.ok_exn
  |> Source.create ~name:"worker-private.json"
  |> Or_error.ok_exn
;;

let config ?(custom_grammars = []) grammar_generation =
  Runtime.Config.create ~custom_grammars ~theme_generation:0 ~grammar_generation
;;

let runtime ?custom_grammars generation =
  Runtime.create ~config:(config ?custom_grammars generation) ~code_cache_capacity:8 ()
  |> Or_error.ok_exn
;;

let job ?(lang = "ocaml") generation =
  let row_id =
    Chat_tui.Projected_message.Id.local
      ~namespace:"render-worker-runtime-test"
      ~local_id:"2"
    |> Result.ok_or_failwith
  in
  Job.create
    ~transcript_generation:1
    ~row_id
    ~row_revision:3
    ~message_index:2
    ~message_revision:3
    ~width:40
    ~role:"assistant"
    ~text:(sprintf "```%s\nlet value = 42\n```" lang)
    ~tool_output:None
    ~tool_call_outcome:None
    ~theme_generation:0
    ~grammar_generation:generation
    ~geometry_generation:1
    ~request_generation:4
    ~render_generation:0
    ~submission_generation:0
    ~semantic_seed:None
    ~priority:Visible
;;

let image_to_string image =
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer
    buffer
    Notty.Cap.ansi
    (0, 0)
    (Notty.I.width image, Notty.I.height image)
    image;
  Buffer.contents buffer
;;

let%test_unit "independent runtimes own equivalent bundled registries and caches" =
  let a = runtime 7 in
  let b = runtime 7 in
  assert (Runtime.For_testing.has_language a "ocaml");
  assert (Runtime.For_testing.has_language b "ocaml");
  let rendered_a = Runtime.render a (job 7) in
  [%test_eq: int] (Runtime.For_testing.code_cache_length a) 1;
  [%test_eq: int] (Runtime.For_testing.code_cache_length b) 0;
  assert (Runtime.For_testing.highlight_cache_length a > 0);
  [%test_eq: int] (Runtime.For_testing.highlight_cache_length b) 0;
  let rendered_b = Runtime.render b (job 7) in
  [%test_eq: string] (image_to_string rendered_a.image) (image_to_string rendered_b.image);
  [%test_eq: int] (Runtime.For_testing.code_cache_length b) 1;
  assert (Runtime.For_testing.highlight_cache_length b > 0)
;;

let%test_unit "custom grammar sources rebuild equivalent private registries" =
  let a = runtime ~custom_grammars:[ grammar ] 8 in
  let b = runtime ~custom_grammars:[ grammar ] 8 in
  assert (Runtime.For_testing.has_language a "worker-private");
  assert (Runtime.For_testing.has_language b "worker-private");
  let rendered_a = Runtime.render a (job ~lang:"worker-private" 8) in
  let rendered_b = Runtime.render b (job ~lang:"worker-private" 8) in
  [%test_eq: string] (image_to_string rendered_a.image) (image_to_string rendered_b.image)
;;

let%test_unit "a custom grammar in one runtime does not leak to another" =
  let private_runtime = runtime ~custom_grammars:[ grammar ] 8 in
  let bundled_runtime = runtime 8 in
  assert (Runtime.For_testing.has_language private_runtime "worker-private");
  assert (not (Runtime.For_testing.has_language bundled_runtime "worker-private"))
;;

let%test_unit "rebuild atomically replaces registry generation and cache" =
  let runtime = runtime 9 in
  ignore (Runtime.render runtime (job 9) : Job.result);
  [%test_eq: int] (Runtime.For_testing.code_cache_length runtime) 1;
  assert (Runtime.For_testing.highlight_cache_length runtime > 0);
  Runtime.rebuild runtime ~config:(config ~custom_grammars:[ grammar ] 10)
  |> Or_error.ok_exn;
  [%test_eq: int] (Runtime.grammar_generation runtime) 10;
  [%test_eq: int] (Runtime.For_testing.code_cache_length runtime) 0;
  [%test_eq: int] (Runtime.For_testing.highlight_cache_length runtime) 0;
  assert (Runtime.For_testing.has_language runtime "worker-private");
  (match Runtime.render runtime (job 9) with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  let result = Runtime.render runtime (job ~lang:"worker-private" 10) in
  assert (Job.result_matches result (job ~lang:"worker-private" 10))
;;

let%test_unit "highlight spans are reused across widths" =
  let runtime = runtime 11 in
  ignore (Runtime.render runtime (job 11) : Job.result);
  let highlighted = Runtime.For_testing.highlight_cache_length runtime in
  let prepared = Runtime.For_testing.prepared_cache_length runtime in
  assert (highlighted > 0);
  assert (prepared > 0);
  let wide = { (job 11) with key = { (job 11).key with width = 72 } } in
  ignore (Runtime.render runtime wide : Job.result);
  [%test_eq: int] (Runtime.For_testing.highlight_cache_length runtime) highlighted;
  [%test_eq: int] (Runtime.For_testing.prepared_cache_length runtime) prepared;
  [%test_eq: int] (Runtime.For_testing.code_cache_length runtime) 2
;;
