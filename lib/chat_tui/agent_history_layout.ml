open! Core

type completion = int * Chat_startup_render.outcome

type request =
  { generation : int
  ; snapshot : Chat_startup_render.snapshot
  ; jobs : Chat_message_render_job.t list
  ; cancelled : bool Atomic.t
  }

type t =
  { model : Model.t
  ; wakeup : unit Eio.Stream.t
  ; mutable generation : int
  ; mutable pending : request option
  ; mutable cancel : bool Atomic.t option
  ; mutable destination : Controller_types.chat_destination option
  ; mutable closed : bool
  }

let navigate model ~viewport_height = function
  | Controller_types.Earlier_conversation ->
    Model.set_auto_follow model false;
    Notty_scroll_box.scroll_to_top (Model.scroll_box model)
  | Latest_conversation -> Model.follow_chat_bottom model ~viewport_height
  | Search_result id -> Model.request_projected_reveal model ~id
;;

let apply_destination t ~size:(screen_w, screen_h) =
  let viewport_height =
    (Chat_page_layout.compute ~screen_w ~screen_h ~model:t.model).scroll_height
  in
  Option.iter t.destination ~f:(navigate t.model ~viewport_height);
  t.destination <- None
;;

let publish t ~size =
  if Renderer_page_chat.publish_startup_history ~size ~model:t.model
  then (
    Model.set_chat_materialization_warm t.model;
    Model.set_normal_input_enabled t.model true;
    apply_destination t ~size;
    true)
  else false
;;

let enqueue t ~grammar_generation jobs =
  Option.iter
    (Chat_startup_render.snapshot ~model:t.model ~theme_generation:0 ~grammar_generation)
    ~f:(fun snapshot ->
      let cancelled = Atomic.make false in
      t.cancel <- Some cancelled;
      t.pending <- Some { generation = t.generation; snapshot; jobs; cancelled };
      if Eio.Stream.length t.wakeup = 0 then Eio.Stream.add t.wakeup ())
;;

let request_background t ~size =
  if not t.closed
  then (
    Option.iter t.cancel ~f:(fun cancel -> Atomic.set cancel true);
    t.pending <- None;
    t.generation <- t.generation + 1;
    Model.set_chat_materialization_loading t.model;
    Renderer_page_chat.prepare_startup_history ~size ~model:t.model;
    let grammar_generation = Highlight_registry.generation () in
    let jobs =
      Renderer_page_chat.startup_background_jobs
        ~theme_generation:0
        ~grammar_generation
        ~model:t.model
    in
    if List.is_empty jobs
    then ignore (publish t ~size : bool)
    else enqueue t ~grammar_generation jobs)
;;

let request t ~size =
  if not t.closed
  then (
    match Model.chat_materialization t.model, Model.active_history_width t.model with
    | Model.Chat_page_state.Warm, Some width when width = fst size ->
      Renderer_page_chat.warm_history_synchronously ~size ~model:t.model;
      apply_destination t ~size
    | _ -> request_background t ~size)
;;

let is_current t ~size (completion : Chat_startup_render.completion) =
  Chat_startup_render.snapshot_is_current completion.snapshot ~model:t.model
  && completion.snapshot.width = fst size
  && List.length completion.jobs = List.length completion.results
  && List.for_all2_exn
       completion.results
       completion.jobs
       ~f:Chat_message_render_job.result_matches
;;

let install t results =
  List.for_all results ~f:(fun result ->
    let accepted = Model.commit_startup_render_result t.model result in
    if accepted then Renderer_component_message.install_highlights result.highlights;
    accepted)
;;

let accept t ~size (generation, outcome) =
  if t.closed || generation <> t.generation
  then false
  else (
    match outcome with
    | Chat_startup_render.Completed completion when is_current t ~size completion ->
      if install t completion.results && publish t ~size
      then true
      else (
        request t ~size;
        false)
    | Failed _ ->
      Renderer_page_chat.relayout_history_synchronously ~size ~model:t.model;
      publish t ~size
    | Completed _ | Cancelled ->
      request t ~size;
      false)
;;

let prepare_destination t ~size destination =
  match Model.chat_materialization t.model with
  | Model.Chat_page_state.Warm ->
    t.destination <- Some destination;
    apply_destination t ~size
  | _ -> t.destination <- Some destination
;;

let close t =
  t.closed <- true;
  t.pending <- None;
  Option.iter t.cancel ~f:(fun cancelled -> Atomic.set cancelled true)
;;

let run_worker t ~env ~config ~emit =
  while not t.closed do
    Eio.Stream.take t.wakeup;
    let pending = t.pending in
    t.pending <- None;
    Option.iter pending ~f:(fun request ->
      let outcome =
        Chat_startup_render.render
          ~domain_mgr:(Eio.Stdenv.domain_mgr env)
          ~config
          ~code_cache_capacity:128
          ~is_cancelled:(fun () -> Atomic.get request.cancelled)
          ~snapshot:request.snapshot
          ~jobs:request.jobs
      in
      if not t.closed then emit (request.generation, outcome))
  done;
  `Stop_daemon
;;

let create ~sw ~env ~model ~config ~emit =
  let t =
    { model
    ; wakeup = Eio.Stream.create 1
    ; generation = 0
    ; pending = None
    ; cancel = None
    ; destination = None
    ; closed = false
    }
  in
  Eio.Fiber.fork_daemon ~sw (fun () -> run_worker t ~env ~config ~emit);
  t
;;
