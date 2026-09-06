open Core
module Job = Chat_message_render_job
module Runtime = Chat_render_worker_runtime

type snapshot =
  { transcript_generation : int
  ; render_generation : int
  ; width : int
  ; message_count : int
  ; theme_generation : int
  ; grammar_generation : int
  }

type completion =
  { snapshot : snapshot
  ; jobs : Job.t list
  ; results : Job.result list
  }

type outcome =
  | Completed of completion
  | Failed of exn
  | Cancelled

let runtime_initialization_mutex = Stdlib.Mutex.create ()

let snapshot ~model ~theme_generation ~grammar_generation =
  Model.active_history_width model
  |> Option.map ~f:(fun width ->
    { transcript_generation = Model.transcript_generation model
    ; render_generation = Model.render_generation model
    ; width
    ; message_count = Array.length (Model.render_messages model)
    ; theme_generation
    ; grammar_generation
    })
;;

let snapshot_is_current snapshot ~model =
  Int.equal snapshot.transcript_generation (Model.transcript_generation model)
  && Int.equal snapshot.render_generation (Model.render_generation model)
  && Option.equal Int.equal (Model.active_history_width model) (Some snapshot.width)
  && Int.equal snapshot.message_count (Array.length (Model.render_messages model))
;;

let render_partition ~domain_mgr ~config ~code_cache_capacity ~is_cancelled jobs =
  Eio.Domain_manager.run domain_mgr (fun () ->
    if is_cancelled () then raise Exit;
    let runtime =
      Stdlib.Mutex.lock runtime_initialization_mutex;
      Exn.protect
        ~f:(fun () -> Runtime.create ~config ~code_cache_capacity () |> Or_error.ok_exn)
        ~finally:(fun () -> Stdlib.Mutex.unlock runtime_initialization_mutex)
    in
    List.map jobs ~f:(fun job ->
      if is_cancelled () then raise Exit;
      Runtime.render runtime ~is_cancelled job))
;;

let render ~domain_mgr ~config ~code_cache_capacity ~is_cancelled ~snapshot ~jobs =
  let midpoint = (List.length jobs + 1) / 2 in
  let left, right = List.split_n jobs midpoint in
  match
    Eio.Fiber.pair
      (fun () ->
         render_partition ~domain_mgr ~config ~code_cache_capacity ~is_cancelled left)
      (fun () ->
         render_partition ~domain_mgr ~config ~code_cache_capacity ~is_cancelled right)
  with
  | left_results, right_results ->
    Completed { snapshot; jobs; results = left_results @ right_results }
  | exception Exit -> Cancelled
  | exception Eio.Cancel.Cancelled _ -> Cancelled
  | exception exn -> Failed exn
;;
