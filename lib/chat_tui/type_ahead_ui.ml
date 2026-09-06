open! Core
module Coordinator = Type_ahead_controller

type before =
  { draft : string
  ; cursor : int
  ; generation : int
  ; relevant : bool
  }

type t =
  { model : Model.t
  ; config : Type_ahead_config.t
  ; coordinator : Coordinator.t
  ; host : unit -> string option
  ; mutable identity : string option
  ; mutable projection_epoch : int
  ; mutable epoch : int
  ; mutable history : History_entry.t list
  ; mutable warned : bool
  ; mutable eligible : bool
  }

let is_eligible t =
  (not (Type_ahead_config.equal_mode t.config.mode Off))
  && Option.is_some (t.host ())
  && Poly.(Model.active_page t.model = Model.Page_id.Chat)
  && Poly.(Model.mode t.model = Model.Insert)
  && Poly.(Model.draft_mode t.model = Model.Plain)
  && Model.normal_input_is_enabled t.model
  && (match Model.chat_materialization t.model with
      | Warm | Corridor -> true
      | Loading | Resizing -> false)
  && Option.is_none (Model.shell_approval_modal t.model)
  && Option.is_none (Model.moderator_modal t.model)
  && not (String.is_empty (String.strip (Model.input_line t.model)))
;;

let invalidate t =
  Coordinator.cancel t.coordinator;
  ignore (Model.bump_typeahead_generation t.model : int);
  Model.clear_typeahead t.model;
  Model.set_typeahead_status t.model None
;;

let sync t =
  let identity = t.host () in
  let history = Model.history_items t.model in
  if
    (not (Option.equal String.equal identity t.identity))
    || (not (phys_equal history t.history))
    || t.projection_epoch <> Model.typeahead_context_epoch t.model
    || (t.eligible && not (is_eligible t))
  then (
    t.epoch <- t.epoch + 1;
    invalidate t);
  t.identity <- identity;
  t.history <- history;
  t.projection_epoch <- Model.typeahead_context_epoch t.model;
  t.eligible <- is_eligible t;
  if not t.eligible then Model.clear_typeahead t.model
;;

let before t =
  sync t;
  { draft = Model.input_line t.model
  ; cursor = Model.cursor_pos t.model
  ; generation = Model.typeahead_generation t.model
  ; relevant = Model.typeahead_is_relevant t.model
  }
;;

let visible_context t =
  if t.config.history_messages = 0
  then []
  else (
    let messages = Model.message_array t.model in
    let rec loop index remaining acc =
      if index < 0 || remaining = 0
      then acc
      else (
        let role, _ = messages.(index) in
        if
          List.mem [ "user"; "assistant"; "developer"; "system" ] role ~equal:String.equal
        then loop (index - 1) (remaining - 1) (messages.(index) :: acc)
        else loop (index - 1) remaining acc)
    in
    loop (Array.length messages - 1) t.config.history_messages [])
;;

let snapshot t =
  let draft = Model.input_line t.model in
  let cursor = Model.cursor_pos t.model in
  Coordinator.
    { identity = Option.value_exn t.identity
    ; epoch = t.epoch
    ; generation = Model.typeahead_generation t.model
    ; draft
    ; cursor
    ; input =
        Type_ahead_provider.prepare t.config ~messages:(visible_context t) ~draft ~cursor
    }
;;

let is_ctrl_space = function
  | `Key (`ASCII ('@' | ' '), mods) -> List.mem mods `Ctrl ~equal:Poly.equal
  | `Key (`ASCII '\000', _) -> true
  | _ -> false
;;

let is_accept = function
  | `Key (`Tab, _) -> true
  | _ -> false
;;

let manual_request t =
  invalidate t;
  Model.set_typeahead_preview_open t.model true;
  Model.set_typeahead_status t.model (Some "suggesting");
  Coordinator.request t.coordinator (snapshot t)
;;

let automatic_request t =
  invalidate t;
  if Type_ahead_config.equal_mode t.config.mode Auto
  then
    Coordinator.schedule
      t.coordinator
      (snapshot t)
      ~delay:(Float.of_int t.config.debounce_ms /. 1000.)
;;

let after t pre event ~finished =
  sync t;
  let changed =
    (not (String.equal pre.draft (Model.input_line t.model)))
    || pre.cursor <> Model.cursor_pos t.model
  in
  if finished
  then invalidate t
  else if is_eligible t
  then
    if is_ctrl_space event && not pre.relevant
    then manual_request t
    else if changed && pre.relevant && is_accept event
    then Coordinator.cancel t.coordinator
    else if changed
    then automatic_request t
    else if pre.generation <> Model.typeahead_generation t.model
    then Coordinator.cancel t.coordinator
;;

let matches t (request : Coordinator.snapshot) =
  is_eligible t
  && Option.equal String.equal t.identity (Some request.identity)
  && t.epoch = request.epoch
  && Model.typeahead_generation t.model = request.generation
  && String.equal (Model.input_line t.model) request.draft
  && Model.cursor_pos t.model = request.cursor
;;

let install t (request : Coordinator.snapshot) = function
  | Ok text ->
    if String.is_empty text
    then Model.clear_typeahead t.model
    else
      Model.set_typeahead_completion
        t.model
        (Some
           { text
           ; base_input = request.draft
           ; base_cursor = request.cursor
           ; generation = request.generation
           })
  | Error _ ->
    Model.clear_typeahead t.model;
    if not t.warned
    then (
      t.warned <- true;
      Model.set_typeahead_status t.model (Some "typeahead unavailable"))
;;

let handle t event =
  sync t;
  match event with
  | Coordinator.Ready request ->
    if matches t request
    then (
      Model.set_typeahead_status t.model (Some "suggesting");
      Coordinator.request t.coordinator request)
  | Completed (request, result) ->
    if matches t request
    then (
      Model.set_typeahead_status t.model None;
      install t request result)
;;

let create_with ~sw ~sleep ~complete ~config ~model ~host ~emit =
  let coordinator = Coordinator.create ~sw ~sleep ~complete ~emit in
  { model
  ; config
  ; coordinator
  ; host
  ; identity = host ()
  ; projection_epoch = Model.typeahead_context_epoch model
  ; epoch = 0
  ; history = Model.history_items model
  ; warned = false
  ; eligible = false
  }
;;

let create ~sw ~env ~config ~model ~host ~emit =
  create_with
    ~sw
    ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env))
    ~complete:(Type_ahead_provider.complete_suffix ~env ~config)
    ~config
    ~model
    ~host
    ~emit
;;

let close t =
  invalidate t;
  Coordinator.close t.coordinator
;;
