open! Core
module C = Developer_codec
module D = Document_schema
module Common = Developer_common
module Base = Developer_repl_wire_common
module Limits = Base.Limits
module Wire_id = Base.Wire_id

let ( let* ) result f = Result.bind result ~f
let get = Base.get

module Features = Base.Features
module Create = Base.Create
module Submit = Base.Submit
module Lookup = Base.Lookup
module Current_epoch = Base.Current_epoch
module Type_selector = Base.Type_selector
module Inspect = Base.Inspect
module Reference_page_request = Base.Reference_page_request
module Task_submit = Base.Task_submit
module View = Developer_repl_wire_result.View
module Submission = Developer_repl_wire_result.Submission
module Type_inspection = Developer_repl_wire_result.Type_inspection
module Reference_page = Developer_repl_wire_result.Reference_page
module Task_view = Developer_repl_wire_task.Task_view

let supported_method_names = Base.supported_method_names

module Capabilities = struct
  type t =
    { methods : string list
    ; limits : Limits.t
    }

  let to_json t =
    `Object
      [ "format_version", C.integer_json 1
      ; "methods", `Array (List.map t.methods ~f:(fun x -> `String x))
      ; "limits", Limits.to_json t.limits
      ]
  ;;

  let create ~methods ~limits =
    if
      List.length methods > 32
      || List.length methods
         <> List.length (List.dedup_and_sort methods ~compare:String.compare)
      || List.exists methods ~f:(fun method_ ->
        not (List.mem supported_method_names method_ ~equal:String.equal))
    then C.invalid "invalid REPL method inventory"
    else (
      let t = { methods; limits } in
      Result.map (C.validate ~limits:D.Limits.default (to_json t)) ~f:(fun () -> t))
  ;;

  let supports t method_ = List.mem t.methods method_ ~equal:String.equal
  let limits t = t.limits

  let of_json json =
    let* () = C.validate ~limits:D.Limits.default json in
    let* fields = C.fields ~allowed:[ "format_version"; "methods"; "limits" ] json in
    let* format_version = get fields "format_version" C.positive in
    if format_version <> 1
    then C.invalid "unsupported REPL wire format"
    else
      let* methods =
        get fields "methods" (C.array ~max_items:32 (C.string ~max_bytes:96))
      in
      let* limits = get fields "limits" Limits.of_json in
      create ~methods ~limits
  ;;
end

module Watch_key = struct
  type t =
    | Repl of Lookup.t
    | Task of Common.Lookup.t

  let to_json = function
    | Repl lookup -> `Object [ "kind", `String "repl"; "lookup", Lookup.to_json lookup ]
    | Task lookup ->
      `Object [ "kind", `String "task"; "lookup", Common.Lookup.to_json lookup ]
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.request limits) json in
    let* fields = C.fields ~allowed:[ "kind"; "lookup" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    match kind with
    | "repl" ->
      let* lookup = get fields "lookup" (Lookup.of_json ~limits) in
      Ok (Repl lookup)
    | "task" ->
      let* lookup =
        get fields "lookup" (Common.Lookup.of_json ~limits:(Limits.request limits))
      in
      Ok (Task lookup)
    | _ -> C.invalid "unknown REPL watch kind"
  ;;
end

module Request = struct
  type t =
    | Capabilities
    | Create of Create.t
    | Submit of Submit.t
    | Observe of Lookup.t
    | Cancel_submission of Lookup.t
    | Inspect_type of Inspect.t
    | Task_references_page of Reference_page_request.t
    | Task_submit of Task_submit.t
    | Task_observe of Common.Lookup.t
    | Task_await_terminal of Common.Lookup.t
    | Task_cancel_and_wait of Common.Lookup.t
    | Reset_and_wait of Current_epoch.t
    | Close_and_wait of Lookup.t
    | Watch of Watch_key.t
    | Unwatch of Watch_key.t

  let method_name = function
    | Capabilities -> "developer.repl.capabilities"
    | Create _ -> "developer.repl.create"
    | Submit _ -> "developer.repl.submit"
    | Observe _ -> "developer.repl.observe"
    | Cancel_submission _ -> "developer.repl.cancel_submission"
    | Inspect_type _ -> "developer.repl.inspect_type"
    | Task_references_page _ -> "developer.repl.task_references_page"
    | Task_submit _ -> "developer.repl.task.submit"
    | Task_observe _ -> "developer.repl.task.observe"
    | Task_await_terminal _ -> "developer.repl.task.await_terminal"
    | Task_cancel_and_wait _ -> "developer.repl.task.cancel_and_wait"
    | Reset_and_wait _ -> "developer.repl.reset_and_wait"
    | Close_and_wait _ -> "developer.repl.close_and_wait"
    | Watch _ -> "developer.repl.watch"
    | Unwatch _ -> "developer.repl.unwatch"
  ;;

  let supported_methods = supported_method_names

  let params = function
    | Capabilities -> `Object []
    | Create value -> Create.to_json value
    | Submit value -> Submit.to_json value
    | Observe value | Cancel_submission value | Close_and_wait value ->
      Lookup.to_json value
    | Inspect_type value -> Inspect.to_json value
    | Task_references_page value -> Reference_page_request.to_json value
    | Task_submit value -> Task_submit.to_json value
    | Task_observe value | Task_await_terminal value | Task_cancel_and_wait value ->
      Common.Lookup.to_json value
    | Reset_and_wait value -> Current_epoch.to_json value
    | Watch value | Unwatch value -> Watch_key.to_json value
  ;;

  let of_method_and_params ~limits ~method_ ~params =
    let* () = C.validate ~limits:(Limits.request limits) params in
    match method_ with
    | "developer.repl.capabilities" ->
      let* () = C.fields ~allowed:[] params |> Result.map ~f:ignore in
      Ok Capabilities
    | "developer.repl.create" ->
      Result.map (Create.of_json ~limits params) ~f:(fun x -> Create x)
    | "developer.repl.submit" ->
      Result.map (Submit.of_json ~limits params) ~f:(fun x -> Submit x)
    | "developer.repl.observe" ->
      Result.map (Lookup.of_json ~limits params) ~f:(fun x -> Observe x)
    | "developer.repl.cancel_submission" ->
      Result.map (Lookup.of_json ~limits params) ~f:(fun x -> Cancel_submission x)
    | "developer.repl.inspect_type" ->
      Result.map (Inspect.of_json ~limits params) ~f:(fun x -> Inspect_type x)
    | "developer.repl.task_references_page" ->
      Result.map (Reference_page_request.of_json ~limits params) ~f:(fun x ->
        Task_references_page x)
    | "developer.repl.task.submit" ->
      Result.map (Task_submit.of_json ~limits params) ~f:(fun x -> Task_submit x)
    | "developer.repl.task.observe" ->
      Result.map
        (Common.Lookup.of_json ~limits:(Limits.request limits) params)
        ~f:(fun x -> Task_observe x)
    | "developer.repl.task.await_terminal" ->
      Result.map
        (Common.Lookup.of_json ~limits:(Limits.request limits) params)
        ~f:(fun x -> Task_await_terminal x)
    | "developer.repl.task.cancel_and_wait" ->
      Result.map
        (Common.Lookup.of_json ~limits:(Limits.request limits) params)
        ~f:(fun x -> Task_cancel_and_wait x)
    | "developer.repl.reset_and_wait" ->
      Result.map (Current_epoch.of_json ~limits params) ~f:(fun x -> Reset_and_wait x)
    | "developer.repl.close_and_wait" ->
      Result.map (Lookup.of_json ~limits params) ~f:(fun x -> Close_and_wait x)
    | "developer.repl.watch" ->
      Result.map (Watch_key.of_json ~limits params) ~f:(fun x -> Watch x)
    | "developer.repl.unwatch" ->
      Result.map (Watch_key.of_json ~limits params) ~f:(fun x -> Unwatch x)
    | _ -> C.invalid "unknown REPL protocol method"
  ;;
end

module Response = struct
  type t =
    | Capabilities of Capabilities.t
    | Create of View.t
    | Submit of Submission.t
    | Observe of Submission.t option
    | Cancel_submission of Submission.t
    | Inspect_type of Type_inspection.t
    | Task_references_page of Reference_page.t
    | Task_submit of Task_view.t
    | Task_observe of Task_view.t option
    | Task_await_terminal of Task_view.t
    | Task_cancel_and_wait of Task_view.t
    | Reset_and_wait of View.t
    | Close_and_wait
    | Watch
    | Unwatch

  let method_name = function
    | Capabilities _ -> "developer.repl.capabilities"
    | Create _ -> "developer.repl.create"
    | Submit _ -> "developer.repl.submit"
    | Observe _ -> "developer.repl.observe"
    | Cancel_submission _ -> "developer.repl.cancel_submission"
    | Inspect_type _ -> "developer.repl.inspect_type"
    | Task_references_page _ -> "developer.repl.task_references_page"
    | Task_submit _ -> "developer.repl.task.submit"
    | Task_observe _ -> "developer.repl.task.observe"
    | Task_await_terminal _ -> "developer.repl.task.await_terminal"
    | Task_cancel_and_wait _ -> "developer.repl.task.cancel_and_wait"
    | Reset_and_wait _ -> "developer.repl.reset_and_wait"
    | Close_and_wait -> "developer.repl.close_and_wait"
    | Watch -> "developer.repl.watch"
    | Unwatch -> "developer.repl.unwatch"
  ;;

  let raw_json = function
    | Capabilities value -> Capabilities.to_json value
    | Create value | Reset_and_wait value -> View.to_json value
    | Submit value | Cancel_submission value -> Submission.to_json value
    | Observe None | Task_observe None -> `Object [ "found", `False ]
    | Observe (Some value) ->
      `Object [ "found", `True; "submission", Submission.to_json value ]
    | Inspect_type value -> Type_inspection.to_json value
    | Task_references_page value -> Reference_page.to_json value
    | Task_submit value | Task_await_terminal value | Task_cancel_and_wait value ->
      Task_view.to_json value
    | Task_observe (Some value) ->
      `Object [ "found", `True; "view", Task_view.to_json value ]
    | Close_and_wait | Watch | Unwatch -> `Object []
  ;;

  let of_method_and_json ~limits ~method_ json =
    let* () =
      C.validate
        ~limits:
          (if String.equal method_ "developer.repl.capabilities"
           then D.Limits.default
           else Limits.response limits)
        json
    in
    match method_ with
    | "developer.repl.capabilities" ->
      Result.map (Capabilities.of_json json) ~f:(fun x -> Capabilities x)
    | "developer.repl.create" ->
      Result.map (View.of_json ~limits json) ~f:(fun x -> Create x)
    | "developer.repl.submit" ->
      Result.map (Submission.of_json ~limits json) ~f:(fun x -> Submit x)
    | "developer.repl.observe" ->
      let* fields = C.fields ~allowed:[ "found"; "submission" ] json in
      let* found = get fields "found" C.boolean in
      if found
      then
        let* submission = get fields "submission" (Submission.of_json ~limits) in
        Ok (Observe (Some submission))
      else
        let* () = C.fields ~allowed:[ "found" ] json |> Result.map ~f:ignore in
        Ok (Observe None)
    | "developer.repl.cancel_submission" ->
      Result.map (Submission.of_json ~limits json) ~f:(fun x -> Cancel_submission x)
    | "developer.repl.inspect_type" ->
      Result.map (Type_inspection.of_json ~limits json) ~f:(fun x -> Inspect_type x)
    | "developer.repl.task_references_page" ->
      Result.map (Reference_page.of_json ~limits json) ~f:(fun x ->
        Task_references_page x)
    | "developer.repl.task.submit" ->
      Result.map (Task_view.of_json ~limits json) ~f:(fun x -> Task_submit x)
    | "developer.repl.task.observe" ->
      let* fields = C.fields ~allowed:[ "found"; "view" ] json in
      let* found = get fields "found" C.boolean in
      if found
      then
        let* view = get fields "view" (Task_view.of_json ~limits) in
        Ok (Task_observe (Some view))
      else
        let* () = C.fields ~allowed:[ "found" ] json |> Result.map ~f:ignore in
        Ok (Task_observe None)
    | "developer.repl.task.await_terminal" ->
      Result.map (Task_view.of_json ~limits json) ~f:(fun x -> Task_await_terminal x)
    | "developer.repl.task.cancel_and_wait" ->
      Result.map (Task_view.of_json ~limits json) ~f:(fun x -> Task_cancel_and_wait x)
    | "developer.repl.reset_and_wait" ->
      Result.map (View.of_json ~limits json) ~f:(fun x -> Reset_and_wait x)
    | "developer.repl.close_and_wait" ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Close_and_wait
    | "developer.repl.watch" ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Watch
    | "developer.repl.unwatch" ->
      let* () = C.fields ~allowed:[] json |> Result.map ~f:ignore in
      Ok Unwatch
    | _ -> C.invalid "unknown REPL response method"
  ;;

  let to_json t ~limits =
    let json = raw_json t in
    let* _ = of_method_and_json ~limits ~method_:(method_name t) json in
    Ok json
  ;;
end

module Change = struct
  type kind =
    | Changed
    | Resync_required
  [@@deriving equal, sexp_of]

  type t =
    { key : Watch_key.t
    ; kind : kind
    }

  let method_name = "developer.repl.changed"

  let to_json t =
    `Object
      [ "key", Watch_key.to_json t.key
      ; ( "kind"
        , `String
            (match t.kind with
             | Changed -> "changed"
             | Resync_required -> "resync_required") )
      ]
  ;;

  let create ~limits ~key ~kind =
    let t = { key; kind } in
    let* bytes = C.measure ~limits:(Limits.response limits) (to_json t) in
    if bytes > Limits.max_event_bytes limits
    then C.limit "REPL event exceeds wire bound"
    else Ok t
  ;;

  let key t = t.key
  let kind t = t.kind

  let of_json ~limits json =
    let* () = C.validate ~limits:(Limits.response limits) json in
    let* fields = C.fields ~allowed:[ "key"; "kind" ] json in
    let* key = get fields "key" (Watch_key.of_json ~limits) in
    let* kind = get fields "kind" (C.string ~max_bytes:32) in
    let* kind =
      match kind with
      | "changed" -> Ok Changed
      | "resync_required" -> Ok Resync_required
      | _ -> C.invalid "unknown REPL change kind"
    in
    create ~limits ~key ~kind
  ;;
end
