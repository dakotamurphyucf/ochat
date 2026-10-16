open! Core
module P = Agent_protocol
module CM = Prompt.Chat_markdown
module Source = Agent_session.Prompt_source_owner
module Scope = Source.Scope
module Content_source = Agent_session.Captured_agent_runner.Source_binding.Content_source
module C = Webpage_markdown.Bounded_conversion
open P.Error

module Limits = struct
  type t =
    { max_input_bytes : int
    ; max_output_bytes : int
    ; max_work_steps : int
    ; max_nodes : int
    ; max_depth : int
    }

  let create ~max_input_bytes ~max_output_bytes ~max_work_steps ~max_nodes ~max_depth =
    if
      max_input_bytes <= 0
      || max_input_bytes > 16 * 1024 * 1024
      || max_output_bytes <= 0
      || max_output_bytes > 32 * 1024 * 1024
      || max_work_steps <= 0
      || max_work_steps > 64 * 1024 * 1024
      || max_nodes <= 0
      || max_nodes > 1_000_000
      || max_depth <= 0
      || max_depth > 256
    then Or_error.error_string "child content conversion limits are invalid"
    else Ok { max_input_bytes; max_output_bytes; max_work_steps; max_nodes; max_depth }
  ;;

  let for_host configured =
    let module L = Chatmd_prompt_content.Limits in
    let input, output, work, nodes, depth =
      match configured with
      | None -> 8 * 1024 * 1024, 8 * 1024 * 1024, 10_000_000, 1_000_000, 128
      | Some limits ->
        ( L.input_bytes limits
        , L.output_bytes limits
        , L.work_steps limits
        , L.nodes limits
        , L.depth limits )
    in
    create
      ~max_input_bytes:input
      ~max_output_bytes:output
      ~max_work_steps:work
      ~max_nodes:nodes
      ~max_depth:depth
  ;;
end

let error code message = P.Error.create code ~message ~retryable:false ()
let resource message = Error (error Resource_limit message)
let invalid message = Error (error Invalid_state message)

module Budget = struct
  type t =
    { limits : Limits.t
    ; mutable contents : Agent_session.Prompt_revision.t list
    ; mutable input_remaining : int
    ; mutable output_remaining : int
    ; mutable work_remaining : int
    ; mutable nodes_remaining : int
    ; gate : Eio.Mutex.t
    ; mutable uncertain : bool
    }

  let create ~limits ~declaration ~caller =
    let contents =
      match caller with
      | None -> [ declaration ]
      | Some caller when phys_equal declaration caller -> [ declaration ]
      | Some caller -> [ declaration; caller ]
    in
    let budget =
      { limits
      ; contents
      ; input_remaining = limits.max_input_bytes
      ; output_remaining = limits.max_output_bytes
      ; work_remaining = limits.max_work_steps
      ; nodes_remaining = limits.max_nodes
      ; gate = Eio.Mutex.create ()
      ; uncertain = false
      }
    in
    let debit captured =
      let module A = Chatmd_prompt_content.Accounting in
      let input = A.input_bytes captured in
      let output = A.output_bytes captured in
      let work = A.work_steps captured in
      let nodes = A.nodes captured in
      if
        input > budget.input_remaining
        || output > budget.output_remaining
        || work > budget.work_remaining
        || nodes > budget.nodes_remaining
      then resource "captured child content exceeds aggregate budget"
      else (
        budget.input_remaining <- budget.input_remaining - input;
        budget.output_remaining <- budget.output_remaining - output;
        budget.work_remaining <- budget.work_remaining - work;
        budget.nodes_remaining <- budget.nodes_remaining - nodes;
        Ok ())
    in
    let open Result.Let_syntax in
    let%map () =
      List.fold_result contents ~init:() ~f:(fun () content ->
        match Agent_session.Prompt_revision.content content with
        | None -> Ok ()
        | Some captured -> debit (Chatmd_prompt_content.accounting captured))
    in
    budget
  ;;

  let with_exclusive t ~f =
    Eio.Mutex.lock t.gate;
    Exn.protect
      ~f:(fun () ->
        if t.uncertain
        then resource "child content budget has an unresolved read"
        else f ())
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Mutex.unlock t.gate))
  ;;

  let admit_revisions t ~declaration ~caller =
    with_exclusive t ~f:(fun () ->
      let revisions =
        if phys_equal declaration caller then [ declaration ] else [ declaration; caller ]
      in
      let fresh =
        List.filter revisions ~f:(fun revision ->
          not (List.exists t.contents ~f:(phys_equal revision)))
      in
      let module A = Chatmd_prompt_content.Accounting in
      let open Result.Let_syntax in
      let%bind input, output, work, nodes =
        List.fold_result
          fresh
          ~init:(0, 0, 0, 0)
          ~f:(fun (input, output, work, nodes) revision ->
            let captured =
              Option.map
                (Agent_session.Prompt_revision.content revision)
                ~f:Chatmd_prompt_content.accounting
            in
            let next getter = Option.value_map captured ~default:0 ~f:getter in
            let input_next = next A.input_bytes in
            let output_next = next A.output_bytes in
            let work_next = next A.work_steps + 1 in
            let nodes_next = next A.nodes in
            if
              input_next > t.input_remaining - input
              || output_next > t.output_remaining - output
              || work_next > t.work_remaining - work
              || nodes_next > t.nodes_remaining - nodes
            then resource "nested child captured content exceeds aggregate budget"
            else
              Ok
                ( input + input_next
                , output + output_next
                , work + work_next
                , nodes + nodes_next ))
      in
      t.input_remaining <- t.input_remaining - input;
      t.output_remaining <- t.output_remaining - output;
      t.work_remaining <- t.work_remaining - work;
      t.nodes_remaining <- t.nodes_remaining - nodes;
      t.contents <- List.rev_append fresh t.contents;
      Ok ())
  ;;
end

type source =
  | Captured_only
  | Selected of Content_source.t

type t =
  { budget : Budget.t
  ; source : source
  ; side : Content_source.side
  ; remote : Developer_child_remote_content.t option
  ; check_current : unit -> (unit, P.Error.t) result
  ; checkpoint : unit -> unit
  ; mutable used : bool
  }

let create ~(budget : Budget.t) ~revision ~source ~side ~remote ~check_current ~checkpoint
  =
  if
    List.exists budget.contents ~f:(phys_equal revision)
    &&
    match source with
    | Captured_only -> true
    | Selected port -> Content_source.matches_revision port ~side revision
  then Ok { budget; source; side; remote; check_current; checkpoint; used = false }
  else invalid "child content source is not in the selected aggregate budget"
;;

let check t =
  let open Result.Let_syntax in
  let%bind () = t.check_current () in
  match t.source with
  | Captured_only -> Ok ()
  | Selected port -> Content_source.check port
;;

let debit_input t count =
  if count < 0 || count > t.budget.input_remaining
  then resource "child content input budget exhausted"
  else (
    t.budget.input_remaining <- t.budget.input_remaining - count;
    Ok ())
;;

let debit_output t count =
  if count < 0 || count > t.budget.output_remaining
  then resource "child content output budget exhausted"
  else (
    t.budget.output_remaining <- t.budget.output_remaining - count;
    Ok ())
;;

let debit_work t count =
  if count < 0 || count > t.budget.work_remaining
  then resource "child content work budget exhausted"
  else (
    t.budget.work_remaining <- t.budget.work_remaining - count;
    Ok ())
;;

let debit_nodes t count =
  if count < 0 || count > t.budget.nodes_remaining
  then resource "child content node budget exhausted"
  else (
    t.budget.nodes_remaining <- t.budget.nodes_remaining - count;
    Ok ())
;;

let read_attempt t ~f =
  let acquired = ref false in
  let returned = ref false in
  Exn.protect
    ~f:(fun () ->
      let result = f ~on_acquire:(fun () -> acquired := true) in
      returned := true;
      (match result with
       | Ok _ -> ()
       | Error _ -> if !acquired then t.budget.uncertain <- true);
      result)
    ~finally:(fun () -> if !acquired && not !returned then t.budget.uncertain <- true)
;;

let read_bytes t (item : CM.basic_content_item) =
  let open Result.Let_syntax in
  let%bind () = check t in
  let%bind port =
    match t.source with
    | Captured_only -> invalid "child content has no dynamic source admission"
    | Selected port -> Ok port
  in
  let%bind () =
    if t.budget.input_remaining <= 0 || t.budget.work_remaining <= 0
    then resource "child content aggregate read budget exhausted"
    else Ok ()
  in
  let%bind contents =
    if item.is_local
    then (
      let%bind found =
        Content_source.remote_origin
          port
          ~side:t.side
          ~item
          ~max_work_steps:t.budget.work_remaining
          ~checkpoint:t.checkpoint
      in
      let%bind () = debit_work t (Scope.Origin_read.work_steps found) in
      let%bind read =
        read_attempt t ~f:(fun ~on_acquire ->
          Content_source.read_local
            port
            ~side:t.side
            ~item
            ~max_bytes:(Int.min t.budget.input_remaining t.budget.work_remaining)
            ~max_work_steps:t.budget.work_remaining
            ~checkpoint:t.checkpoint
            ~on_acquire)
      in
      let%bind () = debit_work t (Scope.Content_read.work_steps read) in
      let%map () = debit_input t (Scope.Content_read.input_bytes read) in
      Scope.Content_read.contents read)
    else (
      let%bind remote =
        Option.value_map
          t.remote
          ~default:(Error (error Permission_denied "remote child content is disabled"))
          ~f:Result.return
      in
      let%bind found =
        Content_source.remote_origin
          port
          ~side:t.side
          ~item
          ~max_work_steps:t.budget.work_remaining
          ~checkpoint:t.checkpoint
      in
      let%bind () = debit_work t (Scope.Origin_read.work_steps found) in
      let origin = Scope.Origin_read.origin found in
      let%bind () =
        match CM.Content_origin.kind origin with
        | Image | Document CM.Document_occurrence.Selection.Dynamic -> Ok ()
        | Document (Local_capture | Builtin _) ->
          invalid "captured child document cannot be fetched again"
      in
      let%bind contents =
        read_attempt t ~f:(fun ~on_acquire ->
          Developer_child_remote_content.read
            remote
            ~url:(CM.Content_origin.locator origin)
            ~max_bytes:(Int.min t.budget.input_remaining t.budget.work_remaining)
            ~check_owner:(fun () ->
              let open Result.Let_syntax in
              let%bind () = check t in
              Content_source.check_dynamic port ~side:t.side)
            ~checkpoint:t.checkpoint
            ~on_acquire)
      in
      let%bind () = debit_input t (String.length contents) in
      let%map () = debit_work t (String.length contents) in
      contents)
  in
  let%map () = check t in
  contents
;;

let image_media_type contents =
  if String.is_prefix contents ~prefix:"\x89PNG\r\n\x1a\n"
  then Some "image/png"
  else if String.is_prefix contents ~prefix:"\xff\xd8\xff"
  then Some "image/jpeg"
  else if
    String.is_prefix contents ~prefix:"GIF87a"
    || String.is_prefix contents ~prefix:"GIF89a"
  then Some "image/gif"
  else if
    String.length contents >= 12
    && String.is_prefix contents ~prefix:"RIFF"
    && String.equal (String.sub contents ~pos:8 ~len:4) "WEBP"
  then Some "image/webp"
  else None
;;

let image t item =
  Budget.with_exclusive t.budget ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind contents = read_bytes t item in
    let%bind media_type =
      image_media_type contents
      |> Result.of_option
           ~error:(error Invalid_request "child image bytes have an unsupported format")
    in
    let prefix = "data:" ^ media_type ^ ";base64," in
    let encoded_length = (String.length contents + 2) / 3 * 4 in
    let total = String.length prefix + encoded_length in
    let%bind () = debit_work t total in
    let%bind () = debit_output t total in
    t.checkpoint ();
    let encoded = Base64.encode_exn contents in
    let%map () = check t in
    prefix ^ encoded)
;;

let document t (item : CM.basic_content_item) =
  Budget.with_exclusive t.budget ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind contents = read_bytes t item in
    let mode =
      if item.cleanup_html
      then C.Mode.Strip_html
      else if item.markdown
      then Html_to_markdown
      else Verbatim
    in
    let%bind limits =
      C.Limits.create
        ~max_input_bytes:(Int.max 1 (String.length contents))
        ~max_output_bytes:t.budget.output_remaining
        ~max_work_steps:t.budget.work_remaining
        ~max_nodes:t.budget.nodes_remaining
        ~max_depth:t.budget.limits.max_depth
      |> Result.map_error ~f:(fun _ ->
        error Resource_limit "child document conversion budget exhausted")
    in
    let%bind converted =
      read_attempt t ~f:(fun ~on_acquire ->
        on_acquire ();
        C.convert mode ~contents ~limits ~checkpoint:t.checkpoint
        |> Result.map_error ~f:(function
          | Invalid_html -> error Invalid_request "child document HTML is invalid"
          | Input_limit | Output_limit | Work_limit | Node_limit | Depth_limit ->
            error Resource_limit "child document conversion budget exhausted"))
    in
    let accounting = C.Output.accounting converted in
    let%bind () = debit_work t (C.Accounting.work_steps accounting) in
    let%bind () = debit_nodes t (C.Accounting.nodes accounting) in
    let%bind () = debit_output t (C.Accounting.output_bytes accounting) in
    let%map () = check t in
    C.Output.contents converted)
;;

let to_items t ~ctx ~agent elements =
  let open Result.Let_syntax in
  if t.used
  then invalid "child content projection was already used"
  else (
    t.used <- true;
    let%bind () = check t in
    let source =
      Chat_response.Converter_source.create ~image:(image t) ~document:(document t) ~agent
    in
    let%bind items =
      Chat_response.Converter.to_items_with_source
        ~source
        ~ctx
        ~max_output_bytes:t.budget.output_remaining
        ~max_source_bytes:t.budget.work_remaining
        ~remaining_output_bytes:(fun () ->
          match
            Budget.with_exclusive t.budget ~f:(fun () -> Ok t.budget.output_remaining)
          with
          | Ok remaining -> remaining
          | Error _ -> 0)
        ~charge_work:(fun count ->
          Budget.with_exclusive t.budget ~f:(fun () -> debit_work t count))
        ~charge_output:(fun count ->
          Budget.with_exclusive t.budget ~f:(fun () -> debit_output t count))
        ~checkpoint:t.checkpoint
        elements
    in
    let%map () = check t in
    items)
;;
