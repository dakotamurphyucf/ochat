open! Core
module Current = Chatml_authoring_context.Document.Inputs
module Sources = Authoring_source_host.Document_context
module Wire_source = Agent_protocol.Authoring.Source
module Chatmd = Chatmd_authoring_assistance
module Captured = Chatml_source
module Input = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module Error = Authoring_context_service_core.Error

exception Work_limit

type t =
  { requested_source : Captured.t
  ; requested_inputs : Input.t list
  ; unique_view_input : Input.t option
  ; parsed : Parsed.t
  ; captured_inputs : Chatmd.Captured_input.t list
  }

let requested_source t = t.requested_source
let requested_inputs t = t.requested_inputs
let unique_view_input t = t.unique_view_input
let parsed t = t.parsed
let captured_inputs t = t.captured_inputs

let source_id_bytes source =
  let id = Captured.id source in
  String.length (Captured.Id.package_namespace id)
  + String.length (Captured.Id.relative_path id)
;;

let same_source a b ~checkpoint =
  if
    (not (Captured.Id.equal (Captured.id a) (Captured.id b)))
    || (not (Captured.Digest.equal (Captured.digest a) (Captured.digest b)))
    || not (Int.equal (Captured.byte_length a) (Captured.byte_length b))
  then false
  else (
    let a = Captured.contents a in
    let b = Captured.contents b in
    let rec loop i =
      if i >= String.length a
      then true
      else (
        checkpoint ();
        let stop = Int.min (String.length a) (i + 4096) in
        let rec compare j =
          if j >= stop
          then loop stop
          else if Char.equal a.[j] b.[j]
          then compare (j + 1)
          else false
        in
        compare i)
    in
    loop 0)
;;

let admit
      ~current
      ~capture
      ~requested
      ~max_inputs
      ~max_source_bytes
      ~max_total_bytes
      ~max_work_steps
      ~checkpoint
  =
  if
    max_inputs <= 0
    || max_source_bytes <= 0
    || max_total_bytes <= 0
    || max_work_steps <= 0
    || List.exists
         [ max_inputs; max_source_bytes; max_total_bytes; max_work_steps ]
         ~f:(fun n -> n > Agent_protocol.Authoring.Limits.maximum_wire_integer)
  then Error Error.Resource_limit
  else (
    let remaining_work = ref max_work_steps in
    let tick () =
      if !remaining_work <= 0
      then Error Error.Resource_limit
      else (
        decr remaining_work;
        checkpoint ();
        Ok ())
    in
    let checkpoint_or_limit () =
      match tick () with
      | Ok () -> ()
      | Error _ -> raise Work_limit
    in
    let current_source = Current.source current in
    let captured_root = Sources.Parsed_capture.source capture in
    let pairs = Sources.Parsed_capture.input_sources capture in
    let receipt = Sources.Parsed_capture.parsed capture in
    let parsed = (Prompt.Chat_markdown.Generated.parsed receipt).parsed_root in
    let total_bytes = ref 0 in
    let count = ref 0 in
    let preflight () =
      let rec loop = function
        | [] -> Ok ()
        | (input, source) :: tail ->
          let open Result.Let_syntax in
          let%bind () = tick () in
          incr count;
          if !count > max_inputs
          then Error Error.Resource_limit
          else (
            let source_bytes = Captured.byte_length source in
            let authored_bytes = Input.authored_byte_length input in
            let effective_bytes = Input.effective_byte_length input in
            let charges =
              [ source_bytes
              ; authored_bytes
              ; effective_bytes
              ; source_id_bytes source
              ; String.length (Input.path input)
              ]
            in
            let rec charge = function
              | [] -> Ok ()
              | n :: rest ->
                if n < 0 || n > max_source_bytes || n > max_total_bytes - !total_bytes
                then Error Error.Resource_limit
                else (
                  total_bytes := !total_bytes + n;
                  charge rest)
            in
            let%bind () = charge charges in
            loop tail)
      in
      let root_charges =
        [ Captured.byte_length current_source
        ; source_id_bytes current_source
        ; Captured.byte_length captured_root
        ; source_id_bytes captured_root
        ]
      in
      let rec charge_roots = function
        | [] -> loop pairs
        | n :: rest ->
          if n < 0 || n > max_source_bytes || n > max_total_bytes - !total_bytes
          then Error Error.Resource_limit
          else (
            total_bytes := !total_bytes + n;
            charge_roots rest)
      in
      charge_roots root_charges
    in
    let open Result.Let_syntax in
    let%bind () = preflight () in
    try
      if not (same_source current_source captured_root ~checkpoint:checkpoint_or_limit)
      then Error Error.Stale_source
      else (
        let root_input = Parsed.root_input parsed in
        let root_seen = ref false in
        let by_id = ref (Map.empty (module Captured.Id)) in
        let requested_source = ref None in
        let requested_inputs = ref [] in
        let captured_inputs = ref [] in
        let rec gather = function
          | [] ->
            (match !requested_source, !root_seen with
             | Some source, true ->
               let inputs = List.rev !requested_inputs in
               let unique_view_input =
                 match inputs with
                 | [] -> None
                 | first :: rest ->
                   let identity =
                     Input.identity_digest first ~checkpoint:checkpoint_or_limit
                   in
                   if
                     List.for_all rest ~f:(fun input ->
                       checkpoint_or_limit ();
                       String.equal
                         identity
                         (Input.identity_digest input ~checkpoint:checkpoint_or_limit))
                   then Some first
                   else None
               in
               Ok
                 { requested_source = source
                 ; requested_inputs = inputs
                 ; unique_view_input
                 ; parsed
                 ; captured_inputs = List.rev !captured_inputs
                 }
             | None, true -> Error Error.Source_unavailable
             | _, false -> Error Error.Stale_source)
          | (input, source) :: tail ->
            let%bind () = tick () in
            let id = Captured.id source in
            let%bind () =
              match Map.find !by_id id with
              | None ->
                by_id := Map.set !by_id ~key:id ~data:source;
                Ok ()
              | Some prior ->
                if same_source prior source ~checkpoint:checkpoint_or_limit
                then Ok ()
                else Error Error.Stale_source
            in
            let%bind () =
              if phys_equal input root_input
              then (
                root_seen := true;
                if not (same_source source captured_root ~checkpoint:checkpoint_or_limit)
                then Error Error.Stale_source
                else Ok ())
              else Ok ()
            in
            let%bind captured_input =
              Chatmd.Captured_input.create
                ~input
                ~source
                ~max_bytes:max_source_bytes
                ~checkpoint:checkpoint_or_limit
              |> Result.map_error ~f:(function
                | Chatmd.Error.Source_mismatch -> Error.Stale_source
                | _ -> Error.Invalid_request)
            in
            captured_inputs := captured_input :: !captured_inputs;
            if Captured.Id.equal id (Wire_source.id requested)
            then
              if
                (not
                   (Captured.Digest.equal
                      (Captured.digest source)
                      (Wire_source.digest requested)))
                || (not
                      (Int.equal
                         (Captured.byte_length source)
                         (Wire_source.byte_length requested)))
                || ((not (Captured.Id.equal id (Captured.id current_source)))
                    && Option.is_some (Wire_source.editor_revision requested))
              then Error Error.Stale_source
              else (
                requested_source := Some source;
                requested_inputs := input :: !requested_inputs;
                gather tail)
            else gather tail
        in
        gather pairs)
    with
    | Work_limit -> Error Error.Resource_limit)
;;
