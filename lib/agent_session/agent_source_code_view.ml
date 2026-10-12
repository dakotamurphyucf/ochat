open Core
module CM = Prompt.Chat_markdown
module R = CM.Agent_reference

module Context = struct
  type t =
    { revision : Prompt_revision.t
    ; reference : R.t
    }

  let revision t = t.revision
  let reference t = t.reference
end

type association =
  { implementation : Ochat_function.t
  ; context : Context.t
  }

type t =
  { original : Prompt_revision.t
  ; selected : Prompt_revision.t
  ; associations : association list
  ; closed : bool Atomic.t
  ; domain_id : int
  }

let error message = Error (Agent_protocol.Error.invalid_request message)

let create ~original ~selected ~native ~sw ~max_work_steps ~checkpoint =
  let exception Rejected in
  let work = ref 0 in
  let poll () =
    if !work >= max_work_steps then raise Rejected;
    incr work;
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ()
  in
  let equal_string left right =
    poll ();
    if not (Int.equal (String.length left) (String.length right))
    then false
    else (
      let rec loop offset =
        if offset = String.length left
        then true
        else (
          poll ();
          let finish = Int.min (offset + 4096) (String.length left) in
          let rec chunk index =
            index = finish || (Char.equal left.[index] right.[index] && chunk (index + 1))
          in
          chunk offset && loop finish)
      in
      loop 0)
  in
  let equal_option f left right =
    match left, right with
    | None, None -> true
    | Some left, Some right -> f left right
    | None, Some _ | Some _, None -> false
  in
  let bounded_filter items ~f =
    let rec loop count reversed = function
      | [] -> Ok (List.rev reversed)
      | item :: rest ->
        poll ();
        if count >= 16_384
        then error "too many selected native source declarations"
        else (
          let reversed =
            match f item with
            | None -> reversed
            | Some selected -> selected :: reversed
          in
          loop (count + 1) reversed rest)
    in
    loop 0 [] items
  in
  let reference_tool reference =
    match R.invocation reference with
    | Tool tool -> Some (tool, None, reference)
    | Persistent_tool (tool, persistence) -> Some (tool, Some persistence, reference)
    | Content _ -> None
  in
  let open Result.Let_syntax in
  if max_work_steps <= 0
  then error "selected native source work limit must be positive"
  else (
    try
      poll ();
      Eio.Switch.check sw;
      let%bind original_parsed, selected_parsed =
        match
          Prompt_revision.parser_context original, Prompt_revision.parser_context selected
        with
        | Some original, Some selected -> Ok (original, selected)
        | None, _ | _, None ->
          error "selected native source requires authentic parser owners"
      in
      let%bind original_references =
        bounded_filter (CM.Parsed.agent_references original_parsed) ~f:reference_tool
      in
      let%bind selected_references =
        bounded_filter (CM.Parsed.agent_references selected_parsed) ~f:reference_tool
      in
      let%bind functions =
        bounded_filter native.Chat_response.Agent_runtime.functions ~f:Option.some
      in
      let find_function name =
        let rec loop found = function
          | [] ->
            Result.of_option
              found
              ~error:
                (Agent_protocol.Error.invalid_request
                   "original native source function is missing")
          | implementation :: rest ->
            poll ();
            if equal_string name implementation.Ochat_function.info.function_.name
            then (
              match found with
              | None -> loop (Some implementation) rest
              | Some _ -> error "original native source function is ambiguous")
            else loop found rest
        in
        loop None functions
      in
      let rec associate reversed originals selections =
        match originals, selections with
        | [], [] -> Ok (List.rev reversed)
        | ( (old_tool, old_persistence, old_reference) :: originals
          , (tool, persistence, reference) :: selections ) ->
          poll ();
          let%bind () =
            if
              equal_string old_tool.CM.name tool.CM.name
              && equal_option equal_string old_tool.description tool.description
              && Bool.equal old_tool.is_local tool.is_local
              && Option.equal CM.equal_agent_persistence old_persistence persistence
              && equal_option
                   equal_string
                   (R.queued_key old_reference)
                   (R.queued_key reference)
              && (Option.is_some (R.queued_key old_reference)
                  || equal_string old_tool.agent tool.agent)
            then Ok ()
            else error "selected native source changes the original tool interface/member"
          in
          let%bind implementation = find_function old_tool.name in
          let%bind () =
            let rec duplicate = function
              | [] -> Ok ()
              | association :: rest ->
                poll ();
                if phys_equal association.implementation implementation
                then error "selected native source repeats an original implementation"
                else duplicate rest
            in
            duplicate reversed
          in
          associate
            ({ implementation; context = { Context.revision = selected; reference } }
             :: reversed)
            originals
            selections
        | [], _ :: _ | _ :: _, [] -> error "selected native source declarations changed"
      in
      let%bind associations = associate [] original_references selected_references in
      poll ();
      Eio.Switch.check sw;
      let closed = Atomic.make false in
      Eio.Switch.on_release sw (fun () -> Atomic.set closed true);
      Ok
        { original
        ; selected
        ; associations
        ; closed
        ; domain_id = (Stdlib.Domain.self () :> int)
        }
    with
    | Rejected -> error "selected native source work limit exceeded")
;;

let check t =
  Eio.Fiber.check ();
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then error "selected native source belongs to another domain"
  else if Atomic.get t.closed
  then error "selected native source is closed"
  else Ok ()
;;

let find_for_preparation t ~implementation =
  let open Result.Let_syntax in
  let%bind () = check t in
  let rec loop = function
    | [] -> None
    | association :: rest ->
      Eio.Fiber.check ();
      if phys_equal association.implementation implementation
      then Some association.context
      else loop rest
  in
  let context = loop t.associations in
  let%map () = check t in
  context
;;

let find t ~defining_revision ~implementation =
  let open Result.Let_syntax in
  let%bind context = find_for_preparation t ~implementation in
  match context with
  | None -> Ok None
  | Some context ->
    if phys_equal defining_revision t.selected
    then Ok None
    else if phys_equal defining_revision t.original
    then Ok (Some context)
    else error "associated native source has a foreign defining revision"
;;
