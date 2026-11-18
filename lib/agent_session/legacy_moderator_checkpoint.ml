open! Core
module CM = Prompt.Chat_markdown

type t =
  | No_migration
  | Rebuild_required
[@@deriving equal, sexp_of]

let incompatible () =
  Error
    (Agent_protocol.Error.create
       Invalid_state
       ~message:"historical moderator checkpoint does not match captured Single source"
       ~retryable:false
       ())
;;

let is_lower_hex text =
  String.for_all text ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let classify revision ~checkpoint ~poll =
  let open Result.Let_syntax in
  poll ();
  let%bind snapshot = Moderator_checkpoint.decode checkpoint in
  match snapshot with
  | None -> Ok No_migration
  | Some snapshot when String.length snapshot.script_source_hash <> 32 -> Ok No_migration
  | Some snapshot ->
    let artifact = Prompt_revision.artifact revision in
    if
      (not (is_lower_hex snapshot.script_source_hash))
      || Option.is_some (Prompt_revision.parser_context revision)
      || Option.is_some (Prompt_revision.programs revision)
      || Option.is_some (Prompt_revision.content revision)
      || Option.is_some artifact.program_bundle
      || Option.is_some artifact.content_bundle
    then incompatible ()
    else (
      (* Registry.prepare_elements/of_elements select the final legacy Script. *)
      let selected =
        List.fold
          (Prompt_revision.elements revision)
          ~init:None
          ~f:(fun selected -> function
          | CM.Script script ->
            poll ();
            Some script
          | _ ->
            poll ();
            selected)
      in
      match selected with
      | None -> incompatible ()
      | Some script ->
        if
          Option.is_some script.project_selection
          || not (String.equal script.id snapshot.script_id)
        then incompatible ()
        else (
          let source =
            match script.source with
            | Inline source -> source
            | Src { source_text; _ } -> source_text
          in
          poll ();
          let historical_hash = Md5.digest_string source |> Md5.to_hex in
          poll ();
          if String.equal historical_hash snapshot.script_source_hash
          then Ok Rebuild_required
          else incompatible ()))
;;
