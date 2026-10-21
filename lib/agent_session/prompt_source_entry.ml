open Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Receipt = Agent_store.Prompt_source_receipt
module Path = Source.Portable_path

type t =
  { definition : Prompt_definition.t
  ; receipt : Receipt.t
  ; member : Path.t
  ; origin : string * string * int * int
  ; selected_parent : (Agent_protocol.Id.Prompt_revision.t * string) option
  ; work_steps : int
  }

let definition t = t.definition
let receipt t = t.receipt
let member { member; origin = _; _ } = member
let work_steps t = t.work_steps

let matches_selected_parent t parent =
  match t.selected_parent with
  | None -> false
  | Some (revision_id, manifest_sha256) ->
    Agent_protocol.Id.Prompt_revision.equal revision_id (Prompt_revision.id parent)
    && String.equal manifest_sha256 (Prompt_revision.artifact parent).manifest_sha256
;;

let of_reference ~parent reference ~max_work_steps ~checkpoint =
  let open Or_error.Let_syntax in
  if max_work_steps <= 0
  then Or_error.error_string "Selected-member admission work allowance exhausted"
  else (
    let used = ref 0 in
    let charge () =
      if !used >= max_work_steps
      then Or_error.error_string "Selected-member admission work allowance exhausted"
      else (
        checkpoint ();
        Int.incr used;
        Ok ())
    in
    let%bind () = charge () in
    let%bind parsed =
      Prompt_revision.parser_context parent
      |> Or_error.of_option
           ~error:
             (Error.of_string "Selected member requires authentic owned parser input")
    in
    let%bind receipt =
      (Prompt_revision.artifact parent).source_receipt
      |> Or_error.of_option
           ~error:(Error.of_string "Selected member requires original source selection")
    in
    let definition = Prompt_revision.definition parent in
    let%bind () = charge () in
    let%bind () =
      if
        Agent_protocol.Id.Prompt_definition.equal
          definition.id
          (Receipt.definition_id receipt)
        && String.equal definition.root_file (Receipt.root_file receipt)
      then Ok ()
      else Or_error.error_string "Selected member has a foreign original selection"
    in
    let%bind occurrences =
      List.fold_result
        (CM.Parsed.agent_references parsed)
        ~init:0
        ~f:(fun count candidate ->
          let%map () = charge () in
          if phys_equal reference candidate then count + 1 else count)
    in
    let%bind () =
      if Int.equal occurrences 1
      then Ok ()
      else Or_error.error_string "Agent reference is not owned by this parsed revision"
    in
    let portable value =
      let%bind () = charge () in
      if String.length value > 1024
      then Or_error.error_string "Selected-member portable path exceeds 1024 bytes"
      else Path.create value
    in
    let%bind queued_key =
      CM.Agent_reference.queued_key reference
      |> Or_error.of_option
           ~error:(Error.of_string "Agent reference has no relative local queued member")
    in
    let%bind member = portable queued_key in
    let root = CM.Parsed.root_input parsed in
    let range = CM.Agent_reference.range reference in
    let declaring = Input.Range.input range in
    let%bind _root_path = portable (Input.path root) in
    let%bind _declaring_path = portable (Input.path declaring) in
    let%bind () = charge () in
    let root_identity = Input.identity_digest root ~checkpoint in
    let%bind () = charge () in
    let declaring_identity = Input.identity_digest declaring ~checkpoint in
    let%map () = charge () in
    { definition
    ; receipt
    ; member
    ; origin =
        ( root_identity
        , declaring_identity
        , Input.Range.start_byte range
        , Input.Range.end_byte range )
    ; selected_parent = None
    ; work_steps = !used
    })
;;

let of_selected_member ~parent ~member ~max_work_steps ~checkpoint =
  let open Or_error.Let_syntax in
  if max_work_steps <= 0
  then Or_error.error_string "Selected-member admission work allowance exhausted"
  else (
    let used = ref 0 in
    let charge () =
      if !used >= max_work_steps
      then Or_error.error_string "Selected-member admission work allowance exhausted"
      else (
        checkpoint ();
        Int.incr used;
        Ok ())
    in
    let%bind () = charge () in
    let%bind parsed =
      Prompt_revision.parser_context parent
      |> Or_error.of_option
           ~error:(Error.of_string "Selected Model member requires owned parser input")
    in
    let artifact = Prompt_revision.artifact parent in
    let%bind receipt =
      artifact.source_receipt
      |> Or_error.of_option
           ~error:(Error.of_string "Selected Model member requires original source")
    in
    let definition = Prompt_revision.definition parent in
    let%bind () = charge () in
    let%bind () =
      if
        Agent_protocol.Id.Prompt_definition.equal
          definition.id
          (Receipt.definition_id receipt)
        && String.equal definition.root_file (Receipt.root_file receipt)
      then Ok ()
      else Or_error.error_string "Selected Model member has foreign original selection"
    in
    let root = CM.Parsed.root_input parsed in
    let%bind () = charge () in
    let%bind _root_path = Path.create (Input.path root) in
    let%bind () = charge () in
    let root_identity = Input.identity_digest root ~checkpoint in
    let%map () = charge () in
    { definition
    ; receipt
    ; member
    ; origin = root_identity, root_identity, 0, 0
    ; selected_parent = Some (artifact.revision_id, artifact.manifest_sha256)
    ; work_steps = !used
    })
;;
