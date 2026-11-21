open! Core
module P = Agent_protocol
module O = Recovery_checkpoint_opportunity
module R = Inference_runtime
module Catalog = Recovery_notes.Catalog
module Scope = P.Recovery_window.Scope
module Authored = Recovery_authored_checkpoint
module Tools = Chat_response.Tool_capability
module Note = Recovery_note_access.Effect

type t =
  { scope : Scope.t
  ; key : string
  ; recipe : O.recipe
  ; notes : Catalog.selection list
  }

let exact_native registry ~name ~revision ~contract =
  Result.ok (Tools.find registry ~name)
  |> Option.exists ~f:(fun binding ->
    String.equal (Tools.reference binding).implementation_revision revision
    && Tools.equal_result_contract (Tools.result_contract binding) contract
    && Option.is_some (Tools.native_implementation binding))
;;

let checkpoint_selected registry =
  exact_native
    registry
    ~name:Authored.name
    ~revision:Authored.implementation_revision
    ~contract:Invocation_v1
;;

let selected_tools registry =
  if not (checkpoint_selected registry)
  then Chat_response.Soft_checkpoint.Unselected
  else if
    exact_native
      registry
      ~name:(Note.name Write)
      ~revision:(Note.implementation_revision Write)
      ~contract:Protected_recovery_v1
  then Register_and_write
  else Register
;;

let create ~scope ~id ~(tools : Chat_response.Soft_checkpoint.tools) catalog =
  let open Option.Let_syntax in
  let key = P.Id.Operation.to_string id in
  let notes = List.map (Catalog.list catalog) ~f:(fun row -> row.selection) in
  match tools, notes with
  | Unselected, _ | Register, [] -> None
  | (Register | Register_and_write), _ :: _ ->
    Some { scope; key; recipe = Register; notes }
  | Register_and_write, [] ->
    let window = P.Id.Context_window.to_string (Scope.window scope) in
    let suffix =
      Chatml_source.Digest.of_contents window ~checkpoint:ignore
      |> Chatml_source.Digest.to_hex
      |> fun digest -> String.prefix digest 48
    in
    let%map path =
      Catalog.Note_path.of_string ("context-checkpoints/" ^ suffix) |> Result.ok
    in
    { scope
    ; key
    ; recipe = Create_note { catalog = Catalog.revision catalog; path }
    ; notes
    }
;;

let recipe t = t.recipe

let name t =
  match t.recipe with
  | Register -> Authored.name
  | Create_note _ -> Note.name Write
;;

let instructions t =
  let frame =
    "This single request can preserve essential work before the host changes the active \
     context window. Use the one selected local tool exactly once. Emit no prose or \
     additional calls. Follow the authored instructions."
  in
  let action =
    match t.recipe with
    | Register ->
      let coordinate (selected : Catalog.selection) =
        sprintf
          "{\"id\":\"%Ld\",\"revision\":\"%Ld\"}"
          (Catalog.Note_id.to_int64 selected.id)
          (Catalog.Note_revision.to_int64 selected.revision)
      in
      sprintf
        "Register useful committed local notes with mode \"register\", expected_window \
         %S, and key %S. Select a nonempty subset of these exact note revisions: [%s]."
        (P.Id.Context_window.to_string (Scope.window t.scope))
        t.key
        (String.concat ~sep:"," (List.map t.notes ~f:coordinate))
    | Create_note { catalog; path } ->
      sprintf
        "Create a new local note with kind \"create\", path %S, \
         expected_catalog_revision \"%Ld\", expected null, and nonblank content. Record \
         decisions, completed work, and concrete remaining work supported by this \
         request."
        (Catalog.Note_path.to_string path)
        (Catalog.Catalog_revision.to_int64 catalog)
  in
  frame ^ "\n" ^ action
;;

let project t request =
  let open Option.Let_syntax in
  let%bind tool =
    List.find (Inference.Request.tools request) ~f:(fun tool ->
      String.equal (Inference.Request.Tool_spec.name tool) (name t)
      && History_entry.Payload.Call_kind.equal
           (Inference.Request.Tool_spec.kind tool)
           Function)
  in
  let module G = Inference.Request.Guidance in
  let previous = Inference.Request.guidance request in
  let text =
    Option.value_map previous ~default:(instructions t) ~f:(fun prior ->
      G.text prior ^ "\n\n" ^ instructions t)
  in
  let limits = Transcript.Admission.default in
  let provenance =
    Option.value_map previous ~default:"host authored checkpoint" ~f:G.provenance
  in
  let%bind guidance = G.create ~text ~provenance ~limits |> Result.ok in
  Inference.Request.with_single_tool request ~tool ~guidance:(Some guidance) ~limits
  |> Result.ok
;;

let matches_create recipe mutation =
  match recipe, mutation with
  | O.Create_note selected, Catalog.Create { expected_catalog_revision; path; content } ->
    Catalog.Catalog_revision.equal selected.catalog expected_catalog_revision
    && Catalog.Note_path.equal selected.path path
    && not (String.is_empty (String.strip (Catalog.Content.to_string content)))
  | _ -> false
;;

let exact_notes t notes =
  List.for_all notes ~f:(fun note -> List.mem t.notes note ~equal:Catalog.equal_selection)
  && not
       (List.contains_dup notes ~compare:(fun a b ->
          Catalog.Note_id.compare a.Catalog.id b.Catalog.id))
;;

let accepts t ~name:call_name ~arguments =
  String.equal call_name (name t)
  &&
  match t.recipe with
  | Create_note _ ->
    (match Recovery_note_native_request.decode_text arguments with
     | Error _ -> false
     | Ok json ->
       (match Recovery_note_native_request.decode ~effect_kind:Write json with
        | Ok (Mutate mutation) -> matches_create t.recipe mutation
        | Ok (Query _ | Shared_read _) | Error _ -> false))
  | Register ->
    if String.length arguments > 16_384
    then false
    else (
      match Jsonaf.of_string arguments with
      | exception _ -> false
      | json ->
        (match Authored.Request.of_json json with
         | Ok { window; key; action = Register notes } ->
           P.Id.Context_window.equal window (Scope.window t.scope)
           && Chatml_source.Digest.equal
                key
                (Chatml_source.Digest.of_contents t.key ~checkpoint:ignore)
           && exact_notes t notes
         | Ok { action = Readback; _ } | Error _ -> false))
;;

let pressure policy prepared =
  let module P = R.Prepared in
  let assessment = P.assess_capacity ~policy prepared in
  if
    P.binding_guard prepared ()
    && P.capacity_assessment_is_current ~policy prepared assessment
  then (
    match R.Context_capacity.Assessment.view assessment with
    | Assessed { pressure; _ } -> Some pressure
    | Unavailable _ -> None)
  else None
;;
