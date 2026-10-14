open Core
module Custody = Chatml_prompt_programs
module Plan = Chat_response.Script_contract_plan
module Slot = Custody.Binding.Slot
module Role = Plan.Program.Role
module Id = Agent_protocol.Id
module Receipt = Agent_store.Prompt_source_receipt
module Storage = Agent_store.Prompt_program_bundle

module Limits = struct
  type t =
    { definitions : int
    ; references : int
    ; identity_bytes : int
    ; work_steps : int
    ; document : Document_schema.Limits.t
    }

  let create
        ~max_definitions
        ~max_references
        ~max_identity_bytes
        ~max_work_steps
        ~document
    =
    if
      max_definitions < 0
      || max_references < 0
      || max_identity_bytes <= 0
      || max_work_steps <= 0
    then Or_error.error_string "Invalid project baseline limits"
    else
      Ok
        { definitions = max_definitions
        ; references = max_references
        ; identity_bytes = max_identity_bytes
        ; work_steps = max_work_steps
        ; document
        }
  ;;

  let max_definitions t = t.definitions
  let max_references t = t.references
  let max_identity_bytes t = t.identity_bytes
  let max_work_steps t = t.work_steps
  let document t = t.document
end

module Error = struct
  type t =
    | Limit_exceeded
    | Invalid_association of string
    | Ambiguous_association
    | Foreign_artifact
    | Foreign_operator_intent
    | Missing_program_bundle
    | Document of Document_schema.Error.t
    | Custody of Custody.Error.t
    | Retained of Custody.Retained.Error.t
end

module Work = struct
  type t =
    { mutable remaining : int
    ; checkpoint : unit -> unit
    ; reject : unit -> unit
    }

  let poll t =
    if t.remaining <= 0 then t.reject ();
    t.remaining <- t.remaining - 1;
    t.checkpoint ()
  ;;

  let remaining t = t.remaining

  let string_equal t left right =
    poll t;
    if String.length left <> String.length right
    then false
    else (
      let offset = ref 0 in
      let same = ref true in
      while !same && !offset < String.length left do
        poll t;
        let stop = !offset + Int.min 4096 (String.length left - !offset) in
        while !same && !offset < stop do
          same := Char.equal left.[!offset] right.[!offset];
          incr offset
        done
      done;
      !same)
  ;;

  let run limits ~checkpoint ~f =
    let module Stop = struct
      exception Limit
    end
    in
    let work =
      { remaining = Limits.max_work_steps limits
      ; checkpoint
      ; reject = (fun () -> raise Stop.Limit)
      }
    in
    try f work with
    | Stop.Limit -> Error Error.Limit_exceeded
  ;;
end

let map_result values ~f =
  Result.map
    (List.fold_result values ~init:[] ~f:(fun reversed value ->
       Result.map (f value) ~f:(fun value -> value :: reversed)))
    ~f:List.rev
;;

let bounded_count work values maximum =
  let count = ref 0 in
  List.iter values ~f:(fun _ ->
    Work.poll work;
    if !count < maximum then incr count else work.Work.reject ());
  !count
;;

let valid_digest value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let valid_program_key value =
  String.length value = 129
  && Char.equal value.[64] ':'
  && String.for_alli value ~f:(fun index character ->
    if index = 64
    then true
    else (
      match character with
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false))
;;

let check_identity work limits value =
  Work.poll work;
  if String.length value > Limits.max_identity_bytes limits
  then Error Error.Limit_exceeded
  else Ok ()
;;

let role_equal work left right =
  Work.poll work;
  match left, right with
  | Role.Native_hook left_hook, Role.Native_hook right_hook ->
    Work.string_equal work left_hook.entrypoint right_hook.entrypoint
    && Role.equal left right
  | ( (Standard _ | Legacy_moderator _ | Native_hook _)
    , (Standard _ | Legacy_moderator _ | Native_hook _) ) -> Role.equal left right
;;

module Slot_assertion = struct
  type t =
    { registration_path : Source.Portable_path.t
    ; declaration_role : Custody.Parser_input.Role.t
    ; declaration_path : Source.Portable_path.t
    ; script_id : string
    }

  let of_slot slot =
    { registration_path = Slot.registration_path slot
    ; declaration_role = Slot.declaration_role slot
    ; declaration_path = Slot.declaration_path slot
    ; script_id = Slot.script_id slot
    }
  ;;

  let create
        ~registration_path
        ~declaration_role
        ~declaration_path
        ~script_id
        ~limits
        ~checkpoint
    =
    Work.run limits ~checkpoint ~f:(fun work ->
      let open Result.Let_syntax in
      let%map () = check_identity work limits script_id in
      { registration_path; declaration_role; declaration_path; script_id })
  ;;

  let registration_path t = t.registration_path
  let declaration_role t = t.declaration_role
  let declaration_path t = t.declaration_path
  let script_id t = t.script_id

  let matches work t slot =
    Custody.Parser_input.Role.equal t.declaration_role (Slot.declaration_role slot)
    && Work.string_equal
         work
         (Source.Portable_path.to_string t.registration_path)
         (Source.Portable_path.to_string (Slot.registration_path slot))
    && Work.string_equal
         work
         (Source.Portable_path.to_string t.declaration_path)
         (Source.Portable_path.to_string (Slot.declaration_path slot))
    && Work.string_equal work t.script_id (Slot.script_id slot)
  ;;

  let equal work left right =
    Custody.Parser_input.Role.equal left.declaration_role right.declaration_role
    && Work.string_equal
         work
         (Source.Portable_path.to_string left.registration_path)
         (Source.Portable_path.to_string right.registration_path)
    && Work.string_equal
         work
         (Source.Portable_path.to_string left.declaration_path)
         (Source.Portable_path.to_string right.declaration_path)
    && Work.string_equal work left.script_id right.script_id
  ;;
end

module Reference = struct
  type t =
    { association_key : string
    ; slot : Slot_assertion.t
    ; role : Role.t
    ; program_key : string
    }

  let association_key t = t.association_key
  let slot t = t.slot
  let role t = t.role
  let program_key t = t.program_key

  let role_fields = function
    | Role.Standard target ->
      [ "standard"
      ; (match target with
         | One_off_v1 -> "one_off_v1"
         | Tool_v1 -> "tool_v1"
         | Developer_tool_v1 -> "developer_tool_v1"
         | Moderator_v1 -> "moderator_v1"
         | Delegated_moderator_v1 -> "delegated_moderator_v1")
      ]
    | Legacy_moderator invocation ->
      [ "legacy_moderator"
      ; (match invocation with
         | Ui -> "ui"
         | Headless -> "headless")
      ]
    | Native_hook { kind; entrypoint } ->
      [ "native_hook"
      ; Chatmd_shell_spec.Chatmd_script_spec.kind_to_string kind
      ; entrypoint
      ]
  ;;

  let create ~slot ~role ~program_key ~limits ~checkpoint =
    Work.run limits ~checkpoint ~f:(fun work ->
      let open Result.Let_syntax in
      let%bind () = check_identity work limits (Slot_assertion.script_id slot) in
      let%bind () =
        match role with
        | Role.Native_hook { entrypoint; _ } -> check_identity work limits entrypoint
        | Standard _ | Legacy_moderator _ -> Ok ()
      in
      if not (valid_program_key program_key)
      then Error (Error.Invalid_association "Invalid retained program key")
      else (
        let fields =
          [ "ochat.script-project-slot-role.v1"
          ; Source.Portable_path.to_string (Slot_assertion.registration_path slot)
          ; (match Slot_assertion.declaration_role slot with
             | Root -> "root"
             | Import -> "import")
          ; Source.Portable_path.to_string (Slot_assertion.declaration_path slot)
          ; Slot_assertion.script_id slot
          ]
          @ role_fields role
        in
        let state = ref Digestif.SHA256.empty in
        List.iter fields ~f:(fun bytes ->
          Work.poll work;
          state
          := Digestif.SHA256.feed_string !state (Int.to_string (String.length bytes) ^ ":");
          let offset = ref 0 in
          while !offset < String.length bytes do
            Work.poll work;
            let len = Int.min 4096 (String.length bytes - !offset) in
            state := Digestif.SHA256.feed_string !state ~off:!offset ~len bytes;
            offset := !offset + len
          done);
        Ok
          { association_key = Digestif.SHA256.(get !state |> to_hex)
          ; slot
          ; role
          ; program_key
          }))
  ;;

  let same_association work left right =
    Slot_assertion.equal work left.slot right.slot && role_equal work left.role right.role
  ;;
end

module Prepared = struct
  type t =
    { custody : Custody.t
    ; references : Reference.t list
    }

  let custody t = t.custody

  let create custody ~capture ~plans ~limits ~checkpoint =
    Work.run limits ~checkpoint ~f:(fun work ->
      let open Result.Let_syntax in
      let count = ref 0 in
      List.iter plans ~f:(fun plan ->
        Work.poll work;
        List.iter (Plan.programs plan) ~f:(fun _ ->
          Work.poll work;
          if !count >= Limits.max_references limits then work.Work.reject ();
          incr count));
      let root = Prompt_revision_builder.Capture.root capture in
      let contexts =
        root :: List.map (Prompt_revision_builder.Capture.agents capture) ~f:snd
      in
      let%bind () =
        Custody.validate_capture
          custody
          ~parsed_contexts:contexts
          ~root:(Prompt.Chat_markdown.Parsed.root_input root)
          ~limits:(Prompt_program_custody.limits ())
          ~checkpoint:(fun () -> Work.poll work)
        |> Result.map_error ~f:(fun error -> Error.Custody error)
      in
      let%bind retained =
        Custody.retained
          custody
          ~max_work_steps:(Work.remaining work)
          ~checkpoint:(fun () -> Work.poll work)
        |> Result.map_error ~f:(fun error -> Error.Retained error)
      in
      let references = ref [] in
      let add program =
        let binding = Plan.Program.binding program in
        let%bind () = check_identity work limits (Custody.Binding.script_id binding) in
        let%bind key =
          let matching = ref None in
          let ambiguous = ref false in
          List.iter (Custody.Retained.programs retained) ~f:(fun previous ->
            Work.poll work;
            let other = Custody.Retained.Program.binding previous in
            if
              Work.string_equal
                work
                (Custody.Binding.script_id binding)
                (Custody.Binding.script_id other)
              && Custody.Binding.equal binding other
              && Chatml_source.Digest.equal
                   (Plan.Program.compiler_contract program)
                   (Custody.Retained.Program.compiler_contract previous)
            then
              if Option.is_some !matching
              then ambiguous := true
              else matching := Some (Custody.Retained.Program.key previous));
          if !ambiguous
          then Error Error.Ambiguous_association
          else (
            match !matching with
            | Some key -> Ok key
            | None ->
              Error
                (Error.Invalid_association "Plan program is absent from retained custody"))
        in
        let%bind reference =
          Reference.create
            ~slot:(Slot_assertion.of_slot (Custody.Binding.slot binding))
            ~role:(Plan.Program.role program)
            ~program_key:key
            ~limits
            ~checkpoint:(fun () -> Work.poll work)
        in
        match List.find !references ~f:(Reference.same_association work reference) with
        | None ->
          references := reference :: !references;
          Ok ()
        | Some previous ->
          if Work.string_equal work (Reference.program_key previous) key
          then Ok ()
          else Error Error.Ambiguous_association
      in
      let%map () =
        List.fold_result plans ~init:() ~f:(fun () plan ->
          Work.poll work;
          if
            not
              (List.exists contexts ~f:(fun context ->
                 Work.poll work;
                 phys_equal context (Plan.parsed plan)))
          then
            Error
              (Error.Invalid_association
                 "Plan is not owned by the captured parser context")
          else
            List.fold_result (Plan.programs plan) ~init:() ~f:(fun () program ->
              add program))
      in
      { custody; references = List.rev !references })
  ;;
end

module Entry = struct
  type t =
    { definition_id : Id.Prompt_definition.t
    ; source_receipt : Receipt.t
    ; revision_id : Id.Prompt_revision.t
    ; manifest_sha256 : string
    ; references : Reference.t list
    }

  let definition_id t = t.definition_id
  let source_receipt t = t.source_receipt
  let revision_id t = t.revision_id
  let manifest_sha256 t = t.manifest_sha256
  let references t = t.references

  let of_fields
        ~definition_id
        ~source_receipt
        ~revision_id
        ~manifest_sha256
        ~references
        ~limits
        ~checkpoint
    =
    Work.run limits ~checkpoint ~f:(fun work ->
      ignore (bounded_count work references (Limits.max_references limits) : int);
      if
        not
          (Id.Prompt_definition.equal
             definition_id
             (Receipt.definition_id source_receipt))
      then Error Error.Foreign_operator_intent
      else if not (valid_digest manifest_sha256)
      then Error (Error.Invalid_association "Invalid artifact manifest digest")
      else (
        let rec unique = function
          | [] -> Ok ()
          | reference :: rest ->
            Work.poll work;
            if
              List.exists rest ~f:(fun other ->
                Work.poll work;
                Work.string_equal
                  work
                  (Reference.association_key reference)
                  (Reference.association_key other)
                || Reference.same_association work reference other)
            then Error Error.Ambiguous_association
            else unique rest
        in
        let%map.Result () = unique references in
        { definition_id; source_receipt; revision_id; manifest_sha256; references }))
  ;;

  module Prior = struct
    type row =
      { reference : Reference.t
      ; slot : Slot.t
      ; input : Custody.Retained.Program.input
      }

    type t = row list

    type selection =
      | New_slot
      | Provided
      | Selected of Chatml_source.t

    let find t ~slot ~role ~limits ~checkpoint =
      Work.run limits ~checkpoint ~f:(fun work ->
        let matches = ref None in
        let duplicate = ref false in
        List.iter t ~f:(fun row ->
          Work.poll work;
          if
            Slot.matches row.slot slot ~checkpoint:(fun () -> Work.poll work)
            && role_equal work (Reference.role row.reference) role
          then
            if Option.is_some !matches
            then duplicate := true
            else matches := Some row.input);
        if !duplicate
        then Error Error.Ambiguous_association
        else
          Ok
            (match !matches with
             | None -> New_slot
             | Some Provided -> Provided
             | Some (Selected source) -> Selected source))
    ;;
  end

  let reconcile t ~artifact ~custody_limits ~limits ~checkpoint =
    Work.run limits ~checkpoint ~f:(fun work ->
      let open Result.Let_syntax in
      let module Artifact = Agent_store.Prompt_artifact_store.Artifact in
      if
        (not (Id.Prompt_revision.equal t.revision_id artifact.Artifact.revision_id))
        || (not (Work.string_equal work t.manifest_sha256 artifact.manifest_sha256))
        || not
             (Option.equal
                Id.Prompt_definition.equal
                (Some t.definition_id)
                artifact.prompt_definition_id)
      then Error Error.Foreign_artifact
      else if
        not
          (match artifact.source_receipt with
           | None -> false
           | Some receipt ->
             Id.Prompt_definition.equal
               (Receipt.definition_id t.source_receipt)
               (Receipt.definition_id receipt)
             && Work.string_equal
                  work
                  (Receipt.root_file t.source_receipt)
                  (Receipt.root_file receipt))
      then Error Error.Foreign_operator_intent
      else (
        match artifact.program_bundle with
        | None -> Error Error.Missing_program_bundle
        | Some bundle ->
          let%bind authored =
            Prompt_program_custody.authored
              ~root_file:artifact.root_relative_path
              ~sources:
                ((artifact.root_relative_path, artifact.root_chatmd)
                 :: List.map artifact.sources ~f:(fun source ->
                   Work.poll work;
                   ( source.Agent_store.Prompt_artifact_store.Source.relative_path
                   , source.contents )))
            |> Result.map_error ~f:(fun error ->
              Error.Invalid_association
                (String.prefix (Core.Error.to_string_hum error) 256))
          in
          let%bind custody =
            Custody.of_storage
              bundle
              ~authored
              ~limits:custody_limits
              ~checkpoint:(fun () -> Work.poll work)
            |> Result.map_error ~f:(fun error -> Error.Custody error)
          in
          let%bind retained =
            Custody.retained
              custody
              ~max_work_steps:(Work.remaining work)
              ~checkpoint:(fun () -> Work.poll work)
            |> Result.map_error ~f:(fun error -> Error.Retained error)
          in
          map_result t.references ~f:(fun reference ->
            Work.poll work;
            match
              List.find (Custody.Retained.programs retained) ~f:(fun program ->
                Work.poll work;
                Work.string_equal
                  work
                  (Reference.program_key reference)
                  (Custody.Retained.Program.key program))
            with
            | None ->
              Error
                (Error.Invalid_association
                   "Baseline key absent from exact artifact custody")
            | Some program ->
              let slot =
                Custody.Binding.slot (Custody.Retained.Program.binding program)
              in
              if not (Slot_assertion.matches work (Reference.slot reference) slot)
              then
                Error
                  (Error.Invalid_association
                     "Baseline slot differs from exact retained program")
              else
                Ok
                  Prior.
                    { reference; slot; input = Custody.Retained.Program.input program })))
  ;;
end

module Success = struct
  type t = { entry : Entry.t }

  let entry t = t.entry

  let finalize prepared ~revision ~limits ~checkpoint =
    Work.run limits ~checkpoint ~f:(fun work ->
      let open Result.Let_syntax in
      let artifact = Prompt_revision.artifact revision in
      match
        ( artifact.prompt_definition_id
        , artifact.source_receipt
        , artifact.program_bundle
        , Prompt_revision.programs revision )
      with
      | Some definition_id, Some source_receipt, Some bundle, Some current
        when Id.Prompt_definition.equal
               definition_id
               (Prompt_revision.definition revision).id ->
        let%bind expected =
          Prompt_program_custody.fingerprint
            prepared.Prepared.custody
            ~checkpoint:(fun () -> Work.poll work)
          |> Result.map_error ~f:(fun error -> Error.Custody error)
        in
        let%bind actual =
          Prompt_program_custody.fingerprint current ~checkpoint:(fun () ->
            Work.poll work)
          |> Result.map_error ~f:(fun error -> Error.Custody error)
        in
        let%bind stored =
          Storage.Manifest.fingerprint
            (Storage.manifest bundle)
            ~limits:(Prompt_program_custody.manifest_limits ())
            ~checkpoint:(fun () -> Work.poll work)
          |> Result.map_error ~f:(fun error -> Error.Document error)
        in
        if
          not
            (Work.string_equal work expected actual
             && Work.string_equal work expected stored)
        then Error Error.Foreign_artifact
        else (
          let left = Storage.payloads (Custody.to_storage prepared.custody) in
          let right = Storage.payloads bundle in
          let equal =
            List.equal
              (fun left right ->
                 Work.poll work;
                 Work.string_equal
                   work
                   (Storage.Payload.sha256 left)
                   (Storage.Payload.sha256 right)
                 && Work.string_equal
                      work
                      (Storage.Payload.contents left)
                      (Storage.Payload.contents right))
              left
              right
          in
          if not equal
          then Error Error.Foreign_artifact
          else (
            let%map entry =
              Entry.of_fields
                ~definition_id
                ~source_receipt
                ~revision_id:artifact.revision_id
                ~manifest_sha256:artifact.manifest_sha256
                ~references:prepared.references
                ~limits
                ~checkpoint:(fun () -> Work.poll work)
            in
            { entry }))
      | _, _, _, _ -> Error Error.Missing_program_bundle)
  ;;
end
