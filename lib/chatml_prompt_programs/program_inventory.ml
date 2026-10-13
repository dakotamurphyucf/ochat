open Core

let map_result values ~f =
  List.fold_result values ~init:[] ~f:(fun acc value ->
    Result.map (f value) ~f:(fun value -> value :: acc))
  |> Result.map ~f:List.rev
;;

let iter_result values ~f = List.fold_result values ~init:() ~f:(fun () value -> f value)

module Captured = Chatml_source
module Revision = Program_source_revision
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Binding = Chatml_prompt_binding
module Parser = Chatmd_parser_input
module Parsed = Prompt.Chat_markdown.Parsed
module F = Program_fields
module Storage = Agent_store.Prompt_program_bundle

module Input = struct
  type t =
    | Provided of Prepared.t
    | Selected of Build.t
end

module Limits = struct
  type t =
    { max_programs : int
    ; max_parser_inputs : int
    ; max_logical_sources : int
    ; max_total_input_bytes : int
    ; project_work_steps : int
    ; document : Document_schema.Limits.t
    }

  let create
        ~max_programs
        ~max_parser_inputs
        ~max_logical_sources
        ~max_total_input_bytes
        ~project_work_steps
        ~document
    =
    if
      max_programs <= 0
      || max_parser_inputs <= 0
      || max_logical_sources <= 0
      || max_total_input_bytes <= 0
      || project_work_steps <= 0
    then Or_error.error_string "Program custody limits must be positive"
    else (
      let%map.Or_error document =
        Document_schema.Limits.create
          ~max_bytes:(Int.min 262144 (Document_schema.Limits.max_bytes document))
          ~max_depth:(Document_schema.Limits.max_depth document)
          ~max_fields:(Document_schema.Limits.max_fields document)
          ~max_nodes:(Document_schema.Limits.max_nodes document)
        |> Result.map_error ~f:(fun e ->
          Core.Error.of_string (Sexp.to_string_hum (Document_schema.Error.sexp_of_t e)))
      in
      { max_programs
      ; max_parser_inputs
      ; max_logical_sources
      ; max_total_input_bytes
      ; project_work_steps
      ; document
      })
  ;;

  let policy_identity t =
    let module D = Document_schema.Limits in
    [%sexp
      (t.max_programs : int)
    , (t.max_parser_inputs : int)
    , (t.max_logical_sources : int)
    , (t.max_total_input_bytes : int)
    , (t.project_work_steps : int)
    , (D.max_bytes t.document : int)
    , (D.max_depth t.document : int)
    , (D.max_fields t.document : int)
    , (D.max_nodes t.document : int)]
  ;;

  let max_programs t = t.max_programs
  let max_parser_inputs t = t.max_parser_inputs
  let max_logical_sources t = t.max_logical_sources
  let max_total_input_bytes t = t.max_total_input_bytes
  let project_work_steps t = t.project_work_steps
  let document t = t.document
end

module Error = struct
  type t =
    | Document of Document_schema.Error.t
    | Storage of Agent_store.Store_error.t
    | Provided of Chatml_program_document.Error.t
    | Selected of Build.Error.t
    | Invalid_binding of string
    | Duplicate_program of Binding.t * Captured.Digest.t
    | Conflicting_source of Captured.Id.t
    | Missing_contract of Binding.t
    | Unexpected_contract of Binding.t
    | Contract_mismatch of Binding.t
    | Script_mismatch of Binding.t
    | Invalid_inventory of string
    | Limit_exceeded
end

module Program = struct
  type kind =
    | Provided of Chatml_program_document.t
    | Selected of Revision.t

  type t =
    { key : string
    ; binding : Binding.t
    ; compiler_contract : Captured.Digest.t
    ; fingerprint : Captured.Digest.t
    ; input_bytes : int
    ; sources : Revision.t list
    ; kind : kind
    }

  let key binding contract = Binding.key binding ^ ":" ^ Captured.Digest.to_hex contract
end

module Selection = struct
  type t =
    { program : string
    ; binding : Binding.t
    }
end

type t =
  { programs : Program.t list
  ; inputs : Parser.t list
  ; sources : Captured.t list
  ; selections : Selection.t list
  ; root : Parser.t
  }

let invalid message = Error (Error.Invalid_inventory message)
let limit = Error Error.Limit_exceeded

let count values maximum ~checkpoint =
  List.fold_result values ~init:0 ~f:(fun used _ ->
    checkpoint ();
    if used >= maximum then limit else Ok (used + 1))
  |> Result.map ~f:ignore
;;

let prepared = function
  | Input.Provided p -> p
  | Selected b -> Build.prepared b
;;

let find_program t key ~checkpoint =
  List.find t.programs ~f:(fun p ->
    checkpoint ();
    String.equal p.Program.key key)
;;

let find_source t revision ~checkpoint =
  List.find t.sources ~f:(fun s ->
    checkpoint ();
    Revision.matches revision s)
;;

let bytes_equal left right ~checkpoint =
  if String.length left <> String.length right
  then false
  else (
    let offset = ref 0
    and same = ref true in
    while !same && !offset < String.length left do
      if !offset mod 4096 = 0 then checkpoint ();
      same := Char.equal left.[!offset] right.[!offset];
      incr offset
    done;
    checkpoint ();
    !same)
;;

let same_source left right ~checkpoint =
  Captured.Digest.equal (Captured.digest left) (Captured.digest right)
  && bytes_equal (Captured.contents left) (Captured.contents right) ~checkpoint
;;

let admit_sources sources ~limits ~checkpoint =
  List.fold_result
    sources
    ~init:(Map.empty (module Revision))
    ~f:(fun all source ->
      checkpoint ();
      let revision = Revision.of_source source in
      match Map.find all revision with
      | Some previous when same_source previous source ~checkpoint -> Ok all
      | Some _ -> Error (Error.Conflicting_source (Captured.id source))
      | None ->
        if Map.length all >= Limits.max_logical_sources limits
        then limit
        else Ok (Map.add_exn all ~key:revision ~data:source))
  |> Result.map ~f:Map.data
;;

let admit_inputs inputs ~limits ~checkpoint =
  List.fold_result inputs ~init:String.Map.empty ~f:(fun all input ->
    checkpoint ();
    let open Result.Let_syntax in
    let%bind _ =
      Source.Portable_path.create (Parser.path input)
      |> Result.map_error ~f:(fun _ -> Error.Invalid_inventory "Nonportable parser path")
    in
    let key =
      (match Parser.role input with
       | Root -> "root:"
       | Import -> "import:")
      ^ Parser.path input
    in
    match Map.find all key with
    | Some previous ->
      if
        Binding.same_input previous input
        && Parser.authored_matches previous (Parser.authored input) ~checkpoint
        && Parser.effective_matches previous (Parser.effective input) ~checkpoint
      then Ok all
      else invalid "Conflicting parser role/path revision"
    | None ->
      if Map.length all >= Limits.max_parser_inputs limits
      then limit
      else Ok (Map.add_exn all ~key ~data:input))
  |> Result.map ~f:Map.data
;;

let admit_authored authored ~checkpoint =
  List.fold_result
    (Chatmd_source_bundle.sources authored)
    ~init:(0, 0)
    ~f:(fun (count, bytes) (_, body) ->
      checkpoint ();
      if
        count >= Storage.max_payload_count
        || String.length body > Storage.max_payload_bytes - bytes
      then limit
      else Ok (count + 1, bytes + String.length body))
  |> Result.map ~f:ignore
;;

let input_equal_exact left right ~checkpoint =
  Binding.same_input left right
  && Parser.authored_matches left (Parser.authored right) ~checkpoint
  && Parser.effective_matches left (Parser.effective right) ~checkpoint
;;

let physical t ~authored ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = admit_authored authored ~checkpoint in
  let raw = Chatmd_source_bundle.sources authored in
  let count = List.length raw in
  let bytes = List.sum (module Int) raw ~f:(fun (_, body) -> String.length body) in
  let%bind initial =
    List.fold_result raw ~init:String.Map.empty ~f:(fun seen (_, body) ->
      checkpoint ();
      let digest =
        Captured.Digest.of_contents body ~checkpoint |> Captured.Digest.to_hex
      in
      match Map.find seen digest with
      | Some previous ->
        if bytes_equal previous body ~checkpoint
        then Ok seen
        else invalid "SHA256 collision in authored inventory"
      | None -> Ok (Map.add_exn seen ~key:digest ~data:body))
  in
  let bodies =
    List.map t.sources ~f:(fun s ->
      Captured.Digest.to_hex (Captured.digest s), Captured.contents s)
    @ List.map t.inputs ~f:(fun input ->
      Parser.effective_sha256 input, Parser.effective input)
  in
  let%map _, _, _, private_bodies =
    List.fold_result
      bodies
      ~init:(initial, count, bytes, [])
      ~f:(fun (seen, count, bytes, private_bodies) (digest, body) ->
        checkpoint ();
        match Map.find seen digest with
        | Some previous ->
          if bytes_equal previous body ~checkpoint
          then Ok (seen, count, bytes, private_bodies)
          else invalid "SHA256 collision in payload inventory"
        | None ->
          if
            count >= Storage.max_payload_count
            || String.length body > Storage.max_payload_bytes - bytes
          then limit
          else
            Ok
              ( Map.add_exn seen ~key:digest ~data:body
              , count + 1
              , bytes + String.length body
              , (digest, body) :: private_bodies ))
  in
  List.rev private_bodies
;;

let payloads t ~authored ~checkpoint =
  let open Result.Let_syntax in
  let%bind bodies = physical t ~authored ~checkpoint in
  map_result bodies ~f:(fun (_, body) ->
    checkpoint ();
    Storage.Payload.create body |> Result.map_error ~f:(fun e -> Error.Storage e))
;;

(* A lower bound on the actual custody ID schema: its fixed64-byte key plus
   owned field names/punctuation exceed100 bytes before opaque labels and path.
   This is early allocation admission, not another JSON escaping implementation;
   the exact two-pass encoder remains authoritative. *)
let reserve_id remaining id =
  let namespace = String.length (Captured.Id.package_namespace id) in
  let path = String.length (Captured.Id.relative_path id) in
  if remaining < 100 || path > remaining - 100 || namespace > (remaining - 100 - path) / 2
  then limit
  else Ok (remaining - 100 - path - (2 * namespace))
;;

let preflight_references groups ~values ~id ~limits ~checkpoint =
  List.fold_result
    groups
    ~init:(Document_schema.Limits.max_bytes (Limits.document limits))
    ~f:(fun remaining group ->
      checkpoint ();
      List.fold_result (values group) ~init:remaining ~f:(fun remaining value ->
        checkpoint ();
        reserve_id remaining (id value)))
  |> Result.map ~f:ignore
;;

let validate_program t program ~limits ~checkpoint =
  let open Result.Let_syntax in
  let doc result = Result.map_error result ~f:(fun e -> Error.Document e) in
  let descriptor id digest length =
    let revision = Revision.create id ~digest ~byte_length:length |> Or_error.ok_exn in
    match find_source t revision ~checkpoint with
    | Some actual
      when Captured.Digest.equal digest (Captured.digest actual)
           && Int.equal length (Captured.byte_length actual) -> Ok ()
    | Some _ | None -> invalid "Program expected source revision differs"
  in
  let%bind expected, entry =
    match program.Program.kind with
    | Provided graph ->
      if
        not
          (Captured.Digest.equal
             program.fingerprint
             (Chatml_program_document.fingerprint graph))
      then invalid "Provided fingerprint descriptor mismatch"
      else (
        let payload =
          Document_schema.Document.payload (Chatml_program_document.to_document graph)
        in
        let%bind contract_json = doc (F.field payload "compiler_contract") in
        let%bind contract = doc (F.digest contract_json) in
        let%bind entry_json = doc (F.field payload "entry") in
        let%bind entry = doc (F.id entry_json ~checkpoint) in
        if not (Captured.Digest.equal contract program.compiler_contract)
        then invalid "Provided contract descriptor mismatch"
        else (
          let%map ids =
            List.fold_result
              (Chatml_program_document.sources graph)
              ~init:(Set.empty (module Revision))
              ~f:(fun ids expected ->
                checkpoint ();
                let id = Chatml_program_document.Source.id expected in
                let revision =
                  Revision.create
                    id
                    ~digest:(Chatml_program_document.Source.digest expected)
                    ~byte_length:(Chatml_program_document.Source.byte_length expected)
                  |> Or_error.ok_exn
                in
                if Set.mem ids revision
                then invalid "Duplicate provided expected source"
                else (
                  let%map () =
                    descriptor
                      id
                      (Chatml_program_document.Source.digest expected)
                      (Chatml_program_document.Source.byte_length expected)
                  in
                  Set.add ids revision))
          in
          ids, entry))
    | Selected lock_id ->
      let%bind lock =
        match find_source t lock_id ~checkpoint with
        | Some s -> Ok s
        | None -> invalid "Missing actual selected lock capture"
      in
      let%bind receipt =
        Chatml_project.Lock.decode lock ~limits:(Limits.document limits) ~checkpoint
        |> Result.map_error ~f:(fun e -> Error.Document (Chatml_project.Error.details e))
      in
      if
        not
          (Captured.Digest.equal
             (Chatml_project.Lock.compiler_contract receipt)
             program.compiler_contract)
      then invalid "Selected contract descriptor mismatch"
      else (
        let entries =
          Chatml_project.Lock.Entry.source (Chatml_project.Lock.entry_selection receipt)
          :: List.concat_map (Chatml_project.Lock.catalogs receipt) ~f:(fun catalog ->
            checkpoint ();
            Chatml_project.Lock.Catalog.sources catalog)
        in
        let%map ids =
          List.fold_result
            entries
            ~init:(Set.singleton (module Revision) lock_id)
            ~f:(fun ids expected ->
              checkpoint ();
              let id = Chatml_project.Lock.Source_entry.id expected in
              let%map () =
                descriptor
                  id
                  (Chatml_project.Lock.Source_entry.digest expected)
                  (Chatml_project.Lock.Source_entry.byte_length expected)
              in
              let revision =
                Revision.create
                  id
                  ~digest:(Chatml_project.Lock.Source_entry.digest expected)
                  ~byte_length:(Chatml_project.Lock.Source_entry.byte_length expected)
                |> Or_error.ok_exn
              in
              Set.add ids revision)
        in
        ids, Chatml_project.Lock.entry receipt)
  in
  let actual = Set.of_list (module Revision) program.sources in
  if not (Set.equal expected actual)
  then invalid "Program source membership differs from metadata"
  else (
    match
      List.find program.sources ~f:(fun revision ->
        checkpoint ();
        Captured.Id.equal (Revision.id revision) entry)
      |> Option.bind ~f:(fun revision -> find_source t revision ~checkpoint)
    with
    | Some source
      when Captured.Digest.equal
             (Captured.digest source)
             (Binding.script_digest program.binding) -> Ok ()
    | Some _ | None -> Error (Error.Script_mismatch program.binding))
;;

let validate t ~authored ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = admit_authored authored ~checkpoint in
  let%bind () = count t.programs (Limits.max_programs limits) ~checkpoint in
  let%bind () = count t.inputs (Limits.max_parser_inputs limits) ~checkpoint in
  let%bind () = count t.sources (Limits.max_logical_sources limits) ~checkpoint in
  let%bind () = count t.selections (Limits.max_programs limits) ~checkpoint in
  let%bind _ =
    List.fold_result t.programs ~init:0 ~f:(fun bytes p ->
      checkpoint ();
      if
        p.Program.input_bytes <= 0
        || p.input_bytes > Limits.max_total_input_bytes limits - bytes
      then limit
      else Ok (bytes + p.input_bytes))
  in
  let%bind () =
    preflight_references
      t.programs
      ~values:(fun p -> p.Program.sources)
      ~id:Revision.id
      ~limits
      ~checkpoint
  in
  let%bind () =
    iter_result t.programs ~f:(fun p ->
      if
        String.length (Binding.script_id p.Program.binding)
        > Document_schema.Limits.max_bytes (Limits.document limits) / 2
      then limit
      else Ok ())
  in
  let%bind sources = admit_sources t.sources ~limits ~checkpoint in
  let%bind inputs = admit_inputs t.inputs ~limits ~checkpoint in
  let%bind () =
    if
      List.length sources <> List.length t.sources
      || List.length inputs <> List.length t.inputs
    then invalid "Duplicate inventory identity"
    else Ok ()
  in
  let%bind () =
    if
      (not (Parser.Role.equal (Parser.role t.root) Root))
      || (not
            (List.exists t.inputs ~f:(fun input ->
               input_equal_exact t.root input ~checkpoint)))
      || not (String.equal (Parser.path t.root) (Chatmd_source_bundle.root_file authored))
    then invalid "Current Root is absent or differs from authored root"
    else Ok ()
  in
  let raw = Chatmd_source_bundle.sources authored in
  let%bind () =
    iter_result t.inputs ~f:(fun input ->
      checkpoint ();
      match List.Assoc.find raw (Parser.path input) ~equal:String.equal with
      | None -> invalid "Parser input has no authored source"
      | Some body ->
        if not (Parser.authored_matches input body ~checkpoint)
        then invalid "Parser authored revision mismatch"
        else (
          match Source.Portable_path.create (Parser.path input) with
          | Ok _ -> Ok ()
          | Error _ -> invalid "Nonportable parser path"))
  in
  let%bind keys =
    List.fold_result t.programs ~init:String.Set.empty ~f:(fun keys p ->
      checkpoint ();
      if not (String.equal p.Program.key (Program.key p.binding p.compiler_contract))
      then invalid "Program identity mismatch"
      else if Set.mem keys p.key
      then Error (Error.Duplicate_program (p.binding, p.compiler_contract))
      else if
        (not
           (List.exists t.inputs ~f:(fun input ->
              input_equal_exact (Binding.parser_input p.binding) input ~checkpoint)))
        || not
             (List.exists t.inputs ~f:(fun input ->
                input_equal_exact (Binding.registration_input p.binding) input ~checkpoint))
      then Error (Error.Invalid_binding "Binding input is absent")
      else Ok (Set.add keys p.key))
  in
  let%bind referenced =
    List.fold_result
      t.programs
      ~init:(Set.empty (module Revision))
      ~f:(fun referenced p ->
        checkpoint ();
        let%bind local =
          List.fold_result
            p.Program.sources
            ~init:(Set.empty (module Captured.Id), Set.empty (module Revision))
            ~f:(fun (ids, revisions) revision ->
              let id = Revision.id revision in
              checkpoint ();
              if Set.mem ids id
              then invalid "Duplicate program source reference"
              else if Option.is_none (find_source t revision ~checkpoint)
              then invalid "Missing logical source"
              else Ok (Set.add ids id, Set.add revisions revision))
        in
        Ok (Set.union referenced (snd local)))
  in
  let%bind () =
    if Set.length referenced <> List.length t.sources
    then invalid "Surplus logical source"
    else Ok ()
  in
  let%bind _ =
    List.fold_result
      t.selections
      ~init:(String.Set.empty, String.Set.empty)
      ~f:(fun (seen, current) selection ->
        checkpoint ();
        if
          (not (Set.mem keys selection.Selection.program))
          || Set.mem seen selection.program
        then invalid "Missing or duplicate active program"
        else (
          match find_program t selection.program ~checkpoint with
          | None -> invalid "Missing selected program"
          | Some p ->
            if
              (not (Binding.same_declaration p.binding selection.binding))
              || (not
                    (input_equal_exact
                       (Binding.registration_input selection.binding)
                       t.root
                       ~checkpoint))
              || not
                   (List.exists t.inputs ~f:(fun input ->
                      input_equal_exact
                        (Binding.parser_input selection.binding)
                        input
                        ~checkpoint))
            then Error (Error.Invalid_binding "Invalid active declaration mapping")
            else (
              let current_key = Program.key selection.binding p.compiler_contract in
              if Set.mem current current_key
              then invalid "Duplicate current active binding/contract"
              else Ok (Set.add seen selection.program, Set.add current current_key))))
  in
  let%bind () =
    iter_result t.programs ~f:(fun p -> validate_program t p ~limits ~checkpoint)
  in
  let%map _ = physical t ~authored ~checkpoint in
  ()
;;

let capture authored ~parsed_contexts ~root ~programs ~active_programs ~limits ~checkpoint
  =
  let raw_programs = programs in
  let open Result.Let_syntax in
  let%bind () = admit_authored authored ~checkpoint in
  let%bind () = count programs (Limits.max_programs limits) ~checkpoint in
  let%bind () = count active_programs (Limits.max_programs limits) ~checkpoint in
  let%bind () = count parsed_contexts (Limits.max_parser_inputs limits) ~checkpoint in
  let%bind _ =
    List.fold_result programs ~init:0 ~f:(fun used (_, input) ->
      checkpoint ();
      let bytes =
        Prepared.Accounting.input_bytes (Prepared.accounting (prepared input))
      in
      if bytes > Limits.max_total_input_bytes limits - used
      then limit
      else Ok (used + bytes))
  in
  let%bind () =
    preflight_references
      programs
      ~values:(fun (_, input) -> Prepared.sources (prepared input))
      ~id:Captured.id
      ~limits
      ~checkpoint
  in
  let%bind programs, _ =
    List.fold_result programs ~init:([], 0) ~f:(fun (output, used) (binding, input) ->
      checkpoint ();
      let p = prepared input in
      if
        not
          (List.exists parsed_contexts ~f:(fun parsed ->
             Binding.matches_parsed binding parsed ~checkpoint))
      then Error (Error.Invalid_binding "Binding is not an actual supplied declaration")
      else (
        match Prepared.find_source p (Prepared.entry p) with
        | None -> Error (Error.Script_mismatch binding)
        | Some entry
          when not
                 (Captured.Digest.equal
                    (Captured.digest entry)
                    (Binding.script_digest binding)) ->
          Error (Error.Script_mismatch binding)
        | Some entry ->
          let%bind () =
            if
              List.exists parsed_contexts ~f:(fun parsed ->
                Binding.matches_entry binding parsed (Captured.contents entry) ~checkpoint)
            then Ok ()
            else Error (Error.Script_mismatch binding)
          in
          let%bind kind, bytes =
            match input with
            | Input.Selected build ->
              Ok (Program.Selected (Revision.of_source (Build.lock_source build)), 0)
            | Provided _ ->
              let remaining =
                Document_schema.Limits.max_bytes (Limits.document limits) - used
              in
              if remaining <= 0
              then limit
              else (
                let d = Limits.document limits in
                let%bind d =
                  Document_schema.Limits.create
                    ~max_bytes:remaining
                    ~max_depth:(Document_schema.Limits.max_depth d)
                    ~max_fields:(Document_schema.Limits.max_fields d)
                    ~max_nodes:(Document_schema.Limits.max_nodes d)
                  |> Result.map_error ~f:(fun e -> Error.Document e)
                in
                let%bind doc =
                  Chatml_program_document.capture p ~limits:d ~checkpoint
                  |> Result.map_error ~f:(fun e -> Error.Provided e)
                in
                let%map bytes =
                  Document_schema.Json.validate_and_measure
                    ~limits:d
                    (Document_schema.Document.json
                       (Chatml_program_document.to_document doc))
                  |> Result.map_error ~f:(fun e -> Error.Document e)
                in
                Program.Provided doc, bytes)
          in
          let contract = Prepared.compiler_contract p in
          let program =
            Program.
              { key = key binding contract
              ; binding
              ; compiler_contract = contract
              ; fingerprint = Prepared.fingerprint p
              ; input_bytes = Prepared.Accounting.input_bytes (Prepared.accounting p)
              ; sources = List.map (Prepared.sources p) ~f:Revision.of_source
              ; kind
              }
          in
          Ok (program :: output, used + bytes)))
  in
  let programs = List.rev programs in
  let%bind sources =
    List.fold_result raw_programs ~init:[] ~f:(fun sources (_, input) ->
      let%map sources =
        admit_sources (sources @ Prepared.sources (prepared input)) ~limits ~checkpoint
      in
      sources)
  in
  let%bind inputs =
    List.fold_result parsed_contexts ~init:[] ~f:(fun inputs parsed ->
      let%map inputs = admit_inputs (inputs @ Parsed.inputs parsed) ~limits ~checkpoint in
      inputs)
  in
  let selections =
    List.map active_programs ~f:(fun (binding, contract) ->
      Selection.{ program = Program.key binding contract; binding })
  in
  let t = { programs; inputs; sources; selections; root } in
  let%map () = validate t ~authored ~limits ~checkpoint in
  t
;;
