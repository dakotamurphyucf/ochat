open Core
module CM = Prompt.Chat_markdown
module C = Chatml_prompt_programs
module Contract = Chatml.Chatml_compiler_contract
module MC = Chatmd_shell_spec.Manifest_compiler
module D = Chatmd_shell_spec.Diagnostic

module Limits = struct
  type t =
    { programs : int
    ; metadata_bytes : int
    ; work_steps : int
    }
  [@@deriving sexp_of]

  let create ~max_programs ~max_metadata_bytes ~max_work_steps =
    if max_programs <= 0 || max_metadata_bytes <= 0 || max_work_steps <= 0
    then Or_error.error_string "Program planning limits must be positive"
    else
      Ok
        { programs = max_programs
        ; metadata_bytes = max_metadata_bytes
        ; work_steps = max_work_steps
        }
  ;;

  let policy_identity = sexp_of_t
end

module Program = struct
  module Role = struct
    module Moderator_invocation = struct
      type t =
        | Ui
        | Headless
      [@@deriving equal, sexp]
    end

    type t =
      | Standard of Contract.Target.t
      | Legacy_moderator of Moderator_invocation.t
      | Native_hook of
          { kind : Chatmd_shell_spec.Chatmd_script_spec.kind
          ; entrypoint : string
          }
    [@@deriving equal, sexp_of]
  end

  type t =
    { role : Role.t
    ; declaration : CM.Declaration.t
    ; binding : C.Binding.t
    ; contract : Contract.t
    ; compiler_contract : Chatml_source.Digest.t
    }

  let role t = t.role
  let declaration t = t.declaration
  let binding t = t.binding
  let contract t = t.contract
  let compiler_contract t = t.compiler_contract
end

type t =
  { parsed : CM.Parsed.t
  ; programs : Program.t list
  }

let parsed t = t.parsed
let programs t = t.programs

let has_projects t =
  List.exists t.programs ~f:(fun program ->
    Option.is_some (CM.Declaration.project_selection program.Program.declaration))
;;

let contracts t =
  List.map t.programs ~f:(fun program -> program.Program.binding, program.contract)
;;

let bytes_equal left right ~checkpoint =
  let length = String.length left in
  let equal = ref (Int.equal length (String.length right)) in
  let offset = ref 0 in
  while !equal && !offset < length do
    checkpoint ();
    let stop = !offset + Int.min 4096 (length - !offset) in
    while !equal && !offset < stop do
      equal := Char.equal left.[!offset] right.[!offset];
      incr offset
    done
  done;
  checkpoint ();
  !equal
;;

let create
      parsed
      ~moderator_invocation
      ~moderator_surface
      ~delegated_moderator
      ~shell
      ~limits
      ~checkpoint
  =
  let module Abort = struct
    exception Stop of D.t list
  end
  in
  let fail ?source code message = raise (Abort.Stop [ D.error ?source ~code message ]) in
  let remaining_work = ref limits.Limits.work_steps in
  let poll () =
    checkpoint ();
    if !remaining_work <= 0
    then fail "chatml.program_plan_limit" "Script contract planning work limit";
    decr remaining_work
  in
  let remaining_bytes = ref limits.metadata_bytes in
  let debit bytes =
    if bytes < 0 || bytes > !remaining_bytes
    then fail "chatml.program_plan_limit" "Script contract planning metadata limit";
    remaining_bytes := !remaining_bytes - bytes
  in
  let count = ref 0 in
  let rows = ref [] in
  let descriptor_owner id source =
    poll ();
    debit (String.length id);
    match
      List.filter (CM.Parsed.declarations parsed) ~f:(fun declaration ->
        poll ();
        (* Both records are retained from the SAME actual parser conversion.
           Identity authenticates ownership without hashing source metadata. *)
        phys_equal (CM.Declaration.source_ref declaration) source)
    with
    | [ declaration ] ->
      if not (bytes_equal (CM.Declaration.script_id declaration) id ~checkpoint:poll)
      then
        fail ~source "chatml.program_plan_owner" "Script identity changed after parsing";
      declaration
    | [] | _ :: _ :: _ ->
      fail ~source "chatml.program_plan_owner" "Script has no unique parser declaration"
  in
  let add declaration role make_contract =
    poll ();
    if !count >= limits.programs
    then fail "chatml.program_plan_limit" "Script contract planning program limit";
    incr count;
    let binding =
      C.Binding.of_declaration parsed declaration ~checkpoint:poll
      |> Result.map_error ~f:(fun _ -> ())
      |> function
      | Ok binding -> binding
      | Error () ->
        fail
          ~source:(CM.Declaration.source_ref declaration)
          "chatml.program_plan_owner"
          "Script declaration does not belong to parser context"
    in
    let contract = make_contract () in
    (* Canonical byte accounting is separate from the encoder's conservative
       allocation debit. Avoid multiplying a supplied ceiling before checking. *)
    let allowance = !remaining_bytes in
    let allocation = ref 0 in
    let allocate bytes =
      if bytes < 0 || bytes > Int.max_value - !allocation
      then fail "chatml.program_plan_limit" "Script metadata allocation debit overflow";
      allocation := !allocation + bytes;
      if
        !allocation / 32 > allowance
        || (!allocation / 32 = allowance && !allocation mod 32 > 0)
      then fail "chatml.program_plan_limit" "Script metadata allocation debit limit"
    in
    let bytes = Contract.input_bytes contract ~checkpoint:poll ~allocate in
    debit bytes;
    let compiler_contract = Contract.fingerprint contract ~checkpoint:poll in
    List.iter !rows ~f:(fun previous ->
      poll ();
      if
        phys_equal previous.Program.declaration declaration
        && Chatml_source.Digest.equal previous.compiler_contract compiler_contract
      then fail "chatml.program_plan_duplicate" "Duplicate script binding and contract");
    rows
    := ({ role; declaration; binding; contract; compiler_contract } : Program.t) :: !rows
  in
  try
    poll ();
    List.iter (CM.Parsed.elements parsed) ~f:(function
      | CM.Script script ->
        poll ();
        let source =
          match script.source_ref with
          | Some source -> source
          | None ->
            fail "chatml.program_plan_owner" "Legacy script lacks parser ownership"
        in
        let declaration = descriptor_owner script.id source in
        add declaration (Legacy_moderator moderator_invocation) (fun () ->
          Moderator_program.contract moderator_surface ~checkpoint:poll)
      | Extension_script script ->
        poll ();
        let declaration = descriptor_owner script.id script.source_ref in
        let target =
          match script.kind with
          | Chatmd_shell_spec.Extension_spec.Tool_script -> Contract.Target.Tool_v1
          | Moderator_script ->
            if delegated_moderator then Delegated_moderator_v1 else Moderator_v1
        in
        add declaration (Standard target) (fun () ->
          Contract.for_target target ~checkpoint:poll)
      | Shell_script _
      | Msg _
      | Developer _
      | System _
      | User _
      | Assistant _
      | Tool_call _
      | Tool_response _
      | Config _
      | Reasoning _
      | Tool _
      | Shell_runtime _
      | Moderator_runtime _
      | Authoring_context _
      | Authoring_help _ -> poll ());
    (match shell with
     | None -> ()
     | Some (manifest, material) ->
       let effective f =
         match f material ~manifest with
         | Ok value -> value
         | Error errors -> raise (Abort.Stop errors)
       in
       let scripts = effective MC.effective_scripts in
       let runtimes = effective MC.effective_runtimes in
       let entrypoints =
         List.concat_map runtimes ~f:(fun runtime ->
           poll ();
           Chatmd_shell_spec.Script_reference.of_runtime runtime)
       in
       List.iter scripts ~f:(fun script ->
         poll ();
         let declaration =
           descriptor_owner
             script.Chatmd_shell_spec.Chatmd_script_spec.id
             script.source_ref
         in
         let default = Shell_runtime.Chatml_extension.default_entrypoint script.kind in
         let names =
           List.fold entrypoints ~init:[ default ] ~f:(fun names reference ->
             poll ();
             if
               bytes_equal
                 reference.Chatmd_shell_spec.Script_reference.script
                 script.id
                 ~checkpoint:poll
             then (
               match reference.entrypoint with
               | None -> names
               | Some name ->
                 if String.length name > 1024
                 then
                   fail
                     "chatml.program_plan_limit"
                     "Native entrypoint exceeds byte allowance";
                 if List.mem names name ~equal:String.equal then names else name :: names)
             else names)
         in
         List.iter (List.rev names) ~f:(fun entrypoint ->
           add
             declaration
             (Native_hook { kind = script.kind; entrypoint })
             (fun () ->
                match
                  Shell_runtime.Chatml_extension.compiler_contract
                    script.kind
                    ~entrypoint
                    ~checkpoint:poll
                with
                | Ok contract -> contract
                | Error error -> raise (Abort.Stop [ error ])))));
    poll ();
    Ok { parsed; programs = List.rev !rows }
  with
  | Abort.Stop errors -> Error errors
;;
