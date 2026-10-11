open Core

let iter_result values ~f = List.fold_result values ~init:() ~f:(fun () value -> f value)

module Inventory = Program_inventory
module Binding = Program_binding
module Contract = Chatml.Chatml_compiler_contract
module Captured = Chatml_source
module Revision = Program_source_revision
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Error = Inventory.Error
module Limits = Inventory.Limits

module Program = struct
  type t =
    { binding : Binding.t
    ; input : Inventory.Input.t
    ; compiler_contract : Captured.Digest.t
    }

  let binding t = t.binding
  let input t = t.input

  let prepared t =
    match t.input with
    | Provided p -> p
    | Selected b -> Build.prepared b
  ;;

  let compiler_contract t = t.compiler_contract
end

type t = Program.t list

let programs t = t

let find t binding ~compiler_contract =
  List.find t ~f:(fun p ->
    Binding.equal (Program.binding p) binding
    && Captured.Digest.equal (Program.compiler_contract p) compiler_contract)
;;

let restore inventory ~parsed ~contracts ~limits ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let root = Prompt.Chat_markdown.Parsed.root_input parsed in
  if
    (not (Binding.same_input inventory.Inventory.root root))
    || (not
          (Chatmd_parser_input.authored_matches
             root
             (Chatmd_parser_input.authored inventory.root)
             ~checkpoint))
    || not
         (Chatmd_parser_input.effective_matches
            root
            (Chatmd_parser_input.effective inventory.root)
            ~checkpoint)
  then Error (Error.Invalid_binding "Replayed Root differs from durable selection")
  else (
    let%bind contracts =
      List.fold_result contracts ~init:[] ~f:(fun acc (binding, contract) ->
        checkpoint ();
        if List.length acc >= Limits.max_programs limits
        then Error Error.Limit_exceeded
        else if not (Binding.matches_parsed binding parsed ~checkpoint)
        then
          Error
            (Error.Invalid_binding
               "Contract binding is not an actual replayed declaration")
        else (
          let digest = Contract.fingerprint contract ~checkpoint in
          let key = Inventory.Program.key binding digest in
          if List.exists acc ~f:(fun (previous, _, _) -> String.equal previous key)
          then Error (Error.Unexpected_contract binding)
          else Ok ((key, binding, contract) :: acc)))
    in
    let%bind restored, _, _, consumed =
      List.fold_result
        inventory.selections
        ~init:([], 0, 0, String.Set.empty)
        ~f:(fun (restored, bytes, work, consumed) selection ->
          checkpoint ();
          match
            Inventory.find_program
              inventory
              selection.Inventory.Selection.program
              ~checkpoint
          with
          | None -> Error (Error.Invalid_inventory "Missing active program")
          | Some program ->
            let binding = selection.binding in
            let key = Inventory.Program.key binding program.compiler_contract in
            let%bind contract =
              match
                List.find contracts ~f:(fun (actual, _, _) -> String.equal actual key)
              with
              | Some (_, _, contract) -> Ok contract
              | None -> Error (Error.Missing_contract binding)
            in
            let sources =
              List.filter inventory.sources ~f:(fun source ->
                checkpoint ();
                List.exists program.sources ~f:(fun id ->
                  checkpoint ();
                  Revision.matches id source))
            in
            let remaining = Limits.max_total_input_bytes limits - bytes in
            if remaining <= 0 || program.input_bytes > remaining
            then Error Error.Limit_exceeded
            else (
              let%bind input, used_work =
                match program.kind with
                | Provided document ->
                  let%map prepared =
                    Chatml_program_document.restore
                      document
                      ~sources
                      ~compiler_contract:program.compiler_contract
                      ~max_input_bytes:remaining
                      ~checkpoint
                    |> Result.map_error ~f:(fun e -> Error.Provided e)
                  in
                  Inventory.Input.Provided prepared, 0
                | Selected lock_id ->
                  let%bind lock =
                    match Inventory.find_source inventory lock_id ~checkpoint with
                    | Some lock -> Ok lock
                    | None -> Error (Error.Invalid_inventory "Missing selected lock")
                  in
                  let remaining_work = Limits.project_work_steps limits - work in
                  if remaining_work <= 0
                  then Error Error.Limit_exceeded
                  else (
                    let%bind build_limits =
                      Build.Limits.create
                        ~max_work_steps:remaining_work
                        ~max_input_bytes:(Some remaining)
                        ~document:(Limits.document limits)
                      |> Result.map_error ~f:(fun _ -> Error.Limit_exceeded)
                    in
                    let%map build =
                      Build.restore
                        lock
                        ~sources:
                          (List.filter sources ~f:(fun source ->
                             not (Revision.matches lock_id source)))
                        ~contract
                        ~limits:build_limits
                        ~checkpoint
                      |> Result.map_error ~f:(fun e -> Error.Selected e)
                    in
                    ( Inventory.Input.Selected build
                    , Build.Accounting.work_steps (Build.accounting build) ))
              in
              let prepared =
                match input with
                | Provided p -> p
                | Selected b -> Build.prepared b
              in
              let input_bytes =
                Prepared.Accounting.input_bytes (Prepared.accounting prepared)
              in
              let%bind () =
                if
                  (not
                     (Captured.Digest.equal
                        (Prepared.fingerprint prepared)
                        program.fingerprint))
                  || not (Int.equal input_bytes program.input_bytes)
                then
                  Error
                    (Error.Invalid_inventory "Restored fingerprint/accounting mismatch")
                else Ok ()
              in
              let%bind () =
                match Prepared.find_source prepared (Prepared.entry prepared) with
                | Some source
                  when Captured.Digest.equal
                         (Captured.digest source)
                         (Binding.script_digest binding)
                       && Binding.matches_entry
                            binding
                            parsed
                            (Captured.contents source)
                            ~checkpoint -> Ok ()
                | Some _ | None -> Error (Error.Script_mismatch binding)
              in
              Ok
                ( { Program.binding; input; compiler_contract = program.compiler_contract }
                  :: restored
                , bytes + input_bytes
                , work + used_work
                , Set.add consumed key )))
    in
    let%map () =
      iter_result contracts ~f:(fun (key, binding, _) ->
        if Set.mem consumed key then Ok () else Error (Error.Unexpected_contract binding))
    in
    List.rev restored)
;;
