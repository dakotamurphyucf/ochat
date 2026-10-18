open Core
module P = Agent_protocol
module B = Chat_response.Background_request
module Store = Agent_store.Prompt_artifact_store
module Builder = Prompt_revision_builder
module Owner = Prompt_source_owner
module Startup = Chat_response.Startup_compilation

type t =
  { revision : Prompt_revision.t
  ; reference : B.Agent_artifact.t
  }

type child = t

module Model_selection = struct
  type t =
    { source : child
    ; input_bytes : int
    ; work_steps : int
    }

  let source t = t.source
  let input_bytes t = t.input_bytes
  let work_steps t = t.work_steps
end

let revision t = t.revision
let reference t = t.reference
let unavailable message = P.Error.create Prompt_unavailable ~message ~retryable:false ()

let compilation_error (error : Chatml_compilation.error) =
  unavailable (error.code ^ ": " ^ error.message)
;;

let diagnostics values =
  unavailable (Sexp.to_string_hum ([%sexp_of: Builder.Diagnostic.t list] values))
;;

let owner_error error = unavailable (Sexp.to_string_hum (Owner.Error.sexp_of_t error))
let store_error error = Agent_store.Store_error.to_protocol_error error

let reference_of_artifact (artifact : Store.Artifact.t) ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind root_relative_path =
    Source.Portable_path.create artifact.root_relative_path
    |> Result.map_error ~f:(fun _ -> unavailable "Selected artifact root is not portable")
  in
  let%map manifest_sha256 =
    Chatml_source.Digest.of_hex artifact.manifest_sha256
    |> Result.map_error ~f:(fun _ ->
      unavailable "Selected artifact manifest digest is invalid")
  in
  B.Agent_artifact.create
    ~revision_id:artifact.revision_id
    ~manifest_sha256
    ~root_relative_path
;;

let admit revision ~checkpoint =
  let open Result.Let_syntax in
  let%map reference =
    reference_of_artifact (Prompt_revision.artifact revision) ~checkpoint
  in
  { revision; reference }
;;

let restore ~artifact_store ~definition ~content_limits reference ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind artifact =
    Store.load artifact_store (B.Agent_artifact.revision_id reference)
    |> Result.map_error ~f:store_error
  in
  checkpoint ();
  let%bind () =
    if
      String.equal
        artifact.manifest_sha256
        (Chatml_source.Digest.to_hex (B.Agent_artifact.manifest_sha256 reference))
      && String.equal
           artifact.root_relative_path
           (Source.Portable_path.to_string
              (B.Agent_artifact.root_relative_path reference))
    then Ok ()
    else Error (unavailable "Recorded child artifact differs from its selected revision")
  in
  let%bind revision =
    Builder.reparse
      ~checkpoint
      ?content_limits
      ~definition
      ~artifact
      ~materialized_tree:(Store.materialized_tree artifact_store artifact.revision_id)
      ()
    |> Result.map_error ~f:diagnostics
  in
  checkpoint ();
  admit revision ~checkpoint
;;

let file_with_selection
      ~parent
      ~source_lease
      ~artifact_store
      ~compilation
      ~tool_contract
      ~parser_limits
      ~content_limits
      ~max_work_steps
      ~preprocess
      ~prepare_programs
      ~env
      ~created_at
      ~checkpoint
      ~select_entry
      ~capture_selected
  =
  if max_work_steps <= 0
  then
    Error
      (P.Error.create
         Resource_limit
         ~message:"Child source work allowance exhausted"
         ~retryable:false
         ())
  else
    let module Abort = struct
      exception Limit
      exception Source of Owner.Error.t
    end
    in
    let used = ref 0 in
    let current_capture = ref None in
    let charge_and_check () =
      if !used >= max_work_steps then raise Abort.Limit;
      incr used;
      checkpoint ();
      Eio.Fiber.check ();
      Option.iter !current_capture ~f:(fun (scope, capture) ->
        match Owner.Scope.check_capture scope capture with
        | Ok () -> ()
        | Error error -> raise (Abort.Source error));
      Eio.Fiber.check ()
    in
    try
      Startup.with_context
        ~limits:compilation
        ~env
        ~checkpoint:charge_and_check
        ~map_error:compilation_error
        ~f:(fun startup ->
          (* Every borrower of this SAME Startup calls the charged callback,
             including compilation polls inside the supplied host preparation. *)
          let poll () = Startup.checkpoint startup in
          Startup.within startup ~map_error:compilation_error ~f:(fun () ->
            let open Result.Let_syntax in
            poll ();
            let%bind entry =
              select_entry ~remaining:(max_work_steps - !used) ~checkpoint:poll
              |> Result.map_error ~f:(fun error ->
                unavailable (Error.to_string_hum error))
            in
            let%bind result =
              Prompt_source_lease.with_scope source_lease ~f:(fun scope ->
                let%map capture =
                  capture_selected
                    scope
                    entry
                    ~env
                    ~preprocess
                    ~limits:parser_limits
                    ~checkpoint:poll
                in
                current_capture := Some (scope, capture);
                Fun.protect
                  ~finally:(fun () -> current_capture := None)
                  (fun () ->
                     let value = Owner.Capture.value capture in
                     let input_bytes = Builder.Capture.source_bytes value in
                     let same_capture supplied =
                       if not (phys_equal supplied value)
                       then invalid_arg "Prompt_child_source: foreign Builder capture"
                     in
                     let prepare_programs supplied =
                       same_capture supplied;
                       poll ();
                       prepare_programs startup ~tool_contract scope capture
                     in
                     let prepare_content =
                       Option.map content_limits ~f:(fun limits supplied ->
                         same_capture supplied;
                         poll ();
                         Owner.Scope.capture_documents
                           scope
                           ~capture
                           ~limits
                           ~checkpoint:poll
                         |> Result.map_error ~f:(fun error ->
                           match Owner.Error.capture_diagnostics error with
                           | Some diagnostics -> diagnostics
                           | None ->
                             [ Builder.Diagnostic.
                                 { code = "prompt.child_content"
                                 ; message =
                                     Sexp.to_string_hum (Owner.Error.sexp_of_t error)
                                 ; source =
                                     Some
                                       (Source.Portable_path.to_string
                                          (Prompt_source_entry.member entry))
                                 }
                             ]))
                     in
                     Builder.build_captured_entry
                       ~checkpoint:poll
                       ~prepare_programs
                       ?prepare_content
                       ~entry
                       ~capture:value
                       ~artifact_store
                       ~transaction_id:(P.Id.Transaction.create ())
                       ~created_at
                       ()
                     |> Result.map_error ~f:diagnostics
                     |> Result.map ~f:(fun revision -> revision, input_bytes)))
              |> Result.map_error ~f:owner_error
            in
            let%bind revision, input_bytes = result in
            poll ();
            let%map source = admit revision ~checkpoint:poll in
            source, input_bytes, !used))
        ()
    with
    | Abort.Source error -> Error (owner_error error)
    | Abort.Limit ->
      Error
        (P.Error.create
           Resource_limit
           ~message:"Child source work allowance exhausted"
           ~retryable:false
           ())
;;

let file
      ~parent
      ~reference
      ~source_lease
      ~artifact_store
      ~compilation
      ~tool_contract
      ~parser_limits
      ~content_limits
      ~max_work_steps
      ~preprocess
      ~prepare_programs
      ~env
      ~created_at
      ~checkpoint
  =
  file_with_selection
    ~parent
    ~source_lease
    ~artifact_store
    ~compilation
    ~tool_contract
    ~parser_limits
    ~content_limits
    ~max_work_steps
    ~preprocess
    ~prepare_programs
    ~env
    ~created_at
    ~checkpoint
    ~select_entry:(fun ~remaining ~checkpoint ->
      Prompt_source_entry.of_reference
        ~parent
        reference
        ~max_work_steps:remaining
        ~checkpoint)
    ~capture_selected:(fun scope entry ~env ~preprocess ~limits ~checkpoint ->
      Owner.Scope.capture_entry scope ~entry ~env ~preprocess ~limits ~checkpoint)
  |> Result.map ~f:(fun (source, _, _) -> source)
;;

let file_selected_member
      ~parent
      ~entry
      ~source_lease
      ~artifact_store
      ~compilation
      ~tool_contract
      ~parser_limits
      ~content_limits
      ~max_work_steps
      ~max_input_bytes
      ~preprocess
      ~prepare_programs
      ~env
      ~created_at
      ~checkpoint
      ~on_acquire
  =
  if not (Prompt_source_entry.matches_selected_parent entry parent)
  then Error (unavailable "Selected Model entry has a foreign parent")
  else
    file_with_selection
      ~parent
      ~source_lease
      ~artifact_store
      ~compilation
      ~tool_contract
      ~parser_limits
      ~content_limits
      ~max_work_steps
      ~preprocess
      ~prepare_programs
      ~env
      ~created_at
      ~checkpoint
      ~select_entry:(fun ~remaining:_ ~checkpoint:_ -> Ok entry)
      ~capture_selected:(fun scope entry ~env ~preprocess ~limits ~checkpoint ->
        let open Result.Let_syntax in
        let%bind content = Owner.Scope.bind_content_revision scope ~revision:parent in
        Owner.Scope.capture_selected_member
          scope
          ~content
          ~entry
          ~env
          ~preprocess
          ~limits
          ~max_input_bytes
          ~checkpoint
          ~on_acquire)
    |> Result.map ~f:(fun (selected_child, selected_bytes, selected_work) ->
      Model_selection.
        { source = selected_child
        ; input_bytes = selected_bytes
        ; work_steps = selected_work
        })
;;

let captured
      ~parent
      ~reference
      ~artifact_store
      ~policy
      ~tool_contract
      ~invocation
      ~content_limits
      ~env
      ~checkpoint
  =
  Runtime_script_preparation.with_captured_child
    policy
    ~tool_contract
    parent
    ~reference
    ~invocation
    ~content_limits
    ~env
    ~checkpoint
    ~f:(fun startup ~revision ~compilation_limits:_ ~programs:_ ->
      let poll () = Startup.checkpoint startup in
      let open Result.Let_syntax in
      poll ();
      let artifact = Prompt_revision.artifact revision in
      let%bind retained =
        Store.install_or_reuse
          artifact_store
          ~transaction_id:(P.Id.Transaction.create ())
          artifact
          ~checkpoint:poll
        |> Result.map_error ~f:store_error
      in
      poll ();
      let%bind reference = reference_of_artifact retained ~checkpoint:poll in
      (* Rebase through the verified immutable child tree, never the parent's
       authoring or artifact directory. Stored content is independently admitted. *)
      restore
        ~artifact_store
        ~definition:(Prompt_revision.definition parent)
        ~content_limits
        reference
        ~checkpoint:poll)
;;

let captured_selected_member
      ~parent
      ~member
      ~artifact_store
      ~policy
      ~tool_contract
      ~invocation
      ~content_limits
      ~max_input_bytes
      ~max_work_steps
      ~env
      ~checkpoint
      ~on_acquire
  =
  if max_input_bytes <= 0 || max_work_steps <= 0
  then
    Error
      (P.Error.create
         Resource_limit
         ~message:"Captured Model source allowance exhausted"
         ~retryable:false
         ())
  else
    let module Abort = struct
      exception Work_limit
    end
    in
    let used = ref 0 in
    let poll () =
      if !used >= max_work_steps then raise Abort.Work_limit;
      incr used;
      checkpoint ()
    in
    try
      Runtime_script_preparation.with_captured_selected_member
        policy
        ~tool_contract
        parent
        ~member
        ~invocation
        ~content_limits
        ~max_input_bytes
        ~max_work_steps
        ~env
        ~checkpoint:poll
        ~on_acquire
        ~f:(fun startup ~revision ~compilation_limits:_ ~programs:_ ~input_bytes ->
          let poll () = Startup.checkpoint startup in
          let open Result.Let_syntax in
          poll ();
          let artifact = Prompt_revision.artifact revision in
          let%bind retained =
            Store.install_or_reuse
              artifact_store
              ~transaction_id:(P.Id.Transaction.create ())
              artifact
              ~checkpoint:poll
            |> Result.map_error ~f:store_error
          in
          poll ();
          let%bind reference = reference_of_artifact retained ~checkpoint:poll in
          let%map source =
            restore
              ~artifact_store
              ~definition:(Prompt_revision.definition parent)
              ~content_limits
              reference
              ~checkpoint:poll
          in
          source, input_bytes)
      |> Result.map ~f:(fun (selected_child, selected_bytes) ->
        Model_selection.
          { source = selected_child; input_bytes = selected_bytes; work_steps = !used })
    with
    | Abort.Work_limit ->
      Error
        (P.Error.create
           Resource_limit
           ~message:"Captured Model source work allowance exhausted"
           ~retryable:false
           ())
;;
