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

let admit revision ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let artifact = Prompt_revision.artifact revision in
  let%bind root_relative_path =
    Source.Portable_path.create artifact.root_relative_path
    |> Result.map_error ~f:(fun _ -> unavailable "Selected artifact root is not portable")
  in
  let%map manifest_sha256 =
    Chatml_source.Digest.of_hex artifact.manifest_sha256
    |> Result.map_error ~f:(fun _ ->
      unavailable "Selected artifact manifest digest is invalid")
  in
  { revision
  ; reference =
      B.Agent_artifact.create
        ~revision_id:artifact.revision_id
        ~manifest_sha256
        ~root_relative_path
  }
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
              Prompt_source_entry.of_reference
                ~parent
                reference
                ~max_work_steps:(max_work_steps - !used)
                ~checkpoint:poll
              |> Result.map_error ~f:(fun error ->
                unavailable (Error.to_string_hum error))
            in
            let%bind result =
              Prompt_source_lease.with_scope source_lease ~f:(fun scope ->
                let%map capture =
                  Owner.Scope.capture_entry
                    scope
                    ~entry
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
                     |> Result.map_error ~f:diagnostics))
              |> Result.map_error ~f:owner_error
            in
            let%bind revision = result in
            poll ();
            admit revision ~checkpoint:poll))
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
      let%bind () =
        Store.install artifact_store ~transaction_id:(P.Id.Transaction.create ()) artifact
        |> Result.map_error ~f:store_error
      in
      poll ();
      let%bind selected = admit revision ~checkpoint:poll in
      (* Rebase through the verified immutable child tree, never the parent's
       authoring or artifact directory. Stored content is independently admitted. *)
      restore
        ~artifact_store
        ~definition:(Prompt_revision.definition parent)
        ~content_limits
        selected.reference
        ~checkpoint:poll)
;;
