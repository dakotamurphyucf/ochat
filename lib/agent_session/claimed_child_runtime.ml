open! Core
module P = Agent_protocol
module B = Chat_response.Background_request
module C = Chat_response.Tool_capability
module Runtime = Chat_response.Agent_runtime
module Source = Captured_agent_runner.Source_binding.Claimed_source
module Scripts = Runtime_script_preparation

type t =
  { source : Source.t
  ; selected : C.t
  ; pinned : C.t
  ; managed : Chat_response.Managed_tool_registry.t
  ; native : Runtime.t
  ; switch : Eio.Switch.t
  ; domain_id : int
  ; live : bool Atomic.t
  ; check_current : unit -> (unit, P.Error.t) result
  ; check_local_now : unit -> (unit, P.Error.t) result
  }

let key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let invalid message = P.Error.invalid_request message

let runtime_policy_identity ~base ~policy =
  [%sexp
    ("queued-selected-issuer.v3" : string)
  , (Chatml_source.Digest.to_hex base : string)
  , (policy : Chat_response.Runtime_semantics.policy)]
  |> Sexp.to_string_mach
  |> fun contents -> Chatml_source.Digest.of_contents contents ~checkpoint:Eio.Fiber.check
;;

let selected t = t.selected
let pinned_capabilities t = t.pinned
let managed_definition t = t.managed
let native_runtime t = t.native
let issuer_source t = Source.issuer_source t.source
let same_parent t parent = Source.same_parent t.source parent

let check_scope_now t =
  let open Result.Let_syntax in
  if
    Atomic.get t.live
    && Int.equal t.domain_id (Stdlib.Domain.self () :> int)
    && Option.value_map (Eio.Fiber.get key) ~default:false ~f:(phys_equal t)
  then (
    Eio.Switch.check t.switch;
    let%bind () = Source.check_now t.source in
    B.validate_capabilities (Source.request t.source) ~capabilities:t.pinned)
  else Error (invalid "claimed child runtime has expired")
;;

let check_now t =
  let open Result.Let_syntax in
  let%bind () = check_scope_now t in
  t.check_local_now ()
;;

let check t =
  let open Result.Let_syntax in
  let%bind () = check_scope_now t in
  let%bind () = t.check_current () in
  check_now t
;;

let admit_claimed_registrations
      ~native
      ~selected
      ~registrations
      ~max_native_bindings
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind () =
    if C.length selected <= max_native_bindings
    then Ok ()
    else Error (invalid "claimed child native binding limit exceeded")
  in
  let%bind () =
    Runtime.admit_native_selection
      native
      ~selected
      ~max_bindings:max_native_bindings
      ~checkpoint
  in
  let references = C.references selected in
  List.fold_result registrations ~init:() ~f:(fun () registration ->
    let%bind () =
      Runtime.admit_exact_saved_agent_registration
        native
        ~registration
        ~max_registrations:max_native_bindings
        ~checkpoint
    in
    let%bind matches =
      List.fold_result references ~init:0 ~f:(fun matches reference ->
        checkpoint ();
        let%bind binding =
          C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map_error ~f:(fun error -> invalid error.C.message)
        in
        Ok
          (if
             Option.value_map
               (C.native_implementation binding)
               ~default:false
               ~f:(phys_equal registration.implementation)
           then matches + 1
           else matches))
    in
    if Int.equal matches 1
    then Ok ()
    else Error (invalid "claimed Agent registration is not uniquely selected"))
;;

let with_prepared
      ~source
      ~script_preparation
      ~env
      ~host
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~max_native_bindings
      ~prepare_agent_registrations
      ~checkpoint
      ~check_current
      ~check_local_now
      f
  =
  let open Result.Let_syntax in
  let%bind () =
    if max_native_bindings > 0
    then Ok ()
    else Error (invalid "claimed child native binding limit is invalid")
  in
  let%bind () = Source.check_now source in
  let%bind () = check_current () in
  let%bind () = check_local_now () in
  let revision = Source.revision source in
  let tool_contract = Source.tool_contract source in
  Developer_claimed_resource_owner.with_switch (fun sw ->
    Scripts.with_revision
      (Some script_preparation)
      ~tool_contract
      revision
      ~env
      ~invocation:Scripts.Invocation.Headless
      ~checkpoint
      ~f:(fun startup ~compilation_limits ~programs ->
        let%bind () = Source.check_now source in
        let%bind () = check_current () in
        let%bind claimed_registration = prepare_agent_registrations ~source ~sw in
        let%bind native_registrations =
          Captured_agent_runner.Deferred_developer.Claimed.registrations
            claimed_registration
            ~source
        in
        let%bind parsed =
          Prompt_revision.parser_context revision
          |> Result.of_option
               ~error:(invalid "claimed child has no retained parser context")
        in
        let ordinary_agent_tools =
          if List.is_empty native_registrations
          then Developer_child_preflight.Denied
          else Registered
        in
        let%bind _needs_dynamic_source =
          Developer_child_preflight.classify_selected
            ~ordinary_agent_tools
            ~revision
            ~parsed
            ~input:[]
            ~checkpoint
        in
        let%bind () = Source.check_now source in
        let%bind () = check_current () in
        let%bind () = check_local_now () in
        let preparation_denial = ref None in
        let check_preparation_now () =
          match check_current () with
          | Error error ->
            preparation_denial := Some error;
            Error
              Runtime.
                { code = "developer.claimed_owner_expired"
                ; message = "claimed issuing authority expired during preparation"
                ; source = None
                }
          | Ok () ->
            (match Source.check_now source with
             | Error error ->
               preparation_denial := Some error;
               Error
                 Runtime.
                   { code = "developer.claimed_source_expired"
                   ; message = "claimed issuing source expired during preparation"
                   ; source = None
                   }
             | Ok () ->
               (match check_local_now () with
                | Ok () -> Ok ()
                | Error error ->
                  preparation_denial := Some error;
                  Error
                    Runtime.
                      { code = "developer.claimed_runtime_expired"
                      ; message = "claimed runtime owner expired during preparation"
                      ; source = None
                      }))
        in
        let%bind resources =
          Runtime.prepare_extensions_without_inference
            ~startup
            ~tool_contract
            ~compilation_limits
            ?programs
            ~checkpoint
            ~check_preparation_now
            ~native_registrations
            ~sw
            ~host
            ~platform:(Runtime.platform ())
            ~prompt_elements:(Prompt_revision.elements revision)
            ~manifest_authorizer
            ~approval_provider
            ~approval_store
            ()
          |> Result.map_error ~f:(fun diagnostics ->
            match !preparation_denial with
            | Some error -> error
            | None ->
              let details =
                List.take diagnostics 3
                |> List.map ~f:Runtime.diagnostic_to_string
                |> String.concat ~sep:"; "
                |> fun message -> String.prefix message 1024
              in
              invalid ("claimed child runtime preparation failed: " ^ details))
        in
        let selected =
          Chat_response.Managed_tool_registry.capabilities resources.managed
        in
        let%bind () =
          admit_claimed_registrations
            ~native:resources.native
            ~selected
            ~registrations:native_registrations
            ~max_native_bindings
            ~checkpoint
        in
        let%bind pinned =
          B.selected_capabilities (Source.request source) ~capabilities:selected
        in
        let%bind () = check_current () in
        let%bind () = check_local_now () in
        let scoped =
          { source
          ; selected
          ; pinned
          ; managed = resources.managed
          ; native = resources.native
          ; switch = sw
          ; domain_id = (Stdlib.Domain.self () :> int)
          ; live = Atomic.make true
          ; check_current
          ; check_local_now
          }
        in
        Exn.protect
          ~finally:(fun () -> Atomic.set scoped.live false)
          ~f:(fun () ->
            Eio.Fiber.with_binding key scoped (fun () ->
              let%bind () = check scoped in
              let%bind value = f scoped in
              let%map () = check scoped in
              value))))
;;
