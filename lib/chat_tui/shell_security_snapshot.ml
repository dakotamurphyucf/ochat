open! Core
module S = Chatmd_shell_spec.Shell_spec
module Page = Model.Shell_security_page_state

let sandbox runtime =
  match
    Option.bind runtime.S.capabilities ~f:(fun capabilities ->
      match capabilities.sandbox with
      | S.Set value -> Some value
      | Inherit | Clear -> None)
  with
  | Some S.Required -> "required"
  | Some Preferred -> "preferred"
  | Some Direct_unsafe -> "direct unsafe"
  | None -> "unresolved"
;;

let network runtime =
  Option.exists runtime.S.capabilities ~f:(fun capabilities ->
    match capabilities.network with
    | S.Set value -> value
    | Inherit | Clear -> false)
;;

let audit_name runtime =
  match Option.map runtime.S.audit ~f:(fun audit -> audit.format) with
  | None | Some S.No_audit -> "none"
  | Some Stderr -> "stderr"
  | Some Jsonl -> "jsonl"
  | Some Session -> "session"
;;

let runtime runtime =
  let specification = Shell_runtime.Runtime.spec runtime in
  Page.
    { id = Shell_runtime.Runtime.id runtime
    ; profile = specification.resolved_profile
    ; sandbox = sandbox specification
    ; network = network specification
    ; audit = audit_name specification
    }
;;

let signature_status = function
  | None -> "not configured"
  | Some status ->
    (match status.Shell_runtime.Manifest_security.signature_key_id with
     | None -> "unsigned · accepted by policy"
     | Some key_id ->
       sprintf
         "verified · key=%s%s"
         key_id
         (Option.value_map status.signature_issuer ~default:"" ~f:(fun issuer ->
            " · issuer=" ^ issuer)))
;;

let administrative_policy = function
  | None -> "not applicable"
  | Some policy -> policy.Shell_runtime.Admin_policy.source
;;

let audit_status runtimes =
  let formats = List.map runtimes ~f:(fun runtime -> runtime.Page.audit) in
  if List.for_all formats ~f:(String.equal "none")
  then "no durable sink configured"
  else "live sinks: " ^ String.concat ~sep:", " formats
;;

let create ~agent_runtime ~session =
  let runtimes =
    Option.value_map
      agent_runtime.Chat_response.Agent_runtime.shell_registry
      ~default:[]
      ~f:(fun registry -> Shell_runtime.Registry.runtimes registry |> List.map ~f:runtime)
  in
  let shell_state =
    Option.value_map
      session
      ~default:Session.Shell_state.empty
      ~f:(fun (session : Session.t) -> session.shell_state)
  in
  Page.
    { manifest_sha256 =
        Option.map agent_runtime.shell_manifest ~f:(fun manifest ->
          manifest.Chatmd_shell_spec.Manifest.sha256)
    ; live_manifest_sha256 =
        Option.map agent_runtime.shell_registry ~f:(fun registry ->
          (Shell_runtime.Registry.manifest registry).Chatmd_shell_spec.Manifest.sha256)
    ; runtimes
    ; manifest_grants = shell_state.manifest_grants
    ; grants = shell_state.approval_grants
    ; administrative_policy = administrative_policy agent_runtime.shell_admin_policy
    ; signature_status = signature_status agent_runtime.shell_security_status
    ; audit_status = audit_status runtimes
    ; interrupted_requests = shell_state.interrupted_requests
    }
;;
