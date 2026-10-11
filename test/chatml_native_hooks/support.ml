open Core
module S = Chatml_source
module P = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected native fixture rejection"
;;

let checkpoint () = ()

let id name =
  S.Id.create ~package_namespace:"native-hooks" ~relative_path:name |> Or_error.ok_exn
;;

let capture name contents =
  S.create (id name) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let input contract files =
  let declaration = capture "provided-catalog.json" "explicit provided native catalog" in
  let declared_at =
    S.locate declaration ~start_byte:0 ~end_byte:8 ~checkpoint |> Or_error.ok_exn
  in
  let sources =
    declaration :: List.map files ~f:(fun (name, body, _) -> capture name body)
  in
  let definitions =
    List.map files ~f:(fun (name, _, _) ->
      P.Definition.create ~implementation:(id name) ~interface:None ~declared_at
      |> Or_error.ok_exn)
  in
  let scopes =
    List.map files ~f:(fun (name, _, bindings) ->
      P.Scope.create
        (id name)
        ~bindings:
          (List.map bindings ~f:(fun (name, target) ->
             P.Scope.
               { name
               ; module_id = P.Module_id.of_implementation (id target)
               ; declared_at
               }))
        ~selected_at:(Some declared_at)
        ~checkpoint
      |> Or_error.ok_exn)
  in
  let compiler_contract = Contract.fingerprint contract ~checkpoint in
  P.create
    ~max_input_bytes:None
    ~compiler_contract
    ~environment:(P.Environment.implicit ~required_contract:compiler_contract)
    ~provenance:Implicit
    ~entry:
      (id
         (let name, _, _ = List.hd_exn files in
          name))
    ~sources
    ~definitions
    ~scopes
    ~checkpoint
    ()
  |> ok
;;

let script body =
  let module Spec = Chatmd_shell_spec.Chatmd_script_spec in
  let position = Chatmd_shell_spec.Source_ref.{ offset = 0; line = 1; column = 0 } in
  let source_ref =
    Chatmd_shell_spec.Source_ref.create
      ~file:"fixture.chatmd"
      ~source_dir:"."
      ~prompt_dir:"."
      ~namespace:None
      ~start_pos:position
      ~end_pos:position
      ~source:body
  in
  Spec.
    { id = "matcher"
    ; language = "chatml"
    ; kind = Shell_matcher
    ; source = Inline body
    ; source_ref
    ; source_sha256 = Chatmd_shell_spec.Source_ref.digest body
    ; limits = default_limits
    }
;;

let compiler_limits =
  Chatml_compilation.{ wall_seconds = 10.; max_source_bytes = 8 * 1024 * 1024 }
;;

let context =
  Chatml.Chatml_lang.VRecord
    (String.Map.singleton "phase" (Chatml.Chatml_lang.VString "shell_matcher"))
;;

let event =
  Shell_runtime.Chatml_context_value.
    { phase = "shell_matcher"
    ; request_id = "request"
    ; runtime_id = "runtime"
    ; manifest_sha256 = "digest"
    ; argv = [ "actual-event" ]
    ; executable =
        { requested = "echo"
        ; path = "/bin/echo"
        ; canonical_path = "/bin/echo"
        ; trusted = true
        ; sha256 = "digest"
        }
    ; cwd = "/workspace"
    ; origin = "tool"
    ; request_kind = "structured"
    ; stdin_kind = "empty"
    ; stdin_sha256 = None
    ; stdin_bytes = 0
    ; script_sha256 = None
    ; script_preview = None
    ; effects = []
    ; capabilities =
        { read_roots = []
        ; write_roots = []
        ; network = false
        ; child_processes = false
        ; arbitrary_code = false
        ; privilege_change = false
        ; sandbox = "required"
        }
    ; session_id = Some "session"
    ; policy = None
    }
  |> Shell_runtime.Chatml_context_value.encode
;;
