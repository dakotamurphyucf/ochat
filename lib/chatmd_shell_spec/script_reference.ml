open Core
module S = Shell_spec

type t =
  { path : string list
  ; script : string
  ; kind : Chatmd_script_spec.kind
  ; entrypoint : string option
  }

let reference path script kind = { path; script; kind; entrypoint = None }

let matcher path value =
  let rec loop pending found =
    match pending with
    | [] -> List.rev found
    | [] :: pending -> loop pending found
    | (value :: rest) :: pending ->
      let pending = rest :: pending in
      (match value with
       | S.Chatml_match value ->
         loop
           pending
           ({ path
            ; script = value.script
            ; kind = Shell_matcher
            ; entrypoint = Some value.function_
            }
            :: found)
       | All values | Any values -> loop (values :: pending) found
       | Not value -> loop ([ value ] :: pending) found
       | Any_command
       | Program _
       | Basename _
       | Resolved_path _
       | Trusted_executable
       | Program_regex _
       | Argv_prefix _
       | Argument _
       | Argument_contains _
       | Effect _
       | No_unknown_effects
       | Raw_shell_request -> loop pending found)
  in
  loop [ [ value ] ] []
;;

let of_runtime runtime =
  let path section id = [ "runtime"; S.Runtime_id.to_string runtime.S.id; section; id ] in
  let policy = Option.value_exn runtime.policy in
  let rules =
    List.concat_map policy.rules ~f:(fun rule ->
      matcher (path "policy" rule.id) rule.matcher)
  in
  let reviewers =
    Option.value_map runtime.reviewers ~default:[] ~f:(fun values ->
      List.concat_map values.values ~f:(function
        | S.Chatml_reviewer { id; script; _ } ->
          [ reference (path "reviewers" id) script Shell_reviewer ]
        | Ui_reviewer _ | Model_reviewer _ | Executable_reviewer _ -> []))
  in
  let interceptors =
    Option.value_map runtime.interceptors ~default:[] ~f:(fun values ->
      List.concat_map values.values ~f:(fun (value : S.interceptor) ->
        let at = path "interceptors" value.id in
        let predicates =
          match value.phase with
          | Before -> Option.value_map value.matcher ~default:[] ~f:(matcher at)
          | After -> []
        in
        let hook =
          match value.extension with
          | S.Chatml_extension { script; _ } ->
            let kind =
              match value.phase with
              | Before -> Chatmd_script_spec.Shell_before_interceptor
              | After -> Shell_after_interceptor
            in
            [ reference at script kind ]
          | Executable_extension _ -> []
        in
        predicates @ hook))
  in
  let analyzers =
    Option.value_map runtime.effect_analysis ~default:[] ~f:(fun values ->
      List.concat_map values.analyzers ~f:(fun (value : S.effect_analyzer) ->
        match value.extension with
        | S.Chatml_extension { script; _ } ->
          [ reference (path "effect_analysis" value.id) script Shell_effect_analyzer ]
        | Executable_extension _ -> []))
  in
  let audit = Option.value_exn runtime.audit in
  let filters =
    Option.value_map audit.filter ~default:[] ~f:(function
      | S.Chatml_extension { script; _ } ->
        [ reference (path "audit" "filter") script Shell_audit_filter ]
      | Executable_extension _ -> [])
  in
  rules @ reviewers @ interceptors @ analyzers @ filters
;;
