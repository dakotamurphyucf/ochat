open Core
module X = Chatml.Chatml_extension_surface

type target =
  | One_off_v1
  | Tool_v1
  | Moderator_v1
  | Delegated_moderator_v1
[@@deriving sexp, equal]

type limits =
  { wall_seconds : float
  ; max_source_bytes : int
  }

let default_limits = { wall_seconds = 5.; max_source_bytes = 256 * 1024 }

type policy =
  | Unrestricted
  | Bounded of limits

type error =
  { code : string
  ; message : string
  ; diagnostic : Chatml_host_runtime.compilation_diagnostic option
  }
[@@deriving sexp_of]

let max_error_bytes = 16 * 1024
let bounded_message message = String.prefix message max_error_bytes

let error ?diagnostic code message =
  Error { code; message = bounded_message message; diagnostic }
;;

let surface = function
  | One_off_v1 -> X.one_off_v1, X.one_off_entrypoints
  | Tool_v1 -> X.tool_v1, X.tool_entrypoints
  | Moderator_v1 -> X.moderator_v1, X.moderator_entrypoints
  | Delegated_moderator_v1 -> X.delegated_moderator_v1, X.moderator_entrypoints
;;

let static_target = function
  | One_off_v1 -> Chatml.Chatml_compiler_contract.Target.One_off_v1
  | Tool_v1 -> Tool_v1
  | Moderator_v1 -> Moderator_v1
  | Delegated_moderator_v1 -> Delegated_moderator_v1
;;

(* Keep the existing standard compiler.v1 identity; the owner computes it from
   actual callback-free metadata rather than accepting a caller's second claim. *)
let contract target =
  let checkpoint () = () in
  Chatml.Chatml_compiler_contract.for_target (static_target target) ~checkpoint
  |> fun contract -> Chatml.Chatml_compiler_contract.identity contract ~checkpoint
;;

let validate_limits limits =
  if
    (not (Float.is_finite limits.wall_seconds))
    || Float.(limits.wall_seconds <= 0.)
    || limits.max_source_bytes <= 0
  then error "chatml.invalid_limits" "invalid compiler resource limits"
  else Ok ()
;;

(* Compilation owns its mutable inference/resolution state. Only immutable source,
   builtin type descriptions and the finished program cross the domain boundary.
   Checkpoints run between stages and within inference: this is cooperative cancellation,
   not a hard time or memory sandbox around arbitrary compiler code. *)
module Input = struct
  type t =
    | Text of string
    | Captured of Chatml_source.t

  let byte_length = function
    | Text contents -> String.length contents
    | Captured source -> Chatml_source.byte_length source
  ;;

  let compile t ~checkpoint ~surface ~required_bindings =
    match t with
    | Text source ->
      Chatml_host_runtime.compile_script_detailed
        ~checkpoint
        ~surface
        ~required_bindings
        ~source
        ()
    | Captured source ->
      Chatml_host_runtime.compile_captured_script
        source
        ~checkpoint
        ~surface
        ~required_bindings
        ()
  ;;
end

(* Text/captured, linked and semantic callers share admission, elapsed policy,
   exception mapping and joined domain cleanup. Metadata preparation runs under
   that SAME budget before
   domain creation, so static projection cannot evade bounded compilation. *)
type exception_policy =
  | Legacy_mapping
  | Preserve_unexpected

let run_worker
      ~input_bytes
      ~policy
      ~env
      ~map_policy_error
      ~exception_policy
      ~prepare
      ~compile
  =
  let open Result.Let_syntax in
  let%bind () =
    Result.map_error
      (match policy with
       | Unrestricted -> Ok ()
       | Bounded limits ->
         let%bind () = validate_limits limits in
         if input_bytes > limits.max_source_bytes
         then error "chatml.source_limit" "script exceeds the compiler source limit"
         else Ok ())
      ~f:map_policy_error
  in
  let execute () =
    let clock = Eio.Stdenv.mono_clock env in
    let started = Eio.Time.Mono.now clock in
    let checkpoint () =
      Eio.Fiber.yield ();
      match policy with
      | Unrestricted -> ()
      | Bounded limits ->
        let elapsed = Mtime.span started (Eio.Time.Mono.now clock) in
        if Float.(Mtime.Span.to_float_ns elapsed >= limits.wall_seconds *. 1e9)
        then raise Eio.Time.Timeout
    in
    let%bind input = prepare ~checkpoint in
    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      compile input ~checkpoint)
  in
  try
    match policy with
    | Unrestricted -> execute ()
    | Bounded limits ->
      Eio.Time.Timeout.run_exn
        (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) limits.wall_seconds)
        execute
  with
  | Eio.Time.Timeout ->
    Result.map_error
      (error "chatml.compile_timeout" "compiler cooperative time budget exceeded")
      ~f:map_policy_error
  | Eio.Cancel.Cancelled _ as exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Exn.raise_with_original_backtrace exn backtrace
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (match exception_policy with
     | Preserve_unexpected -> Exn.raise_with_original_backtrace exn backtrace
     | Legacy_mapping ->
       let failure =
         match exn with
         | Stack_overflow -> error "chatml.compiler_limit" "compiler stack limit exceeded"
         | _ -> error "chatml.compiler_failed" "compiler failed unexpectedly"
       in
       Result.map_error failure ~f:map_policy_error)
;;

let compile_input source ~policy ~env ~target =
  run_worker
    ~map_policy_error:Fn.id
    ~exception_policy:Legacy_mapping
    ~input_bytes:(Input.byte_length source)
    ~policy
    ~env
    ~prepare:(fun ~checkpoint:_ -> Ok (source, target))
    ~compile:(fun (source, target) ~checkpoint ->
      let surface, required_bindings = surface target in
      match Input.compile source ~checkpoint ~surface ~required_bindings with
      | Ok compiled -> Ok compiled
      | Error diagnostic ->
        error ~diagnostic "chatml.invalid_handler" diagnostic.formatted)
;;

let compile_with_policy ~policy ~env ~target ~source () =
  compile_input (Input.Text source) ~policy ~env ~target
;;

let compile ?(limits = default_limits) ~env ~target ~source () =
  compile_with_policy ~policy:(Bounded limits) ~env ~target ~source ()
;;

let compile_captured_with_policy source ~policy ~env ~target () =
  compile_input (Input.Captured source) ~policy ~env ~target
;;

let compile_captured source ?(limits = default_limits) ~env ~target () =
  compile_captured_with_policy source ~policy:(Bounded limits) ~env ~target ()
;;

let compile_prepared_with_policy input ~policy ~max_dependency_depth ~env ~target () =
  run_worker
    ~map_policy_error:Fn.id
    ~exception_policy:Legacy_mapping
    ~input_bytes:
      (Chatml_prepared_program.Accounting.input_bytes
         (Chatml_prepared_program.accounting input))
    ~policy
    ~env
    ~prepare:(fun ~checkpoint ->
      Ok
        ( input
        , Chatml.Chatml_compiler_contract.for_target (static_target target) ~checkpoint ))
    ~compile:(fun (input, contract) ~checkpoint ->
      match
        Chatml_linked_program.compile input ~contract ~max_dependency_depth ~checkpoint
      with
      | Ok compiled -> Ok compiled
      | Error failure ->
        let stage =
          match Chatml_linked_program.Failure.stage failure with
          | Parse -> Chatml_host_runtime.Parse
          | Typecheck -> Typecheck
          | Link -> Link
        in
        let diagnostic =
          Chatml_host_runtime.compilation_diagnostic
            (Chatml_linked_program.Failure.diagnostic failure)
            ~stage
            ~source_text:None
            ~checkpoint
        in
        error ~diagnostic "chatml.invalid_handler" diagnostic.formatted)
;;

let compile_prepared
      input
      ?(limits = default_limits)
      ~max_dependency_depth
      ~env
      ~target
      ()
  =
  compile_prepared_with_policy
    input
    ~policy:(Bounded limits)
    ~max_dependency_depth
    ~env
    ~target
    ()
;;

module Analysis_error = struct
  type t =
    | Policy of error
    | Analysis of Chatml_semantics.Error.t
end

let analyze_prepared_with_policy
      input
      ~policy
      ~semantic_limits
      ~max_dependency_depth
      ~env
      ~target
      ()
  =
  run_worker
    ~map_policy_error:(fun error -> Analysis_error.Policy error)
    ~exception_policy:Preserve_unexpected
    ~input_bytes:
      (Chatml_prepared_program.Accounting.input_bytes
         (Chatml_prepared_program.accounting input))
    ~policy
    ~env
    ~prepare:(fun ~checkpoint ->
      Ok
        ( input
        , Chatml.Chatml_compiler_contract.for_target (static_target target) ~checkpoint ))
    ~compile:(fun (input, contract) ~checkpoint ->
      Chatml_semantics.analyze
        input
        ~contract
        ~max_dependency_depth
        ~limits:semantic_limits
        ~checkpoint
      |> Result.map_error ~f:(fun error -> Analysis_error.Analysis error))
;;

let analyze_prepared
      input
      ?(limits = default_limits)
      ~semantic_limits
      ~max_dependency_depth
      ~env
      ~target
      ()
  =
  analyze_prepared_with_policy
    input
    ~policy:(Bounded limits)
    ~semantic_limits
    ~max_dependency_depth
    ~env
    ~target
    ()
;;
