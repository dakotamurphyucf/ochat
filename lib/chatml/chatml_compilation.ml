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
[@@deriving sexp]

let max_error_bytes = 16 * 1024
let bounded_message message = String.prefix message max_error_bytes

let error ?diagnostic code message =
  let diagnostic =
    Option.map
      diagnostic
      ~f:(fun (diagnostic : Chatml_host_runtime.compilation_diagnostic) ->
        { diagnostic with
          message = bounded_message diagnostic.message
        ; formatted = bounded_message diagnostic.formatted
        })
  in
  Error { code; message = bounded_message message; diagnostic }
;;

let surface = function
  | One_off_v1 -> X.one_off_v1, X.one_off_entrypoints
  | Tool_v1 -> X.tool_v1, X.tool_entrypoints
  | Moderator_v1 -> X.moderator_v1, X.moderator_entrypoints
  | Delegated_moderator_v1 -> X.delegated_moderator_v1, X.moderator_entrypoints
;;

(* Cache identity includes the exact host type surface and entrypoint contracts. *)
let contract target =
  let module B = Chatml.Chatml_builtin_spec in
  let module S = Chatml.Chatml_builtin_surface in
  let surface, entrypoints = surface target in
  let bindings entries =
    List.map entries ~f:(fun (entry : B.builtin) -> entry.name, entry.scheme)
  in
  [%sexp
    ("ochat.chatml.compiler.v1" : string)
  , (target : target)
  , (bindings surface.globals : (string * B.ty) list)
  , (List.map surface.modules ~f:(fun (m : B.builtin_module) ->
       m.name, bindings m.exports)
     : (string * (string * B.ty) list) list)
  , (List.map surface.type_aliases ~f:(fun (a : S.builtin_type_alias) -> a.name, a.body)
     : (string * B.ty) list)
  , (entrypoints : (string * B.ty) list)]
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
let compile_with_policy ~policy ~env ~target ~source () =
  let open Result.Let_syntax in
  let%bind () =
    match policy with
    | Unrestricted -> Ok ()
    | Bounded limits ->
      let%bind () = validate_limits limits in
      if String.length source > limits.max_source_bytes
      then error "chatml.source_limit" "script exceeds the compiler source limit"
      else Ok ()
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
    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
      let surface, required_bindings = surface target in
      match
        Chatml_host_runtime.compile_script_detailed
          ~checkpoint
          ~surface
          ~required_bindings
          ~source
          ()
      with
      | Ok compiled -> Ok compiled
      | Error diagnostic ->
        error ~diagnostic "chatml.invalid_handler" diagnostic.formatted)
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
    error "chatml.compile_timeout" "compiler cooperative time budget exceeded"
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Stack_overflow -> error "chatml.compiler_limit" "compiler stack limit exceeded"
  | _ -> error "chatml.compiler_failed" "compiler failed unexpectedly"
;;

let compile ?(limits = default_limits) ~env ~target ~source () =
  compile_with_policy ~policy:(Bounded limits) ~env ~target ~source ()
;;
