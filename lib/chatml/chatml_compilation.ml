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

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

let max_error_bytes = 16 * 1024
let bounded_message message = String.prefix message max_error_bytes
let error code message = Error { code; message = bounded_message message }

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

(* Compilation owns its mutable inference/resolution state. Only immutable source,
   builtin type descriptions and the finished program cross the domain boundary.
   Checkpoints deliberately live between stages: this is cooperative cancellation,
   not a hard time or memory sandbox around arbitrary compiler code. *)
let compile ?(limits = default_limits) ~env ~target ~source () =
  if
    (not (Float.is_finite limits.wall_seconds))
    || Float.(limits.wall_seconds <= 0. || limits.wall_seconds > 30.)
    || limits.max_source_bytes <= 0
    || limits.max_source_bytes > 1024 * 1024
  then error "chatml.invalid_limits" "unsupported compiler resource limits"
  else if String.length source > limits.max_source_bytes
  then error "chatml.source_limit" "script exceeds the compiler source limit"
  else (
    let clock = Eio.Stdenv.mono_clock env in
    let started = Eio.Time.Mono.now clock in
    let checkpoint () =
      Eio.Fiber.yield ();
      let elapsed = Mtime.span started (Eio.Time.Mono.now clock) in
      if Float.(Mtime.Span.to_float_ns elapsed >= limits.wall_seconds *. 1e9)
      then raise Eio.Time.Timeout
    in
    try
      Eio.Time.Timeout.run_exn
        (Eio.Time.Timeout.seconds clock limits.wall_seconds)
        (fun () ->
           Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
             let surface, required_bindings = surface target in
             match
               Chatml_host_runtime.compile_script
                 ~checkpoint
                 ~surface
                 ~required_bindings
                 ~source
                 ()
             with
             | Ok compiled -> Ok compiled
             | Error message -> error "chatml.invalid_handler" message))
    with
    | Eio.Time.Timeout ->
      error "chatml.compile_timeout" "compiler cooperative time budget exceeded"
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | Stack_overflow -> error "chatml.compiler_limit" "compiler stack limit exceeded"
    | _ -> error "chatml.compiler_failed" "compiler failed unexpectedly")
;;
