open Core
module C = Tool_capability
module D = Chatmd_shell_spec.Diagnostic
module Source_ref = Chatmd_shell_spec.Source_ref

type t =
  { source : string
  ; source_ref : Source_ref.t
  ; program : Chatml_host_runtime.compiled_script
  ; capabilities : C.t
  ; fingerprint : string
  }

let entrypoint = "main"
let source t = t.source
let source_ref t = t.source_ref
let program t = t.program
let capabilities t = t.capabilities
let fingerprint t = t.fingerprint

let submitted_source source =
  let line = ref 1
  and column = ref 0 in
  String.iter source ~f:(function
    | '\n' ->
      Int.incr line;
      column := 0
    | _ -> Int.incr column);
  let source_digest = Source_ref.digest source in
  Source_ref.
    { file = "one-off-" ^ source_digest ^ ".chatml"
    ; source_dir = "."
    ; prompt_dir = "."
    ; namespace = None
    ; start_pos = { offset = 0; line = 1; column = 0 }
    ; end_pos = { offset = String.length source; line = !line; column = !column }
    ; source_sha256 = source_digest
    }
;;

let compiler_diagnostic (source_ref : Source_ref.t) (error : Chatml_compilation.error) =
  let code, message, source_ref =
    match error.diagnostic with
    | None -> error.code, error.message, source_ref
    | Some diagnostic ->
      let code =
        match diagnostic.stage with
        | Parse -> "chatml.parse_error"
        | Typecheck -> "chatml.type_error"
      in
      let position (position : Source.position) : Source_ref.position =
        { offset = position.offset; line = position.line; column = position.column }
      in
      let source_ref =
        match diagnostic.span with
        | None -> source_ref
        | Some span ->
          { source_ref with
            start_pos = position span.left
          ; end_pos = position span.right
          }
      in
      code, diagnostic.message, source_ref
  in
  D.error
    ~source:source_ref
    ~path:[ "source" ]
    ~hints:[ "One-off scripts require main : json -> json task." ]
    ~code
    message
;;

let prepare_in_domain
      ?(limits = Chatml_compilation.default_limits)
      ~env
      ~capabilities
      ~tools
      ~source
      ()
  =
  let open Result.Let_syntax in
  let%bind () =
    Chatml_compilation.validate_limits limits
    |> Result.map_error ~f:(fun error ->
      [ D.error ~path:[ "limits" ] ~code:error.code error.message ])
  in
  (* Reject oversized source before hashing or constructing provenance. Compiler
     policy validation remains in the compiler service. *)
  let%bind () =
    if String.length source > limits.max_source_bytes
    then
      Error
        [ D.error
            ~path:[ "source" ]
            ~code:"chatml.source_limit"
            "script exceeds the compiler source limit"
        ]
    else Ok ()
  in
  let source_ref = submitted_source source in
  let%bind selected =
    C.select capabilities ~names:tools
    |> Result.map_error ~f:(fun error ->
      [ D.error ~source:source_ref ~path:[ "tools" ] ~code:error.C.code error.message ])
  in
  let%bind program =
    Chatml_compilation.compile ~limits ~env ~target:One_off_v1 ~source ()
    |> Result.map_error ~f:(fun error -> [ compiler_diagnostic source_ref error ])
  in
  let fingerprint =
    [%sexp
      ("ochat.one-off-script.v1" : string)
    , (source_ref.source_sha256 : string)
    , (entrypoint : string)
    , (Chatml_compilation.contract One_off_v1 : Sexp.t)
    , (C.fingerprint selected : string)]
    |> Sexp.to_string
    |> Source_ref.digest
  in
  Ok { source; source_ref; program; capabilities = selected; fingerprint }
;;

let revalidate t ~capabilities =
  let open Result.Let_syntax in
  let%bind selected =
    C.select
      capabilities
      ~names:(List.map (C.references t.capabilities) ~f:(fun r -> r.name))
  in
  match String.equal (C.fingerprint selected) (C.fingerprint t.capabilities) with
  | true -> Ok ()
  | false ->
    Error
      C.
        { code = "capability.stale_binding"
        ; message = "selected one-off tool bindings changed"
        }
;;
