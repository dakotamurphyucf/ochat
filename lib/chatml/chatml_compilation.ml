open Core
module X = Chatml.Chatml_extension_surface
module Transport = Chatml_host_runtime.Private_compiler_transport

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

type request =
  { version : int
  ; target : target
  ; source : string
  ; contract : Sexp.t
  }
[@@deriving sexp]

type response =
  | Compiled of
      { version : int
      ; target : target
      ; contract : Sexp.t
      ; artifact : Sexp.t
      }
  | Rejected of error
[@@deriving sexp]

let max_wire_bytes = 16 * 1024 * 1024
let max_error_bytes = 16 * 1024
let bounded_message message = String.prefix message max_error_bytes
let error code message = Error { code; message = bounded_message message }

let surface = function
  | One_off_v1 -> X.one_off_v1, X.one_off_entrypoints
  | Tool_v1 -> X.tool_v1, X.tool_entrypoints
  | Moderator_v1 -> X.moderator_v1, X.moderator_entrypoints
  | Delegated_moderator_v1 -> X.delegated_moderator_v1, X.moderator_entrypoints
;;

(* Bind the worker to the exact host type surface and entrypoint contracts.
   Compiler/transport semantics are versioned separately by the envelope. *)
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

(* Check nesting before Sexplib allocates a recursive tree. The writer emits
   canonical sexps; comments and alternate syntaxes are not needed on this pipe. *)
let parse_wire text =
  if String.length text > max_wire_bytes then failwith "compiler transport byte limit";
  let depth = ref 0
  and quoted = ref false
  and escaped = ref false in
  String.iteri text ~f:(fun i c ->
    if !quoted
    then (
      if !escaped
      then escaped := false
      else if Char.equal c '\\'
      then escaped := true
      else if Char.equal c '"'
      then quoted := false)
    else (
      match c with
      | '"' -> quoted := true
      | '(' ->
        incr depth;
        if !depth > 512 then failwith "compiler transport depth limit"
      | ')' ->
        decr depth;
        if !depth < 0 then failwith "invalid compiler transport"
      | ';' -> failwith "noncanonical compiler transport"
      | '#'
        when i + 1 < String.length text
             && (Char.equal text.[i + 1] '|' || Char.equal text.[i + 1] ';') ->
        failwith "noncanonical compiler transport"
      | _ -> ()));
  if !quoted || !depth <> 0 then failwith "incomplete compiler transport";
  Sexp.of_string text
;;

let bounded_read flow =
  let buffer = Buffer.create 4096
  and chunk = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow chunk with
    | n ->
      if n > max_wire_bytes - Buffer.length buffer
      then failwith "compiler transport byte limit";
      Buffer.add_string buffer (Cstruct.to_string ~len:n chunk);
      loop ()
    | exception End_of_file -> Buffer.contents buffer
  in
  loop ()
;;

let compile ?(limits = default_limits) ~env ~worker ~target ~source () =
  if
    (not (Float.is_finite limits.wall_seconds))
    || Float.(limits.wall_seconds <= 0. || limits.wall_seconds > 30.)
    || limits.max_source_bytes <= 0
    || limits.max_source_bytes > 1024 * 1024
  then error "chatml.invalid_limits" "unsupported compiler resource limits"
  else if String.length source > limits.max_source_bytes
  then error "chatml.source_limit" "script exceeds the compiler source limit"
  else if not (Filename.is_absolute worker)
  then
    error "chatml.compiler_unavailable" "compiler worker must be an absolute trusted path"
  else (
    try
      let wire =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) limits.wall_seconds (fun () ->
          Eio.Switch.run (fun sw ->
            let mgr = Eio.Stdenv.process_mgr env in
            let input_r, input_w = Eio.Process.pipe ~sw mgr in
            let output_r, output_w = Eio.Process.pipe ~sw mgr in
            let child =
              Eio.Process.spawn
                ~sw
                mgr
                ~executable:worker
                ~env:[| "LANG=C"; "LC_ALL=C" |]
                ~stdin:input_r
                ~stdout:output_w
                ~stderr:output_w
                [ worker
                ; Int.to_string (Int.of_float (Float.round_up limits.wall_seconds))
                ]
            in
            Eio.Flow.close input_r;
            Eio.Flow.close output_w;
            Exn.protect
              ~f:(fun () ->
                Eio.Fiber.fork ~sw (fun () ->
                  Eio.Flow.copy_string
                    (sexp_of_request
                       { version = 1; target; source; contract = contract target }
                     |> Sexp.to_string)
                    input_w;
                  Eio.Flow.close input_w);
                let output = bounded_read output_r in
                match Eio.Process.await child with
                | `Exited 0 -> output
                | _ -> failwith "compiler worker failed or exceeded its resource limits")
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  (* Reaping belongs to switch release: its reaper daemon may
                     already be cancelled here. Awaiting it before releasing
                     the switch would deadlock on timeout. *)
                  Eio.Process.signal child Stdlib.Sys.sigkill))))
      in
      match response_of_sexp (parse_wire wire) with
      | Rejected e -> Error { e with message = bounded_message e.message }
      | Compiled { version = 1; target = actual; contract = actual_contract; artifact }
        when equal_target target actual && Sexp.equal (contract target) actual_contract ->
        let surface, _ = surface target in
        Ok (Transport.import ~surface ~source artifact)
      | _ -> error "chatml.compiler_protocol" "compiler contract mismatch"
    with
    | Eio.Time.Timeout ->
      error "chatml.compile_timeout" "compiler wall-time limit exceeded"
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ ->
      error
        "chatml.compiler_failed"
        "compiler unavailable, resource limit, or invalid response")
;;

let set_limit resource value =
  let value = Core_unix.RLimit.Limit.Limit (Int64.of_int value) in
  Core_unix.RLimit.set resource { cur = value; max = value }
;;

let worker_main () =
  let response =
    try
      let cpu =
        match Sys.get_argv () with
        | [| _; cpu |] -> Int.of_string cpu
        | _ -> failwith "invalid worker arguments"
      in
      if cpu <= 0 || cpu > 30 then failwith "invalid worker limits";
      set_limit Core_unix.RLimit.cpu_seconds cpu;
      set_limit Core_unix.RLimit.file_size 0;
      set_limit Core_unix.RLimit.num_file_descriptors 32;
      let buffer = Buffer.create 4096
      and chunk = Bytes.create 4096 in
      let rec read () =
        let n = Stdlib.input Stdlib.stdin chunk 0 4096 in
        if n > 0
        then (
          if n > max_wire_bytes - Buffer.length buffer then failwith "request too large";
          Buffer.add_subbytes buffer chunk ~pos:0 ~len:n;
          read ())
      in
      read ();
      let request = Buffer.contents buffer |> parse_wire |> request_of_sexp in
      if
        request.version <> 1
        || String.length request.source > 1024 * 1024
        || not (Sexp.equal request.contract (contract request.target))
      then failwith "invalid compiler request";
      let surface, required_bindings = surface request.target in
      match
        Chatml_host_runtime.compile_script
          ~surface
          ~required_bindings
          ~source:request.source
          ()
      with
      | Error message ->
        Rejected { code = "chatml.invalid_handler"; message = bounded_message message }
      | Ok compiled ->
        Compiled
          { version = 1
          ; target = request.target
          ; contract = request.contract
          ; artifact = Transport.export compiled
          }
    with
    | _ ->
      Rejected
        { code = "chatml.compiler_failed"
        ; message = "compiler resource or request failure"
        }
  in
  let output = sexp_of_response response |> Sexp.to_string in
  let output =
    try
      ignore (parse_wire output : Sexp.t);
      output
    with
    | _ ->
      sexp_of_response
        (Rejected
           { code = "chatml.compiler_limit"
           ; message = "compiled artifact exceeds transport limits"
           })
      |> Sexp.to_string
  in
  Stdlib.output_string Stdlib.stdout output;
  Stdlib.flush Stdlib.stdout
;;
