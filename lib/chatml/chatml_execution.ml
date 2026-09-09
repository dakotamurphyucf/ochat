open Core

type limits =
  { fuel : int
  ; max_tasks : int
  ; wall_seconds : float
  ; max_value_bytes : int
  ; max_array_items : int
  ; max_depth : int
  ; allocation_bytes : int
  ; max_calls : int
  ; max_invocation_depth : int
  }

type policy =
  | Unrestricted
  | Bounded of limits

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

let default_limits =
  { fuel = 100_000
  ; max_tasks = 1024
  ; wall_seconds = 30.
  ; max_value_bytes = 1024 * 1024
  ; max_array_items = 16_384
  ; max_depth = 128
  ; allocation_bytes = 64 * 1024 * 1024
  ; max_calls = 100
  ; max_invocation_depth = 8
  }
;;

exception Budget_exhausted of error

type budget =
  { limits : limits
  ; active : bool Atomic.t
  ; failure : error option Atomic.t
  }

type frame =
  { active : bool Atomic.t
  ; ancestors : frame list
    (* Each entry retains remaining invocation depth at this frame. *)
  ; budgets : (int64 * budget * Chatml.Chatml_lang.execution_control) list
  }

let frame_key = Eio.Fiber.create_key ()

type context = frame list

let capture_context ?(inherited = []) () =
  let contains frame ancestor =
    phys_equal frame ancestor || List.mem frame.ancestors ancestor ~equal:phys_equal
  in
  (* A descendant already retains every ancestor's budget and lifetime. Keep
     only maximal frames so repeated native handoffs don't expand ancestry. *)
  List.fold
    (Option.to_list (Eio.Fiber.get frame_key) @ inherited)
    ~init:[]
    ~f:(fun selected frame ->
      if List.exists selected ~f:(fun other -> contains other frame)
      then selected
      else frame :: List.filter selected ~f:(fun other -> not (contains frame other)))
;;

let rec exhaust budget failure =
  match Atomic.get budget.failure with
  | Some failure -> raise (Budget_exhausted failure)
  | None ->
    if Atomic.compare_and_set budget.failure None (Some failure)
    then raise (Budget_exhausted failure)
    else exhaust budget failure
;;

let check_budget (budget : budget) =
  match Atomic.get budget.active, Atomic.get budget.failure with
  | false, _ ->
    raise
      (Budget_exhausted
         { code = "chatml.inactive_scope"
         ; message = "The owning execution scope has ended."
         })
  | true, Some failure -> raise (Budget_exhausted failure)
  | true, None -> ()
;;

let rec consume budget counter amount failure =
  check_budget budget;
  let remaining = Atomic.get counter in
  if amount < 0 || amount > remaining then exhaust budget failure;
  if not (Atomic.compare_and_set counter remaining (remaining - amount))
  then consume budget counter amount failure
;;

let error code message = Error { code; message }

let valid_limits limits =
  not
    (limits.fuel <= 0
     || limits.max_tasks < 0
     || (not (Float.is_finite limits.wall_seconds))
     || Float.(limits.wall_seconds <= 0.)
     || limits.max_value_bytes <= 0
     || limits.max_array_items <= 0
     || limits.max_depth <= 0
     || limits.allocation_bytes <= 0
     || limits.max_calls < 0
     || limits.max_invocation_depth <= 0)
;;

let make_control ~env budget =
  let limits = budget.limits in
  let fuel_failure =
    { code = "chatml.execution_limit"; message = "ChatML execution fuel exhausted" }
  in
  let allocation_failure =
    { code = "chatml.allocation_limit"; message = "ChatML allocation budget exhausted" }
  in
  let value_failure () =
    exhaust
      budget
      { code = "chatml.value_limit"
      ; message = "ChatML value size or depth limit exceeded"
      }
  in
  let remaining = Atomic.make limits.fuel in
  let until_poll = Atomic.make 0 in
  let clock = Eio.Stdenv.mono_clock env in
  let started = Eio.Time.Mono.now clock in
  let checkpoint () =
    consume budget remaining 1 fuel_failure;
    if Atomic.fetch_and_add until_poll (-1) <= 0
    then (
      Atomic.set until_poll 64;
      Eio.Fiber.yield ();
      check_budget budget;
      let elapsed = Mtime.span started (Eio.Time.Mono.now clock) in
      if Float.(Mtime.Span.to_float_ns elapsed >= limits.wall_seconds *. 1e9)
      then
        exhaust
          budget
          { code = "chatml.execution_timeout"
          ; message = "ChatML execution deadline exceeded"
          })
  in
  let allocation_remaining = Atomic.make limits.allocation_bytes in
  let allocate bytes = consume budget allocation_remaining bytes allocation_failure in
  let array_size count =
    if count > limits.max_array_items then value_failure ();
    if count > Atomic.get allocation_remaining / 16 then exhaust budget allocation_failure;
    if count >= 0 then allocate (16 * count)
  in
  let string_size count =
    if count > limits.max_value_bytes then value_failure ();
    if count >= 0 then allocate count
  in
  let measure_value ?(render_tasks = false) value =
    let bytes = ref 0 in
    let deepest = ref 0 in
    let add count =
      if count > limits.max_value_bytes - !bytes then value_failure ();
      bytes := !bytes + count
    in
    let rec visit depth (value : Chatml.Chatml_lang.value) =
      checkpoint ();
      if depth > limits.max_depth then value_failure ();
      deepest := Int.max !deepest depth;
      add 16;
      match value with
      | VString text -> add (String.length text)
      | VArray values ->
        if Array.length values > limits.max_array_items then value_failure ();
        Array.iter values ~f:(visit (depth + 1))
      | VRecord fields ->
        if Map.length fields > limits.max_array_items then value_failure ();
        Map.iteri fields ~f:(fun ~key ~data ->
          add (String.length key);
          visit (depth + 1) data)
      | VVariant (tag, fields) ->
        add (String.length tag);
        List.iter fields ~f:(visit (depth + 1))
      | VRef cell -> visit (depth + 1) !cell
      | VTask task when render_tasks ->
        (match task with
         | TPure value -> visit (depth + 1) value
         | TBind (task, fn) | TMap (task, fn) | TCatch (task, fn) ->
           visit (depth + 1) (VTask task);
           visit (depth + 1) fn
         | TFail message -> add (String.length message)
         | TPerform eff | TSpawn eff ->
           add (String.length eff.op);
           List.iter eff.args ~f:(visit (depth + 1)))
      | VInt _
      | VFloat _
      | VBool _
      | VUnit
      | VClosure _
      | VBuiltin _
      | VModule _
      | VTask _ -> ()
    in
    visit 0 value;
    !bytes, !deepest
  in
  let check_value value = ignore (measure_value value : int * int) in
  let before_json_import json =
    (* Measure the projected ChatML representation without allocating it. In
       particular an object adds an array, entry records and key strings. *)
    let bytes = ref 0 in
    let add count =
      if count > limits.max_value_bytes - !bytes then value_failure ();
      bytes := !bytes + count
    in
    let node depth =
      checkpoint ();
      if depth > limits.max_depth then value_failure ();
      add 16
    in
    let sequence values f =
      let count = ref 0 in
      List.iter values ~f:(fun value ->
        Int.incr count;
        if !count > limits.max_array_items then value_failure ();
        f value)
    in
    let rec walk depth = function
      | `Null ->
        node depth;
        add 4
      | `True | `False ->
        node depth;
        add 4;
        node (depth + 1)
      | `Number text ->
        node depth;
        add 6;
        node (depth + 1);
        (* Parsing a numeric lexeme can cost more than the resulting float. *)
        add (String.length text)
      | `String text ->
        node depth;
        add 6;
        node (depth + 1);
        add (String.length text)
      | `Array values ->
        node depth;
        add 5;
        node (depth + 1);
        sequence values (walk (depth + 2))
      | `Object fields ->
        node depth;
        add 6;
        node (depth + 1);
        sequence fields (fun (key, value) ->
          node (depth + 2);
          add 8;
          node (depth + 3);
          add (String.length key);
          walk (depth + 3) value)
    in
    walk 0 json;
    allocate !bytes
  in
  let before_json_export value =
    let size = fst (measure_value value) in
    (* Reserve conversion/serialization work, including JSON string escaping. *)
    if size > Atomic.get allocation_remaining / 6 then exhaust budget allocation_failure;
    allocate (6 * size)
  in
  let check_json_text text =
    (* Jsonaf has no execution-control callback. Check lexical nesting before
       entering it, ignoring delimiters in strings and escaped quotes. This is
       a resource preflight, not a second JSON validator. Jsonaf still diagnoses
       syntax errors. Poll in chunks so scanning a scalar also cooperates. *)
    let depth = ref 0 in
    let in_string = ref false in
    let escaped = ref false in
    String.iteri text ~f:(fun index character ->
      if index mod 256 = 0 then checkpoint ();
      match !in_string, !escaped, character with
      | true, true, _ -> escaped := false
      | true, false, '\\' -> escaped := true
      | true, false, '"' -> in_string := false
      | true, false, _ -> ()
      | false, _, '"' -> in_string := true
      | false, _, ('[' | '{') ->
        Int.incr depth;
        if !depth > limits.max_depth then value_failure ()
      | false, _, (']' | '}') -> depth := Int.max 0 (!depth - 1)
      | false, _, _ -> ())
  in
  let before_builtin ~name (args : Chatml.Chatml_lang.value list) =
    checkpoint ();
    List.iter args ~f:check_value;
    match name, args with
    | ("Array.make" | "Array.init" | "Array.literal"), VInt count :: _ -> array_size count
    | "Array.append", [ VArray a; VArray b ] ->
      array_size (Array.length a + Array.length b)
    | ( ( "Array.copy"
        | "Array.reverse"
        | "Array.map"
        | "Array.mapi"
        | "Array.filter"
        | "array_copy" )
      , VArray values :: _ ) -> array_size (Array.length values)
    | "record_keys", [ VRecord fields ] -> array_size (Map.length fields)
    | ( ("Json.object_keys" | "Json.remove_field")
      , VVariant ("Object", [ VArray entries ]) :: _ ) ->
      array_size (Array.length entries)
    | "Json.set_field", [ VVariant ("Object", [ VArray entries ]); VString key; _ ] ->
      let exists =
        Array.exists entries ~f:(function
          | VRecord fields ->
            (match Map.find fields "key" with
             | Some (VString candidate) -> String.equal candidate key
             | _ -> false)
          | _ -> false)
      in
      array_size (Array.length entries + if exists then 0 else 1)
    | "Json.get_path", [ _; VArray path ] -> array_size (Array.length path)
    | "Array.sub", [ _; _; VInt count ] -> array_size count
    | "String.concat", [ VString a; VString b ] ->
      string_size (String.length a + String.length b)
    | "String.slice", [ _; _; VInt count ] -> string_size count
    | ("String.trim" | "String.to_upper" | "String.to_lower"), [ VString text ] ->
      string_size (String.length text)
    | "String.replace_all", [ VString text; VString pattern; VString replacement ]
      when not (String.is_empty pattern) ->
      let size = ref (String.length text) in
      let rec count pos =
        checkpoint ();
        match String.substr_index text ~pos ~pattern with
        | None -> ()
        | Some index ->
          let retained = !size - String.length pattern in
          if String.length replacement > limits.max_value_bytes - retained
          then value_failure ();
          size := retained + String.length replacement;
          count (index + String.length pattern)
      in
      count 0;
      string_size !size
    | "String.split", [ VString text; VString separator ]
      when not (String.is_empty separator) ->
      let rec count pos total =
        checkpoint ();
        if total > limits.max_array_items then value_failure ();
        match String.substr_index text ~pos ~pattern:separator with
        | None -> total
        | Some index -> count (index + String.length separator) (total + 1)
      in
      array_size (count 0 1);
      string_size (String.length text)
    | ("Json.parse" | "Json.parse_opt" | "Json.validate"), [ VString text ] ->
      (* Bound parser expansion conservatively before entering Jsonaf. *)
      check_json_text text;
      if String.length text > Atomic.get allocation_remaining / 32
      then exhaust budget allocation_failure;
      allocate (32 * String.length text)
    | ("Json.stringify" | "Json.pretty" | "to_string" | "print"), [ value ] ->
      (* Include escaping and formatting expansion before allocating text. *)
      let size, depth = measure_value ~render_tasks:true value in
      let expansion =
        match name with
        | "Json.pretty" ->
          (* Pretty JSON can indent each node to its nesting depth. Check the
             multiplication before it can overflow under a large host policy. *)
          if depth > (Int.max_value - 6) / 2 then value_failure ();
          6 + (2 * depth)
        | _ -> 6
      in
      if size > limits.max_value_bytes / expansion then value_failure ();
      string_size (expansion * size)
    | "Hashtbl.set", VRef cell :: _ ->
      (match !cell with
       | VArray values -> array_size (Array.length values + 1)
       | _ -> ())
    | "Hashtbl.remove", VRef cell :: _ ->
      (match !cell with
       | VArray values -> array_size (Array.length values)
       | _ -> ())
    | _ -> allocate 16
  in
  let calls = Atomic.make limits.max_calls in
  let tasks = Atomic.make limits.max_tasks in
  let before_effect ~name ~spawned =
    let starts_job =
      match name with
      | "Job.start_tool" | "Job.start_script" -> true
      | _ -> false
    in
    if spawned || starts_job
    then
      consume
        budget
        tasks
        1
        { code = "chatml.task_limit"; message = "ChatML spawned-task budget exhausted" };
    match name with
    | "Tool.call" | "Tool.spawn" | "Job.start_tool" | "Job.start_script" ->
      consume
        budget
        calls
        1
        { code = "chatml.call_limit"; message = "ChatML tool-call budget exhausted" }
    | _ -> ()
  in
  { Chatml.Chatml_lang.checkpoint
  ; allocate
  ; before_builtin
  ; check_value
  ; before_json_import
  ; before_json_export
  ; before_effect
  ; after_effect = (fun value -> allocate (fst (measure_value value)))
  }
;;

let with_scope
      ?(invocation = true)
      ?(policy = Bounded default_limits)
      ?(context = [])
      ~env
      f
  =
  let execute () =
    let parents = capture_context ~inherited:context () in
    let ancestors =
      List.concat_map parents ~f:(fun parent -> parent :: parent.ancestors)
      |> List.fold ~init:[] ~f:(fun seen frame ->
        if List.mem seen frame ~equal:phys_equal then seen else frame :: seen)
    in
    let inherited =
      List.concat_map parents ~f:(fun parent -> parent.budgets)
      |> List.fold ~init:[] ~f:(fun budgets (remaining, budget, control) ->
        match List.exists budgets ~f:(fun (_, other, _) -> phys_equal budget other) with
        | false -> budgets @ [ remaining, budget, control ]
        | true ->
          List.map budgets ~f:(fun (current, other, control) ->
            ( (if phys_equal budget other then Int64.min current remaining else current)
            , other
            , control )))
    in
    let check_ancestors () =
      List.iter ancestors ~f:(fun frame ->
        if not (Atomic.get frame.active)
        then
          raise
            (Budget_exhausted
               { code = "chatml.inactive_scope"
               ; message = "The owning execution scope has ended."
               }));
      List.iter inherited ~f:(fun (_, budget, _) -> check_budget budget)
    in
    check_ancestors ();
    let inherited =
      match invocation with
      | false -> inherited
      | true ->
        List.map inherited ~f:(fun (remaining, budget, control) ->
          if Int64.(remaining <= 1L)
          then
            exhaust
              budget
              { code = "chatml.invocation_depth"
              ; message = "ChatML invocation depth exhausted"
              };
          Int64.pred remaining, budget, control)
    in
    let active = Atomic.make true in
    let budgets =
      match policy with
      | Unrestricted -> inherited
      | Bounded limits ->
        let budget = { limits; active; failure = Atomic.make None } in
        let depth = Int64.of_int limits.max_invocation_depth in
        (* Host-only scopes have no current ChatML frame. The extra slot is
           consumed by the first actual invocation. Int64 can represent the
           successor of every positive OCaml int without overflow. *)
        let depth =
          match invocation with
          | true -> depth
          | false -> Int64.succ depth
        in
        inherited @ [ depth, budget, make_control ~env budget ]
    in
    let frame = { active; ancestors; budgets } in
    let each f =
      if not (Atomic.get active)
      then
        raise
          (Budget_exhausted
             { code = "chatml.inactive_scope"
             ; message = "The execution scope has ended."
             });
      check_ancestors ();
      List.iter budgets ~f:(fun (_, budget, control) ->
        check_budget budget;
        f control)
    in
    let control =
      match budgets with
      | [] -> None
      | _ ->
        Some
          Chatml.Chatml_lang.
            { checkpoint = (fun () -> each (fun c -> c.checkpoint ()))
            ; allocate = (fun bytes -> each (fun c -> c.allocate bytes))
            ; check_value = (fun value -> each (fun c -> c.check_value value))
            ; before_json_import =
                (fun value -> each (fun c -> c.before_json_import value))
            ; before_json_export =
                (fun value -> each (fun c -> c.before_json_export value))
            ; before_builtin =
                (fun ~name args -> each (fun c -> c.before_builtin ~name args))
            ; before_effect =
                (fun ~name ~spawned -> each (fun c -> c.before_effect ~name ~spawned))
            ; after_effect = (fun value -> each (fun c -> c.after_effect value))
            }
    in
    let run_program () = f control in
    Exn.protect
      ~finally:(fun () -> Atomic.set active false)
      ~f:(fun () ->
        Eio.Fiber.with_binding frame_key frame (fun () ->
          match policy with
          | Unrestricted -> run_program ()
          | Bounded limits ->
            Eio.Time.Timeout.run_exn
              (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) limits.wall_seconds)
              run_program))
  in
  match policy with
  | Bounded limits when not (valid_limits limits) ->
    error "chatml.invalid_limits" "unsupported execution resource limits"
  | Unrestricted | Bounded _ ->
    (try Ok (execute ()) with
     | Budget_exhausted failure -> Error failure
     | Eio.Time.Timeout ->
       error "chatml.execution_timeout" "ChatML execution deadline exceeded"
     | Eio.Cancel.Cancelled _ as exn -> raise exn
     | Stack_overflow ->
       error "chatml.execution_limit" "ChatML execution stack limit exceeded")
;;

type runner =
  { control : Chatml.Chatml_lang.execution_control
  ; run : 'a. ?context:context -> (unit -> 'a) -> ('a, error) result
  }

let create_runner ~env ~policy () =
  let key = Eio.Fiber.create_key () in
  let inactive () =
    raise
      (Budget_exhausted
         { code = "chatml.inactive_scope"
         ; message = "The persistent runtime has no active execution scope."
         })
  in
  let apply (f : Chatml.Chatml_lang.execution_control -> unit) =
    match Eio.Fiber.get key with
    | Some (active, control) when Atomic.get active -> Option.iter control ~f
    | Some _ | None -> inactive ()
  in
  let control : Chatml.Chatml_lang.execution_control =
    { checkpoint = (fun () -> apply (fun c -> c.checkpoint ()))
    ; allocate = (fun bytes -> apply (fun c -> c.allocate bytes))
    ; check_value = (fun value -> apply (fun c -> c.check_value value))
    ; before_json_import = (fun value -> apply (fun c -> c.before_json_import value))
    ; before_json_export = (fun value -> apply (fun c -> c.before_json_export value))
    ; before_builtin = (fun ~name args -> apply (fun c -> c.before_builtin ~name args))
    ; before_effect =
        (fun ~name ~spawned -> apply (fun c -> c.before_effect ~name ~spawned))
    ; after_effect = (fun value -> apply (fun c -> c.after_effect value))
    }
  in
  { control
  ; run =
      (fun ?context f ->
        with_scope ?context ~env ~policy (fun control ->
          let active = Atomic.make true in
          Exn.protect
            ~finally:(fun () -> Atomic.set active false)
            ~f:(fun () -> Eio.Fiber.with_binding key (active, control) f)))
  }
;;

let runner_control runner = runner.control
let run_scoped ?context runner f = runner.run ?context f
let with_control ?policy ?context ~env f = with_scope ?policy ?context ~env f

let with_host_budget ?context ~policy ~env f =
  with_scope ~invocation:false ?context ~policy ~env f
;;

let run_in_scope
      ?prepare_result
      ~(control : Chatml.Chatml_lang.execution_control option)
      ~config
      ~program
      ~entrypoint
      ~arguments
      ()
  =
  Option.iter control ~f:(fun c -> List.iter arguments ~f:c.check_value);
  let prepare_result =
    Option.map prepare_result ~f:(fun prepare ~value ~local_effects ->
      Option.iter control ~f:(fun c -> c.check_value value);
      prepare ~value ~local_effects)
  in
  Chatml_host_runtime.run_entrypoint
    ?control
    ?prepare_result
    config
    program
    ~entrypoint
    ~arguments
    ()
  |> Result.map ~f:(fun value ->
    (match prepare_result with
     | None -> Option.iter control ~f:(fun c -> c.check_value value)
     | Some _ -> ());
    value)
  |> Result.map_error ~f:(fun message ->
    { code = "chatml.execution_failed"; message = String.prefix message (16 * 1024) })
;;

let run ?policy ?context ~env ~config ~program ~entrypoint ~arguments () =
  with_control ?policy ?context ~env (fun control ->
    run_in_scope ~control ~config ~program ~entrypoint ~arguments ())
  |> Result.join
;;
