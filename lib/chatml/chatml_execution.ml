open Core

type limits =
  { fuel : int
  ; max_tasks : int
  ; wall_seconds : float
  ; max_value_bytes : int
  ; max_array_items : int
  ; max_depth : int
  ; allocation_bytes : int
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
  }
;;

exception Fuel_exhausted
exception Value_exhausted
exception Allocation_exhausted

let error code message = Error { code; message }

let run_bounded ~limits ~env ~config ~program ~entrypoint ~arguments () =
  if
    limits.fuel <= 0
    || limits.max_tasks < 0
    || (not (Float.is_finite limits.wall_seconds))
    || Float.(limits.wall_seconds <= 0.)
    || limits.max_value_bytes <= 0
    || limits.max_array_items <= 0
    || limits.max_depth <= 0
    || limits.allocation_bytes <= 0
  then error "chatml.invalid_limits" "unsupported execution resource limits"
  else (
    let remaining = ref limits.fuel in
    let until_poll = ref 0 in
    let clock = Eio.Stdenv.mono_clock env in
    let started = Eio.Time.Mono.now clock in
    let checkpoint () =
      match !remaining with
      | 0 -> raise Fuel_exhausted
      | _ ->
        decr remaining;
        decr until_poll;
        if !until_poll <= 0
        then (
          until_poll := 64;
          Eio.Fiber.yield ();
          let elapsed = Mtime.span started (Eio.Time.Mono.now clock) in
          if Float.(Mtime.Span.to_float_ns elapsed >= limits.wall_seconds *. 1e9)
          then raise Eio.Time.Timeout)
    in
    let allocated = ref 0 in
    let allocate bytes =
      if bytes < 0 || bytes > limits.allocation_bytes - !allocated
      then raise Allocation_exhausted;
      allocated := !allocated + bytes
    in
    let array_size count =
      if count > limits.max_array_items then raise Value_exhausted;
      if count > (limits.allocation_bytes - !allocated) / 16
      then raise Allocation_exhausted;
      if count >= 0 then allocate (16 * count)
    in
    let string_size count =
      if count > limits.max_value_bytes then raise Value_exhausted;
      if count >= 0 then allocate count
    in
    let measure_value value =
      let bytes = ref 0 in
      let add count =
        if count > limits.max_value_bytes - !bytes then raise Value_exhausted;
        bytes := !bytes + count
      in
      let rec visit depth (value : Chatml.Chatml_lang.value) =
        checkpoint ();
        if depth > limits.max_depth then raise Value_exhausted;
        add 16;
        match value with
        | VString text -> add (String.length text)
        | VArray values ->
          if Array.length values > limits.max_array_items then raise Value_exhausted;
          Array.iter values ~f:(visit (depth + 1))
        | VRecord fields ->
          if Map.length fields > limits.max_array_items then raise Value_exhausted;
          Map.iteri fields ~f:(fun ~key ~data ->
            add (String.length key);
            visit (depth + 1) data)
        | VVariant (tag, fields) ->
          add (String.length tag);
          List.iter fields ~f:(visit (depth + 1))
        | VRef cell -> visit (depth + 1) !cell
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
      !bytes
    in
    let check_value value = ignore (measure_value value : int) in
    let before_builtin ~name (args : Chatml.Chatml_lang.value list) =
      checkpoint ();
      List.iter args ~f:check_value;
      match name, args with
      | ("Array.make" | "Array.init" | "Array.literal"), VInt count :: _ ->
        array_size count
      | "Array.append", [ VArray a; VArray b ] ->
        array_size (Array.length a + Array.length b)
      | ( ("Array.copy" | "Array.reverse" | "Array.map" | "Array.mapi" | "array_copy")
        , VArray values :: _ ) -> array_size (Array.length values)
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
            then raise Value_exhausted;
            size := retained + String.length replacement;
            count (index + String.length pattern)
        in
        count 0;
        string_size !size
      | "String.split", [ VString text; VString separator ]
        when not (String.is_empty separator) ->
        let rec count pos total =
          checkpoint ();
          if total > limits.max_array_items then raise Value_exhausted;
          match String.substr_index text ~pos ~pattern:separator with
          | None -> total
          | Some index -> count (index + String.length separator) (total + 1)
        in
        array_size (count 0 1);
        string_size (String.length text)
      | ("Json.parse" | "Json.parse_opt"), [ VString text ] ->
        (* Bound parser expansion conservatively before entering Jsonaf. *)
        if String.length text > (limits.allocation_bytes - !allocated) / 32
        then raise Allocation_exhausted;
        allocate (32 * String.length text)
      | ("Json.stringify" | "Json.pretty" | "to_string"), [ value ] ->
        (* Include escaping and formatting expansion before allocating text. *)
        let size = measure_value value in
        if size > limits.max_value_bytes / 6 then raise Value_exhausted;
        string_size (6 * size)
      | "Hashtbl.set", VRef cell :: _ ->
        (match !cell with
         | VArray values -> array_size (Array.length values + 1)
         | _ -> ())
      | _ -> allocate 16
    in
    let control : Chatml.Chatml_lang.execution_control =
      { checkpoint; allocate; before_builtin; check_value }
    in
    try
      Eio.Time.Timeout.run_exn
        (Eio.Time.Timeout.seconds clock limits.wall_seconds)
        (fun () ->
           List.iter arguments ~f:check_value;
           Chatml_host_runtime.run_entrypoint
             ~control
             ~limits:{ fuel = limits.fuel; max_tasks = limits.max_tasks }
             config
             program
             ~entrypoint
             ~arguments
             ()
           |> Result.map ~f:(fun value ->
             check_value value;
             value)
           |> Result.map_error ~f:(fun message ->
             let message = String.prefix message (16 * 1024) in
             { code = "chatml.execution_failed"; message }))
    with
    | Fuel_exhausted -> error "chatml.execution_limit" "ChatML execution fuel exhausted"
    | Value_exhausted ->
      error "chatml.value_limit" "ChatML value size or depth limit exceeded"
    | Allocation_exhausted ->
      error "chatml.allocation_limit" "ChatML allocation budget exhausted"
    | Eio.Time.Timeout ->
      error "chatml.execution_timeout" "ChatML execution deadline exceeded"
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | Stack_overflow ->
      error "chatml.execution_limit" "ChatML execution stack limit exceeded")
;;

let run ?(policy = Bounded default_limits) ~env ~config ~program ~entrypoint ~arguments ()
  =
  match policy with
  | Bounded limits -> run_bounded ~limits ~env ~config ~program ~entrypoint ~arguments ()
  | Unrestricted ->
    Chatml_host_runtime.run_entrypoint config program ~entrypoint ~arguments ()
    |> Result.map_error ~f:(fun message -> { code = "chatml.execution_failed"; message })
;;
