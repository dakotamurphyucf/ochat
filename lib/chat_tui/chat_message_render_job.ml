open Core

module Priority = struct
  type t =
    | Visible
    | Prefetch
    | Background
end

module Layout_plan = struct
  type t =
    { min_width : int
    ; max_width : int option
    }

  let unconstrained = { min_width = 0; max_width = None }
  let unknown = { min_width = 1; max_width = Some 0 }

  let intersect left right =
    { min_width = Int.max left.min_width right.min_width
    ; max_width =
        (match left.max_width, right.max_width with
         | None, None -> None
         | Some width, None | None, Some width -> Some width
         | Some left, Some right -> Some (Int.min left right))
    }
  ;;

  let allows t ~width =
    match t.max_width with
    | Some max_width when max_width < t.min_width -> false
    | None | Some _ ->
      width >= t.min_width
      && Option.value_map t.max_width ~default:true ~f:(fun max_width ->
        width <= max_width)
  ;;
end

module Key = struct
  type t =
    { transcript_generation : int
    ; row_id : Projected_message.Id.t
    ; row_revision : int
    ; message_index : int
    ; message_revision : int
    ; width : int
    ; role : string
    ; text : string
    ; tool_output : Types.tool_output_kind option
    ; tool_call_outcome : Ochat_function.Trace.outcome option
    ; theme_generation : int
    ; grammar_generation : int
    }

  let equal_tool_output a b =
    match a, b with
    | None, None -> true
    | Some Types.Apply_patch, Some Types.Apply_patch -> true
    | Some (Types.Read_file a), Some (Types.Read_file b) ->
      Option.equal String.equal a.path b.path
    | Some (Types.Read_directory a), Some (Types.Read_directory b) ->
      Option.equal String.equal a.path b.path
    | Some (Types.Other a), Some (Types.Other b) ->
      Option.equal String.equal a.name b.name
    | None, Some _ | Some _, None | Some _, Some _ -> false
  ;;

  let equal_outcome a b =
    match a, b with
    | None, None -> true
    | Some Ochat_function.Trace.Returned, Some Ochat_function.Trace.Returned
    | Some Ochat_function.Trace.Raised, Some Ochat_function.Trace.Raised
    | Some Ochat_function.Trace.Cancelled, Some Ochat_function.Trace.Cancelled -> true
    | None, Some _ | Some _, None | Some _, Some _ -> false
  ;;

  let equal a b =
    Projected_message.Id.equal a.row_id b.row_id
    && Int.equal a.row_revision b.row_revision
    && Int.equal a.width b.width
    && String.equal a.role b.role
    && String.equal a.text b.text
    && equal_tool_output a.tool_output b.tool_output
    && equal_outcome a.tool_call_outcome b.tool_call_outcome
    && Int.equal a.theme_generation b.theme_generation
    && Int.equal a.grammar_generation b.grammar_generation
  ;;
end

module Layout = struct
  type line = (Notty.A.t * string) list

  type t =
    { width : int
    ; lines : line list
    }
end

module Code_cache = struct
  type role_class =
    | Toollike
    | Userlike

  type entry =
    { mutable last_used : int
    ; lines : Layout.line list
    }

  type t =
    { capacity : int
    ; table : (string, entry) Hashtbl.t
    ; mutable tick : int
    }

  let create ~capacity =
    if capacity < 0 then invalid_arg "Code_cache.create: negative capacity";
    { capacity; table = Hashtbl.create (module String); tick = 0 }
  ;;

  let clear t =
    Hashtbl.clear t.table;
    t.tick <- 0
  ;;

  let length t = Hashtbl.length t.table

  let role_tag = function
    | Toollike -> "T"
    | Userlike -> "U"
  ;;

  let bucket_size = 8

  let width_bucket width =
    if width <= 0 then 0 else (width + bucket_size - 1) / bucket_size * bucket_size
  ;;

  let key ~role_class ~grammar_generation ~lang ~code ~width =
    let lang = Option.value lang ~default:"-" in
    let digest = Md5.(to_hex (digest_string (lang ^ "\x00" ^ code))) in
    String.concat
      ~sep:"|"
      [ Int.to_string grammar_generation
      ; role_tag role_class
      ; lang
      ; digest
      ; Int.to_string width
      ]
  ;;

  let find t ~role_class ~grammar_generation ~lang ~code ~width =
    t.tick <- t.tick + 1;
    let key = key ~role_class ~grammar_generation ~lang ~code ~width in
    match Hashtbl.find t.table key with
    | None -> None
    | Some entry ->
      entry.last_used <- t.tick;
      Some entry.lines
  ;;

  let evict_if_needed t =
    if Hashtbl.length t.table > t.capacity
    then
      Hashtbl.fold t.table ~init:None ~f:(fun ~key ~data oldest ->
        match oldest with
        | None -> Some (key, data.last_used)
        | Some (_, tick) when data.last_used < tick -> Some (key, data.last_used)
        | Some _ -> oldest)
      |> Option.iter ~f:(fun (key, _) -> Hashtbl.remove t.table key)
  ;;

  let set t ~role_class ~grammar_generation ~lang ~code ~width lines =
    t.tick <- t.tick + 1;
    let key = key ~role_class ~grammar_generation ~lang ~code ~width in
    Hashtbl.set t.table ~key ~data:{ last_used = t.tick; lines };
    evict_if_needed t
  ;;
end

module Highlight_cache = struct
  type binding =
    | Plain of
        { theme_generation : int
        ; grammar_generation : int
        ; lang : string option
        ; text : string
        ; lines : Highlight_tm_engine.span list list
        }
    | Scoped of
        { theme_generation : int
        ; grammar_generation : int
        ; lang : string option
        ; text : string
        ; result : Highlight_tm_engine.scoped_span list list * Highlight_tm_engine.info
        }

  type entry =
    { mutable last_used : int
    ; binding : binding
    }

  type t =
    { capacity : int
    ; table : (string, entry) Hashtbl.t
    ; mutable tick : int
    ; mutable captured : binding list option
    }

  let create ~capacity =
    if capacity < 0 then invalid_arg "Highlight_cache.create: negative capacity";
    { capacity; table = Hashtbl.create (module String); tick = 0; captured = None }
  ;;

  let clear t =
    Hashtbl.clear t.table;
    t.tick <- 0;
    t.captured <- None
  ;;

  let length t = Hashtbl.length t.table

  let key ~mode ~theme_generation ~grammar_generation ~lang ~text =
    let lang = Option.value lang ~default:"-" in
    let digest = Md5.(to_hex (digest_string (lang ^ "\x00" ^ text))) in
    String.concat
      ~sep:"|"
      [ mode
      ; Int.to_string theme_generation
      ; Int.to_string grammar_generation
      ; lang
      ; digest
      ]
  ;;

  let find t key =
    t.tick <- t.tick + 1;
    match Hashtbl.find t.table key with
    | None -> None
    | Some entry ->
      entry.last_used <- t.tick;
      Option.iter t.captured ~f:(fun captured ->
        t.captured <- Some (entry.binding :: captured));
      Some entry.binding
  ;;

  let evict_if_needed t =
    if Hashtbl.length t.table > t.capacity
    then
      Hashtbl.fold t.table ~init:None ~f:(fun ~key ~data oldest ->
        match oldest with
        | None -> Some (key, data.last_used)
        | Some (_, tick) when data.last_used < tick -> Some (key, data.last_used)
        | Some _ -> oldest)
      |> Option.iter ~f:(fun (key, _) -> Hashtbl.remove t.table key)
  ;;

  let set t key binding =
    t.tick <- t.tick + 1;
    Hashtbl.set t.table ~key ~data:{ last_used = t.tick; binding };
    Option.iter t.captured ~f:(fun captured -> t.captured <- Some (binding :: captured));
    evict_if_needed t
  ;;

  let plain_key = key ~mode:"plain"
  let scoped_key = key ~mode:"scoped"

  let find_plain t ~theme_generation ~grammar_generation ~lang ~text =
    let key = plain_key ~theme_generation ~grammar_generation ~lang ~text in
    match find t key with
    | Some (Plain { lines; _ }) -> Some lines
    | None | Some (Scoped _) -> None
  ;;

  let set_plain t ~theme_generation ~grammar_generation ~lang ~text lines =
    let key = plain_key ~theme_generation ~grammar_generation ~lang ~text in
    set t key (Plain { theme_generation; grammar_generation; lang; text; lines })
  ;;

  let find_scoped t ~theme_generation ~grammar_generation ~lang ~text =
    let key = scoped_key ~theme_generation ~grammar_generation ~lang ~text in
    match find t key with
    | Some (Scoped { result; _ }) -> Some result
    | None | Some (Plain _) -> None
  ;;

  let set_scoped t ~theme_generation ~grammar_generation ~lang ~text result =
    let key = scoped_key ~theme_generation ~grammar_generation ~lang ~text in
    set t key (Scoped { theme_generation; grammar_generation; lang; text; result })
  ;;

  let install t = function
    | Plain { theme_generation; grammar_generation; lang; text; lines } ->
      set_plain t ~theme_generation ~grammar_generation ~lang ~text lines
    | Scoped { theme_generation; grammar_generation; lang; text; result } ->
      set_scoped t ~theme_generation ~grammar_generation ~lang ~text result
  ;;

  let capture t f =
    let previous = t.captured in
    t.captured <- Some [];
    match f () with
    | value ->
      let bindings = Option.value t.captured ~default:[] |> List.rev in
      t.captured <- previous;
      value, bindings
    | exception exn ->
      t.captured <- previous;
      raise exn
  ;;
end

module Prepared_message = struct
  type paragraph =
    { text : string
    ; fallback_spans : (Notty.A.t * string) list
    ; tool_call_parts : (string * string * string * string) option
    }

  type block =
    | Text of paragraph list
    | Code of
        { lang : string option
        ; code : string
        }

  type body =
    | Default of block list
    | Apply_patch of
        { status : paragraph list
        ; patch : string option
        }
    | Read_file of
        { lang : string
        ; code : string
        }

  type t =
    { role : string
    ; text : string
    ; body : body
    }
end

type semantic_seed =
  { prepared : Prepared_message.t
  ; highlights : Highlight_cache.binding list
  }

type t =
  { key : Key.t
  ; geometry_generation : int
  ; request_generation : int
  ; render_generation : int
  ; submission_generation : int
  ; semantic_seed : semantic_seed option
  ; priority : Priority.t
  }

let create
      ~transcript_generation
      ~row_id
      ~row_revision
      ~message_index
      ~message_revision
      ~width
      ~role
      ~text
      ~tool_output
      ~tool_call_outcome
      ~theme_generation
      ~grammar_generation
      ~geometry_generation
      ~request_generation
      ~render_generation
      ~submission_generation
      ~semantic_seed
      ~priority
  =
  let key =
    Key.
      { transcript_generation
      ; row_id
      ; row_revision
      ; message_index
      ; message_revision
      ; width
      ; role
      ; text
      ; tool_output
      ; tool_call_outcome
      ; theme_generation
      ; grammar_generation
      }
  in
  { key
  ; geometry_generation
  ; request_generation
  ; render_generation
  ; submission_generation
  ; semantic_seed
  ; priority
  }
;;

module Prepared_cache = struct
  type entry =
    { row_revision : int
    ; role : string
    ; text : string
    ; tool_output : Types.tool_output_kind option
    ; prepared : Prepared_message.t
    }

  type t =
    { capacity : int
    ; table : (Projected_message.Id.t, entry) Hashtbl.t
    ; order : Projected_message.Id.t Queue.t
    }

  let create ~capacity =
    if capacity < 0 then invalid_arg "Prepared_cache.create: negative capacity";
    { capacity
    ; table = Hashtbl.create (module Projected_message.Id)
    ; order = Queue.create ()
    }
  ;;

  let clear t =
    Hashtbl.clear t.table;
    Queue.clear t.order
  ;;

  let length t = Hashtbl.length t.table

  let find t ~row_id ~row_revision ~role ~text ~tool_output =
    Hashtbl.find t.table row_id
    |> Option.filter ~f:(fun entry ->
      Int.equal entry.row_revision row_revision
      && String.equal entry.role role
      && String.equal entry.text text
      && Key.equal_tool_output entry.tool_output tool_output)
    |> Option.map ~f:(fun entry -> entry.prepared)
  ;;

  let evict_if_needed t =
    while Hashtbl.length t.table > t.capacity do
      match Queue.dequeue t.order with
      | None -> Hashtbl.clear t.table
      | Some row_id -> Hashtbl.remove t.table row_id
    done
  ;;

  let set t ~row_id ~row_revision ~role ~text ~tool_output prepared =
    Hashtbl.set
      t.table
      ~key:row_id
      ~data:{ row_revision; role; text; tool_output; prepared };
    Queue.enqueue t.order row_id;
    evict_if_needed t
  ;;
end

module Wrapped_cache = struct
  type line = (Notty.A.t * string) list

  type t =
    { capacity : int
    ; table : (string, line list) Hashtbl.t
    ; mutable hits : int
    ; mutable misses : int
    }

  let create ~capacity =
    if capacity < 0 then invalid_arg "Wrapped_cache.create: negative capacity";
    { capacity; table = Hashtbl.create (module String); hits = 0; misses = 0 }
  ;;

  let clear t =
    Hashtbl.clear t.table;
    t.hits <- 0;
    t.misses <- 0
  ;;

  let find t key =
    match Hashtbl.find t.table key with
    | Some lines ->
      t.hits <- t.hits + 1;
      Some lines
    | None ->
      t.misses <- t.misses + 1;
      None
  ;;

  let set t key lines =
    if Hashtbl.length t.table >= t.capacity then Hashtbl.clear t.table;
    Hashtbl.set t.table ~key ~data:lines
  ;;

  let stats t = t.hits, t.misses
end

module Runtime = struct
  type t =
    { hi_engine : Highlight_tm_engine.t
    ; theme_generation : int
    ; grammar_generation : int
    ; is_cancelled : unit -> bool
    ; code_cache : Code_cache.t option
    ; highlight_cache : Highlight_cache.t option
    ; prepared_cache : Prepared_cache.t option
    ; wrapped_cache : Wrapped_cache.t option
    }

  let create
        ~hi_engine
        ~theme_generation
        ~grammar_generation
        ?(is_cancelled = fun () -> false)
        ?code_cache
        ?highlight_cache
        ?prepared_cache
        ?wrapped_cache
        ()
    =
    { hi_engine
    ; theme_generation
    ; grammar_generation
    ; is_cancelled
    ; code_cache
    ; highlight_cache
    ; prepared_cache
    ; wrapped_cache
    }
  ;;

  let hi_engine t = t.hi_engine
  let theme_generation t = t.theme_generation
  let grammar_generation t = t.grammar_generation
  let is_cancelled t = t.is_cancelled
  let code_cache t = t.code_cache
  let highlight_cache t = t.highlight_cache
  let prepared_cache t = t.prepared_cache
  let wrapped_cache t = t.wrapped_cache
end

type result =
  { key : Key.t
  ; geometry_generation : int
  ; request_generation : int
  ; render_generation : int
  ; submission_generation : int
  ; prepared : Prepared_message.t option
  ; image : Notty.I.t
  ; height : int
  ; layout : Layout.t
  ; highlights : Highlight_cache.binding list
  ; layout_plan : Layout_plan.t
  }

let result
      ?prepared
      ?(highlights = [])
      ?(layout_plan = Layout_plan.unconstrained)
      ?layout
      (job : t)
      ~image
  =
  let layout =
    Option.value layout ~default:Layout.{ width = job.key.width; lines = [] }
  in
  { key = job.key
  ; geometry_generation = job.geometry_generation
  ; request_generation = job.request_generation
  ; render_generation = job.render_generation
  ; submission_generation = job.submission_generation
  ; prepared
  ; image
  ; height = Notty.I.height image
  ; layout
  ; highlights
  ; layout_plan
  }
;;

let result_matches (result : result) (job : t) =
  Int.equal result.geometry_generation job.geometry_generation
  && Int.equal result.request_generation job.request_generation
  && Int.equal result.render_generation job.render_generation
  && Int.equal result.submission_generation job.submission_generation
  && Key.equal result.key job.key
;;
