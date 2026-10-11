open Core
module CM = Prompt.Chat_markdown
module Builder = Prompt_revision_builder
module Input = Chatmd_parser_input

module Role = struct
  type t =
    | Root_ui
    | Headless
    | Delegated_ui
  [@@deriving equal]
end

type t =
  { parsed : CM.Parsed.t
  ; role : Role.t
  }

let parsed t = t.parsed
let role t = t.role

module Context = struct
  type t =
    { key : string
    ; parsed : CM.Parsed.t
    ; mutable discovered : bool
    ; mutable roles : Role.t list
    }
end

let bytes_equal left right ~checkpoint =
  let length = String.length left in
  let equal = ref (Int.equal length (String.length right)) in
  let offset = ref 0 in
  while !equal && !offset < length do
    checkpoint ();
    let stop = !offset + Int.min 4096 (length - !offset) in
    while !equal && !offset < stop do
      equal := Char.equal left.[!offset] right.[!offset];
      incr offset
    done
  done;
  checkpoint ();
  !equal
;;

let collect captured ~max_context_roles ~max_reference_visits ~checkpoint =
  let module Abort = struct
    exception Stop of Builder.Diagnostic.t list
  end
  in
  let fail ?source code message =
    raise (Abort.Stop [ Builder.Diagnostic.{ code; message; source } ])
  in
  try
    checkpoint ();
    let contexts = ref [] in
    let count = ref 0 in
    let add key parsed =
      checkpoint ();
      (* Builder's actual capture permits at most256 distinct roots. Validate
         portable key before retaining it; its maximum1024 bytes bounds all
         remaining path operations independently of caller work. *)
      if !count >= 256 || Result.is_error (Source.Portable_path.create key)
      then fail "prompt.script_role_owner" "Invalid captured parser context inventory";
      if
        List.exists !contexts ~f:(fun context ->
          bytes_equal context.Context.key key ~checkpoint
          || phys_equal context.parsed parsed)
      then fail "prompt.script_role_owner" "Duplicate captured parser context";
      let context = Context.{ key; parsed; discovered = false; roles = [] } in
      incr count;
      contexts := context :: !contexts;
      context
    in
    let root = Builder.Capture.root captured in
    let root = add (Input.path (CM.Parsed.root_input root)) root in
    List.iter (Builder.Capture.agents captured) ~f:(fun (key, parsed) ->
      ignore (add key parsed : Context.t));
    let queue = Queue.create () in
    let rows = ref [] in
    let role_count = ref 0 in
    let reference_count = ref 0 in
    let request context role =
      checkpoint ();
      (* Three role constructors bound this per-context list. Physical identity
         is intentional: both descriptors must belong to SAME admitted Parsed. *)
      if not (List.mem context.Context.roles role ~equal:Role.equal)
      then (
        if !role_count >= max_context_roles
        then fail "prompt.script_role_limit" "Requested parser role ceiling exceeded";
        incr role_count;
        context.roles <- role :: context.roles;
        rows := { parsed = context.parsed; role } :: !rows;
        Queue.enqueue queue context)
    in
    request root Root_ui;
    while not (Queue.is_empty queue) do
      checkpoint ();
      let context = Queue.dequeue_exn queue in
      if not context.Context.discovered
      then (
        context.discovered <- true;
        List.iter (CM.Parsed.agent_references context.parsed) ~f:(fun reference ->
          checkpoint ();
          if !reference_count >= max_reference_visits
          then fail "prompt.script_role_limit" "Agent reference ceiling exceeded";
          incr reference_count;
          match CM.Agent_reference.queued_key reference with
          | None -> ()
          | Some key ->
            let source =
              Input.path (Input.Range.input (CM.Agent_reference.range reference))
            in
            let target =
              List.find !contexts ~f:(fun candidate ->
                bytes_equal candidate.Context.key key ~checkpoint)
              |> function
              | Some target -> target
              | None ->
                fail
                  ~source
                  "prompt.script_role_owner"
                  "Agent parser context was not captured"
            in
            let role =
              match CM.Agent_reference.invocation reference with
              | Tool _ | Content _ -> Role.Headless
              | Persistent_tool _ -> Role.Delegated_ui
            in
            request target role))
    done;
    Ok (List.rev !rows)
  with
  | Abort.Stop errors -> Error errors
;;
