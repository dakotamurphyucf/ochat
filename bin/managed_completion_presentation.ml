open! Core
module P = Agent_protocol
module T = Transcript
module R = History_chatmd.Rendering
module Payload = History_entry.Payload

type t =
  { domain : int
  ; limits : R.Limits.t
  ; mutable previous : string option
  }

let create limits = { domain = (Stdlib.Domain.self () :> int); limits; previous = None }

let check t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "managed completion presentation belongs to its creating domain"
;;

let line output label value =
  R.text output label;
  R.quoted output value;
  R.text output "\n"
;;

let scope_key output (key : T.Scope.Key.t) =
  R.quoted output (T.Source_id.to_string key.source);
  R.text output "/";
  R.quoted output (T.Attempt_id.to_string key.attempt)
;;

let scope output (scope : T.Scope.t) =
  R.text output "scope ";
  scope_key output scope.key;
  match scope.relation with
  | Root -> R.text output " (root)\n"
  | Nested parent ->
    R.text output " (nested; parent ";
    scope_key output parent.scope;
    Option.iter parent.call_alias ~f:(fun alias ->
      R.text output "; call ";
      R.quoted output alias);
    Option.iter parent.call_entry_id ~f:(fun id ->
      R.text output "; entry ";
      R.quoted_history_id output id);
    R.text output ")\n"
;;

let header output = function
  | None -> R.text output "[item kind unavailable]\n"
  | Some (T.Header.Message role) ->
    let role =
      match role with
      | Payload.Role.System -> "system"
      | Developer -> "developer"
      | User -> "user"
      | Assistant -> "assistant"
      | Tool -> "tool"
    in
    line output "message " role
  | Some (Call Function) -> R.text output "function call\n"
  | Some (Call Custom) -> R.text output "custom call\n"
  | Some (Result Function) -> R.text output "function result\n"
  | Some (Result Custom) -> R.text output "custom result\n"
  | Some Reasoning -> R.text output "reasoning\n"
  | Some (Unknown kind) -> line output "unknown item " kind
;;

let content output = function
  | Payload.Content.Text { text; _ } -> line output "text " text
  | Refusal text -> line output "refusal " text
  | Image { uri; _ } -> line output "image reference " uri
  | Unknown { kind; _ } -> line output "[unsupported content] " kind
;;

let each output values ~f =
  List.iter values ~f:(fun value ->
    R.charge output ~steps:1;
    f value)
;;

let tool_output output = function
  | Payload.Output.Text text -> line output "output " text
  | Content parts -> each output parts ~f:(content output)
;;

let finalized output entry =
  R.text output "[finalized live observation]\n";
  R.quoted_history_id output (History_entry.id entry);
  R.text output "\n";
  match Payload.Semantic.view (Payload.semantic (History_entry.payload entry)) with
  | Message { role; content = parts; _ } ->
    header output (Some (Message role));
    each output parts ~f:(content output)
  | Call { kind; name; input_bytes; _ } ->
    header output (Some (Call kind));
    line output "name " name;
    line output "input " input_bytes
  | Result { kind; output = value; _ } ->
    header output (Some (Result kind));
    tool_output output value
  | Reasoning { readable_summary } ->
    each output readable_summary ~f:(line output "reasoning summary ")
  | Unknown { provider_kind } -> line output "[unsupported finalized item] " provider_kind
;;

let observed_text output label (text : T.Draft.text) =
  (match text.completeness with
   | Prefix_observed -> ()
   | Missing_prefix -> R.text output "[earlier text unavailable]\n");
  line output label text.value
;;

let draft output (item : T.Draft.item_view) =
  scope output item.descriptor.scope;
  line output "item " (T.Item_id.to_string item.descriptor.id);
  match item.state with
  | Finalized entry -> finalized output entry
  | Partial partial ->
    header output item.descriptor.header;
    (match partial.completeness with
     | Prefix_observed -> ()
     | Missing_prefix -> R.text output "[earlier item content unavailable]\n");
    Option.iter item.descriptor.call_name ~f:(line output "name ");
    Option.iter partial.call_input ~f:(observed_text output "input ");
    each output partial.parts ~f:(fun part ->
      line output "part " (T.Part_id.to_string part.descriptor.id);
      let label =
        match part.descriptor.kind with
        | Text -> "text "
        | Refusal -> "refusal "
        | Reasoning_summary -> "reasoning summary "
        | Reasoning_text -> "reasoning text "
        | Image -> "image "
        | Unknown kind ->
          line output "[unsupported part] " kind;
          "observed text "
      in
      match part.text with
      | Some text -> observed_text output label text
      | None -> R.text output "[part text unavailable]\n")
;;

let activity output (summary : P.Activity.Tool.summary) =
  R.text output "tool scope ";
  scope_key output summary.key.scope;
  R.text output "\n";
  line output "call " summary.key.call_alias;
  Option.iter summary.key.parent ~f:(fun parent ->
    R.text output "parent scope ";
    scope_key output parent.scope;
    R.text output "\n";
    line output "parent call " parent.call_alias);
  (match summary.descriptor with
   | Some descriptor -> line output "name " descriptor.name
   | None -> R.text output "[tool descriptor unavailable]\n");
  each output summary.channels ~f:(fun channel ->
    let label =
      match channel.P.Activity.Tool.channel with
      | Assistant -> "assistant progress "
      | Reasoning -> "reasoning progress "
      | Stdout -> "stdout progress "
      | Stderr -> "stderr progress "
      | Activity -> "activity progress "
    in
    if not channel.complete then R.text output "[earlier activity unavailable]\n";
    line output label channel.text);
  match summary.state with
  | Running -> R.text output "tool outcome not observed (running)\n"
  | Finished { outcome; output = value } ->
    R.text
      output
      (match outcome with
       | Returned -> "tool returned\n"
       | Raised -> "tool raised\n"
       | Cancelled -> "tool cancelled\n");
    Option.iter value ~f:(tool_output output)
;;

let operation output (view : Agent_client.Live_projection.operation_view) =
  line output "operation " (P.Id.Operation.to_string view.operation_id);
  (match view.continuity with
   | Complete -> ()
   | Incomplete { first_missing_sequence } ->
     R.text output "[live gap from sequence ";
     R.text output (Int64.to_string first_missing_sequence);
     R.text output "]\n");
  let items = T.Draft.items view.drafts in
  let sources = T.Draft.sources view.drafts in
  let unknowns = T.Draft.unknown_events view.drafts in
  if
    List.is_empty items
    && List.is_empty sources
    && List.is_empty unknowns
    && List.is_empty view.activities
  then R.text output "[no retained item or tool observations]\n";
  each output items ~f:(draft output);
  each output sources ~f:(fun source ->
    scope output source.scope;
    R.text
      output
      (match source.completion with
       | None -> "source completion not observed\n"
       | Some Complete -> "source completed\n"
       | Some Incomplete -> "source incomplete\n"
       | Some Failed -> "source failed\n"
       | Some Cancelled -> "source cancelled\n"));
  each output unknowns ~f:(fun unknown ->
    scope output unknown.scope;
    line output "[unsupported live event] " unknown.provider_kind);
  each output view.activities ~f:(activity output)
;;

let equal_frame ~checkpoint left right =
  if not (Int.equal (String.length left) (String.length right))
  then false
  else (
    let rec loop index =
      if index = String.length left
      then true
      else (
        if index mod 4096 = 0 then checkpoint ();
        if Char.equal left.[index] right.[index] then loop (index + 1) else false)
    in
    loop 0)
;;

let observe t projection ~checkpoint =
  check t;
  let previous = t.previous in
  let poll () =
    check t;
    checkpoint ();
    check t
  in
  poll ();
  let%bind.Result frame =
    R.render t.limits ~checkpoint:poll ~f:(fun output ->
      (* Admit the conservative comparison scan before retaining another frame. *)
      Option.iter previous ~f:(fun previous ->
        R.charge output ~steps:(String.length previous));
      R.text output "[whole-session live observation; replaces previous observation]\n";
      (match Agent_client.Projection.synchronization projection with
       | Current -> ()
       | Snapshot_required _ -> R.text output "[live view stale; snapshot required]\n");
      let live = Agent_client.Projection.live projection in
      let current = Agent_client.Live_projection.operations live in
      let terminal = Agent_client.Live_projection.terminal_view live in
      if List.is_empty current && Option.is_none terminal
      then R.text output "[no retained live observations]\n";
      each output current ~f:(operation output);
      Option.iter terminal ~f:(fun view ->
        R.text output "[terminal operation observation]\n";
        operation output view))
  in
  let unchanged =
    Option.exists previous ~f:(fun previous ->
      equal_frame ~checkpoint:poll previous frame)
  in
  poll ();
  if unchanged
  then Ok None
  else (
    t.previous <- Some frame;
    Ok (Some frame))
;;
