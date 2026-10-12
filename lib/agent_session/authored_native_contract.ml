open Core
module CM = Prompt.Chat_markdown
module Path = Chatmd_shell_spec.Path_expr

let validate ~original ~selected ~checkpoint =
  let fail () =
    Error
      (Agent_protocol.Error.invalid_request "selected code changes native tool authority")
  in
  let string left right =
    checkpoint ();
    if not (Int.equal (String.length left) (String.length right))
    then false
    else (
      let rec loop offset =
        if offset = String.length left
        then true
        else (
          checkpoint ();
          let finish = Int.min (offset + 4096) (String.length left) in
          let rec chunk index =
            index = finish || (Char.equal left.[index] right.[index] && chunk (index + 1))
          in
          chunk offset && loop finish)
      in
      loop 0)
  in
  let option equal left right =
    match left, right with
    | None, None -> true
    | Some left, Some right -> equal left right
    | None, Some _ | Some _, None -> false
  in
  let rec list equal left right =
    checkpoint ();
    match left, right with
    | [], [] -> true
    | left :: lefts, right :: rights -> equal left right && list equal lefts rights
    | [], _ :: _ | _ :: _, [] -> false
  in
  let path (left : Path.t) (right : Path.t) =
    match left, right with
    | Path.Absolute left, Absolute right -> string left right
    | Relative left, Relative right ->
      Path.equal_base left.base right.base && string left.path right.path
    | Absolute _, Relative _ | Relative _, Absolute _ -> false
  in
  let root (left : Chatmd_read_file_spec.Root.t) (right : Chatmd_read_file_spec.Root.t) =
    string left.id right.id
    && path left.path right.path
    && option string left.description right.description
  in
  let tool (left : CM.tool) (right : CM.tool) =
    match left, right with
    | CM.Builtin left, Builtin right | Inherited left, Inherited right ->
      string left right
    | Read_file left, Read_file right ->
      list root left.roots right.roots && option string left.description right.description
    | Mcp left, Mcp right ->
      option (list string) left.names right.names
      && option string left.description right.description
      && string left.mcp_server right.mcp_server
      && Bool.equal left.strict right.strict
      && option string left.client_id_env right.client_id_env
      && option string left.client_secret_env right.client_secret_env
    | (Builtin _ | Inherited _ | Read_file _ | Mcp _), _ -> false
    | (Custom _ | Shell _ | Agent _ | Persistent_agent _ | Extension _), _ -> assert false
  in
  let collect elements =
    let rec loop count reversed = function
      | [] -> Ok (List.rev reversed)
      | element :: rest ->
        checkpoint ();
        if count >= 16_384
        then fail ()
        else (
          match element with
          | CM.Tool ((Builtin _ | Inherited _ | Read_file _ | Mcp _) as declaration) ->
            loop (count + 1) (declaration :: reversed) rest
          | Tool (Custom _ | Shell _ | Agent _ | Persistent_agent _ | Extension _)
          | Msg _
          | Developer _
          | System _
          | User _
          | Assistant _
          | Tool_call _
          | Tool_response _
          | Config _
          | Reasoning _
          | Shell_runtime _
          | Moderator_runtime _
          | Script _
          | Shell_script _
          | Extension_script _
          | Authoring_context _
          | Authoring_help _ -> loop (count + 1) reversed rest)
    in
    loop 0 [] elements
  in
  let open Result.Let_syntax in
  let%bind original = collect original in
  let%bind selected = collect selected in
  if list tool original selected then Ok () else fail ()
;;
