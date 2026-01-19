open Core

[@@@warning "-16-27-32-39"]

let getenv_opt = Core.Sys.getenv

let read_file_opt path =
  try Some (In_channel.read_all path) with
  | _ -> None
;;

let guardrails_default = Templates.system_prompt_guardrails

let load_template ~(env : Eio_unix.Stdenv.base) name =
  let dir = Eio.Stdenv.fs env in
  let path = Eio.Path.(dir / "meta-prompt" / "templates" / name) in
  try Some (Eio.Path.load path) with
  | _ -> None
;;

let load_guardrails ~(env : Eio_unix.Stdenv.base) =
  let dir = Eio.Stdenv.fs env in
  let path =
    Eio.Path.(dir / "meta-prompt" / "integration" / "system_prompt_guardrails.txt")
  in
  try Eio.Path.load path with
  | _ -> guardrails_default
;;

let load_kv_overrides ~(env : Eio_unix.Stdenv.base) : (string, string) Hashtbl.t option =
  let parse_lines s : (string, string) Hashtbl.t =
    let tbl = Hashtbl.create (module String) in
    String.split_lines s
    |> List.iter ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line || Char.( = ) line.[0] '#'
      then ()
      else (
        let split_on ch =
          match String.lsplit2 ~on:ch line with
          | Some (k, v) -> Some (String.strip k, String.strip v)
          | None -> None
        in
        match split_on ':' |> Option.first_some (split_on '=') with
        | None -> ()
        | Some (k, v) -> Hashtbl.set tbl ~key:k ~data:v));
    tbl
  in
  let dir = Eio.Stdenv.fs env in
  let try_load p =
    try Some (Eio.Path.load p |> parse_lines) with
    | _ -> None
  in
  match getenv_opt "META_PROMPT_ONLINE_CONFIG" with
  | Some rel -> try_load Eio.Path.(dir / rel)
  | None ->
    let p1 = Eio.Path.(dir / "meta-prompt" / "online_config.conf") in
    (match try_load p1 with
     | Some _ as t -> t
     | None -> try_load Eio.Path.(dir / "meta-prompt" / "online_config.txt"))
;;

let build_iteration_user_content ~(env : Eio_unix.Stdenv.base) ~goal ~current_prompt =
  let b = Buffer.create (String.length goal + String.length current_prompt + 256) in
  Buffer.add_string b "CURRENT_PROMPT\n";
  Buffer.add_string b current_prompt;
  Buffer.add_string b "\n\nGOAL\n";
  Buffer.add_string b goal;
  let kv = load_kv_overrides ~env in
  let get k ~default =
    match kv with
    | None -> default
    | Some tbl -> Option.value (Hashtbl.find tbl k) ~default
  in
  let re = get "iterate.reasoning_effort" ~default:"low" in
  let v = get "iterate.verbosity" ~default:"low" in
  let rsp = get "iterate.use_responses_api" ~default:"true" in
  let eag = get "iterate.eagerness" ~default:"medium" in
  let md = get "iterate.markdown_allowed" ~default:"true" in
  let dom = get "iterate.domain" ~default:"general" in
  Buffer.add_string
    b
    (Printf.sprintf
       "\n\nPARAMETERS\nreasoning_effort: %s\nverbosity: %s\nuseResponsesAPI: %s\n"
       re
       v
       rsp);
  Buffer.add_string b (Printf.sprintf "\nTARGET_PROFILE\neagerness: %s\n" eag);
  Buffer.add_string
    b
    (Printf.sprintf "\nOUTPUT_CONTRACT and MARKDOWN_POLICY\nmarkdownAllowed: %s\n" md);
  Buffer.add_string b (Printf.sprintf "\nDOMAIN\n/%s\n" dom);
  Buffer.contents b
;;

let extract_section ~text ~section : string option =
  match String.substr_index text ~pattern:section with
  | None -> None
  | Some start ->
    let after = start + String.length section in
    let search_space = String.drop_prefix text after in
    let candidates =
      [ "\nOverview"
      ; "\nIssues_Found"
      ; "\nMinimal_Edit_List"
      ; "\nRevised_Prompt"
      ; "\nOptional_Toggles"
      ; "\nAPI_Parameter_Suggestions"
      ; "\nTest_Plan"
      ; "\nTelemetry"
      ]
    in
    let next_idx =
      List.filter_map candidates ~f:(fun c ->
        match String.substr_index search_space ~pattern:c with
        | None -> None
        | Some i -> if Int.( = ) i 0 then None else Some i)
      |> List.min_elt ~compare:Int.compare
    in
    let content =
      match next_idx with
      | None -> search_space
      | Some i -> String.prefix search_space i
    in
    let content = String.strip content in
    if String.is_empty content then None else Some content
;;

let get_iterate_system_prompt env =
  let templ =
    match load_template ~env "iteration_prompt_v2.txt" with
    | Some t -> t
    | None -> Templates.iteration_prompt_v2
  in
  let system_text = templ ^ "\n" ^ load_guardrails ~env in
  system_text
;;

let iterate_revised_prompt ~env ~goal ~current_prompt ~proposer_model : string option =
  match getenv_opt "OPENAI_API_KEY" with
  | None -> None
  | Some _ ->
    (try
       let dir = Eio.Stdenv.fs env in
       let net = Eio.Stdenv.net env in
       let open Openai.Responses in
       let system_text = get_iterate_system_prompt env in
       let system_msg : Input_message.t =
         { role = Developer
         ; content = [ Text { text = system_text; _type = "input_text" } ]
         ; _type = "message"
         }
       in
       let user_msg : Input_message.t =
         { role = User
         ; content =
             [ Text
                 { text = build_iteration_user_content ~env ~goal ~current_prompt
                 ; _type = "input_text"
                 }
             ]
         ; _type = "message"
         }
       in
       let inputs : Item.t list =
         [ Item.Input_message system_msg; Item.Input_message user_msg ]
       in
       let max_output_tokens = 1000000 in
       let chosen_model = Option.value proposer_model ~default:Request.Gpt5 in
       let ({ Response.output; _ } : Response.t) =
         post_response
           Default
           ~reasoning:{ effort = Some High; summary = Some Detailed }
           ~max_output_tokens
           ~model:chosen_model
           ~dir
           net
           ~inputs
       in
       let rec first_text = function
         | [] -> None
         | Item.Output_message om :: _ ->
           (match om.Output_message.content with
            | [] -> None
            | { text; _ } :: _ -> Some text)
         | _ :: tl -> first_text tl
       in
       match first_text output with
       | None -> None
       | Some txt -> extract_section ~text:txt ~section:"Revised_Prompt\n"
     with
     | exn ->
       Log.emit
         `Debug
         (Printf.sprintf
            "prompt_factory_online.iterate_revised_prompt: %s"
            (Exn.to_string exn));
       None)
;;

let build_generator_user_content ~(env : Eio_unix.Stdenv.base) ~agent_name ~goal =
  let b = Buffer.create (String.length goal + 256) in
  let kv = load_kv_overrides ~env in
  let get k ~default =
    match kv with
    | None -> default
    | Some tbl -> Option.value (Hashtbl.find tbl k) ~default
  in
  let agent_name = get "create.agent_name" ~default:agent_name in
  Buffer.add_string b "\n\nGOAL\n";
  Buffer.add_string b goal;
  let audience = get "create.audience" ~default:"technical users" in
  let tone = get "create.tone" ~default:"neutral" in
  let domain = get "create.domain" ~default:"general" in
  let md = get "create.markdown_allowed" ~default:"true" in
  let eag = get "create.eagerness" ~default:"medium" in
  let re = get "create.reasoning_effort" ~default:"low" in
  let verb = get "create.verbosity" ~default:"low" in
  let rsp = get "create.use_responses_api" ~default:"true" in
  let sc =
    match kv with
    | Some tbl ->
      (match Hashtbl.find tbl "create.success_criteria" with
       | Some s -> s
       | None ->
         "- Adheres to safety and output constraints\n- Produces correct, safe results")
    | None ->
      "- Adheres to safety and output constraints\n- Produces correct, safe results"
  in
  Buffer.add_string b "AGENT_NAME\n";
  Buffer.add_string b agent_name;
  Buffer.add_string b ("\n\nSUCCESS_CRITERIA\n" ^ sc ^ "\n");
  Buffer.add_string b ("\nAUDIENCE\n" ^ audience ^ "\n");
  Buffer.add_string b ("\nTONE\n" ^ tone ^ "\n");
  Buffer.add_string b ("\nDOMAIN\n" ^ domain ^ "\n");
  Buffer.add_string b ("\nOUTPUT_CONTRACT\nmarkdownAllowed: " ^ md ^ "\n");
  Buffer.add_string b ("\nEAGERNESS_PROFILE\n" ^ eag ^ "\n");
  Buffer.add_string b ("\nREASONING_EFFORT\n" ^ re ^ "\n");
  Buffer.add_string b ("\nVERBOSITY_TARGET\n" ^ verb ^ "\n");
  Buffer.add_string b "\nSTOP_CONDITIONS\n\n";
  Buffer.add_string b "\nSAFETY_BOUNDARIES\n\n";
  Buffer.add_string b ("\nuseResponsesAPI\n" ^ rsp ^ "\n");
  Buffer.contents b
;;

let create_pack_online ~env ~agent_name ~goal ~proposer_model : string option =
  match getenv_opt "OPENAI_API_KEY" with
  | None -> None
  | Some _ ->
    (try
       let dir = Eio.Stdenv.fs env in
       let net = Eio.Stdenv.net env in
       let open Openai.Responses in
       let templ =
         match load_template ~env "generator_prompt_v2.txt" with
         | Some t -> t
         | None -> Templates.generator_prompt_v2
       in
       let system_text = templ in
       let system_msg : Input_message.t =
         { role = Developer
         ; content = [ Text { text = system_text; _type = "input_text" } ]
         ; _type = "message"
         }
       in
       let user_msg : Input_message.t =
         { role = User
         ; content =
             [ Text
                 { text = build_generator_user_content ~env ~agent_name ~goal
                 ; _type = "input_text"
                 }
             ]
         ; _type = "message"
         }
       in
       let inputs : Item.t list =
         [ Item.Input_message system_msg; Item.Input_message user_msg ]
       in
       let max_output_tokens = 1000000 in
       let chosen_model = Option.value proposer_model ~default:Request.Gpt5 in
       let ({ Response.output; _ } : Response.t) =
         post_response
           Default
           ~reasoning:{ effort = Some High; summary = Some Detailed }
           ~max_output_tokens
           ~model:chosen_model
           ~dir
           net
           ~inputs
       in
       let rec first_text = function
         | [] -> None
         | Item.Output_message om :: _ ->
           (match om.Output_message.content with
            | [] -> None
            | { text; _ } :: _ -> Some text)
         | _ :: tl -> first_text tl
       in
       first_text output
     with
     | exn ->
       Log.emit
         `Debug
         (Printf.sprintf
            "prompt_factory_online.create_pack_online: %s"
            (Exn.to_string exn));
       None)
;;
