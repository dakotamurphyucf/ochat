open Core
open Angstrom
open Jsonaf.Export
module C = Openai_chat_completion

type template_element =
  | System_message : string -> template_element
  | Variable : string -> template_element
  | Expression : string -> template_element
  | Text : string -> template_element

let insides = take_while1 (fun c -> Char.(c <> '}'))

let c =
  peek_string 1
  >>= function
  | "$" -> advance 1 *> insides >>| fun s -> Variable s
  | _ -> insides >>| fun s -> Expression s
;;

let system_message_parser =
  string "<system-message>" *> take_while1 (fun c -> Char.(c <> '<'))
  <* string "</system-message>"
;;

let expression_parser c = string "{{" *> c <* string "}}"
let text_parser = take_while1 (fun c -> Char.(c <> '{')) >>| fun s -> Text s

let template_parser =
  many
    (choice
       [ (system_message_parser >>| fun s -> System_message s)
       ; expression_parser c
       ; text_parser
       ])
  <* end_of_input
;;

(*
   My name: Unknown function: GetMyName
   My email: Unknown function: GetMyEmailAddress
   My hobbies: Unknown function: "my hobbies"
   Recipient: Unknown function: $recipient
   Email to reply to:
   =========
   Unknown function: $sourceEmail
   =========
   Generate a response to the email, to say: Unknown function: $input

   Include the original email quoted after the response.
*)
module type TemplateHandler = sig
  type t

  val handle_variable : string -> t
  val handle_function : string -> t
  val handle_text : string -> t
  val to_string : t -> string
end

module MakeTemplateProcessor (Handler : TemplateHandler) = struct
  let process_template (template_elements : template_element list) : Handler.t list =
    List.map
      ~f:(function
        | System_message s -> Handler.handle_text s
        | Variable s -> Handler.handle_variable s
        | Expression s -> Handler.handle_function s
        | Text t -> Handler.handle_text t)
      template_elements
  ;;
end
(* Helper parsers *)

module MyTemplateHandler : TemplateHandler = struct
  type t = string

  let handle_variable = function
    | "recipient" -> "John Doe"
    | _var -> ""
  ;;

  let handle_function func =
    match func with
    | "GetMyName" -> "Alice"
    | "GetMyEmailAddress" -> "alice@example.com"
    | _ -> ""
  ;;

  let handle_text text = text
  let to_string t = t
end

module MyTemplateProcessor = MakeTemplateProcessor (MyTemplateHandler)

let run () =
  let input =
    {|<system-message>boats and hoes</system-message>
  My name: {{GetMyName}}
  My email: {{GetMyEmailAddress}}
  My hobbies: {{"my hobbies"}}
  Recipient: {{$recipient}}
  Email to reply to:
  =========
  {{$sourceEmail}}
  =========
  Generate a response to the email, to say: {{$input}}
  
  Include the original email quoted after the response.|}
  in
  let parsed_template = Angstrom.parse_string ~consume:Prefix template_parser input in
  let temp =
    match parsed_template with
    | Ok template_elements ->
      let processed_elements = MyTemplateProcessor.process_template template_elements in
      String.concat ~sep:"" @@ List.map ~f:MyTemplateHandler.to_string processed_elements
    | Error msg -> failwith ("Parsing error: " ^ msg)
  in
  print_endline temp
;;

module Chat_markdown = struct
  type function_call =
    { name : string
    ; arguments : string
    }
  [@@deriving jsonaf, sexp]

  type msg =
    { role : string
    ; content : string option
    ; name : string option [@jsonaf.option]
    ; function_call : function_call option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp]

  type chat_element =
    | Message of msg
    | Text of string
  [@@deriving sexp]

  let chat_element_to_string = function
    | Text s -> s
    | Message m -> m.content |> Option.value ~default:""
  ;;

  let attr_to_msg attr content =
    let hash_tbl = Hashtbl.create (module String) in
    List.iter attr ~f:(fun ((_, b), s) -> Hashtbl.add_exn ~key:b ~data:s hash_tbl);
    let function_call, content =
      match Hashtbl.mem hash_tbl "function_call" with
      | false -> None, content
      | true ->
        ( Some
            { name = Hashtbl.find_exn hash_tbl "function_name"
            ; arguments = Option.value_exn content
            }
        , None )
    in
    { role = Hashtbl.find_exn hash_tbl "role"
    ; name = Hashtbl.find hash_tbl "name"
    ; function_call
    ; content
    }
  ;;

  let to_s s = String.concat ~sep:"" (List.map s ~f:chat_element_to_string)

  let chat_elements =
    Markup.elements (fun (_, name) att ->
      match name with
      | "msg" ->
        List.iter att ~f:(fun ((a, b), s) ->
          print_endline "attr";
          print_endline "a:";
          print_endline a;
          print_endline "b:";
          print_endline b;
          print_endline "s:";
          print_endline s);
        true
      (* | "body" -> System children *)
      | _ -> false)
  ;;

  let parse_chat_elements =
    Markup.tree
      ~text:(fun ss -> Text (String.concat ~sep:"" ss))
      ~element:(fun (_, name) attr children ->
        match name with
        | "msg" ->
          let contents =
            match to_s children with
            | "" -> None
            | c -> Some c
          in
          (* this is where we can parse the attr and contents into the proper message type *)
          Message (attr_to_msg attr contents)
        (* | "body" -> System children *)
        | _ ->
          Text (String.concat ~sep:"" [ "<"; name; ">"; to_s children; "</"; name; ">" ]))
  ;;

  let parse_chat_inputs chat =
    Markup.string chat
    |> Markup.parse_html ~context:`Document
    |> Markup.signals
    |> chat_elements
    |> Markup.map parse_chat_elements
    |> Markup.to_list
    |> List.filter_map ~f:(function
      | None -> None
      | Some elem ->
        (match elem with
         | Text _ -> None
         | Message m -> Some m))
  ;;
end
