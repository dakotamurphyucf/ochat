open Core
module Res = Openai.Responses

let show_output (output : Res.Tool_output.Output.t) =
  let detail_to_string = function
    | Res.Input_message.High -> "high"
    | Res.Input_message.Low -> "low"
    | Res.Input_message.Auto -> "auto"
  in
  match output with
  | Res.Tool_output.Output.Text s -> printf "Text:%s\n" s
  | Content parts ->
    List.iter parts ~f:(function
      | Res.Tool_output.Output_part.Input_text { text } -> printf "Input_text:%s\n" text
      | Res.Tool_output.Output_part.Input_image { image_url; detail } ->
        let detail = Option.value_map detail ~default:"<none>" ~f:detail_to_string in
        printf "Input_image:%s detail=%s\n" image_url detail)
;;

let output_json_of_fn_out (t : Res.Function_call_output.t) : Jsonaf.t =
  match Res.Function_call_output.jsonaf_of_t t with
  | `Object _ as json -> Jsonaf.member_exn "output" json
  | _ -> failwith "expected function_call_output to encode as object"
;;

let%expect_test "function_call_output.output: decode/encode string" =
  let json =
    Jsonaf.of_string
      {|{
  "type": "function_call_output",
  "call_id": "call-1",
  "output": "hello"
}|}
  in
  let t = Res.Function_call_output.t_of_jsonaf json in
  show_output t.output;
  (match output_json_of_fn_out t with
   | `String _ -> print_endline "encoded:string"
   | `Array _ -> print_endline "encoded:array"
   | _ -> print_endline "encoded:other");
  [%expect
    {|
    Text:hello
    encoded:string
    |}]
;;

let%expect_test "function_call_output.output: decode/encode content array" =
  let json =
    Jsonaf.of_string
      {|{
  "type": "function_call_output",
  "call_id": "call-2",
  "output": [
    {"type": "input_text", "text": "hi"},
    {"type": "input_image", "image_url": "https://example.invalid/a.png"},
    {"type": "input_image", "image_url": "https://example.invalid/b.png", "detail": "high"}
  ]
}|}
  in
  let t = Res.Function_call_output.t_of_jsonaf json in
  show_output t.output;
  (match output_json_of_fn_out t with
   | `String _ -> print_endline "encoded:string"
   | `Array parts -> printf "encoded:array parts=%d\n" (List.length parts)
   | _ -> print_endline "encoded:other");
  [%expect
    {|
    Input_text:hi
    Input_image:https://example.invalid/a.png detail=<none>
    Input_image:https://example.invalid/b.png detail=high
    encoded:array parts=3
    |}]
;;
