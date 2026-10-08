open Core

module Agent_key = struct
  type t = Prompt.Chat_markdown.agent_content [@@deriving sexp, bin_io, hash, compare]

  let invariant (_ : t) = ()
end

module Agent_res_LRU = Ttl_lru_cache.Make (Agent_key)

type persistent_form =
  { max_size : int
  ; items : (Prompt.Chat_markdown.agent_content * string Agent_res_LRU.entry) list
    (* in LRU order *)
  }
[@@deriving bin_io]

let to_persistent (t : string Agent_res_LRU.t) : persistent_form =
  { max_size = Agent_res_LRU.max_size t
  ; items = Agent_res_LRU.to_alist t (* least -> most recently used *)
  }
;;

(* Rebuild a new LRU in the original order. *)
let of_persistent (pf : persistent_form) : string Agent_res_LRU.t =
  let t = Agent_res_LRU.create ~max_size:pf.max_size () in
  (* Insert pairs from least- to most-recently used order. *)
  List.iter pf.items ~f:(fun (key, data) -> Agent_res_LRU.set t ~key ~data);
  t
;;

(* Cstruct.of_bigarray  to convet bigarray to cstruct and then we can use eio to write using Path.with_open_out to get flow then we can use Writer to write the cstruct *)
let write_file ~file cache =
  Bin_prot_utils_eio.write_bin_prot'
    file
    [%bin_writer: persistent_form]
    (to_persistent cache)
;;

let read_file ~file =
  of_persistent (Bin_prot_utils_eio.read_bin_prot' file [%bin_reader: persistent_form])
;;

let clean_html raw_html =
  let decompressed =
    Option.value ~default:raw_html @@ Result.ok (Ezgzip.decompress raw_html)
  in
  let soup = Soup.parse decompressed in
  String.concat ~sep:"\n"
  @@ List.filter ~f:(fun s -> not @@ String.equal "" s)
  @@ List.map ~f:(fun s -> String.strip s)
  @@ Soup.texts soup
;;

let tab_on_newline (input : string) : string =
  let buffer = Buffer.create (String.length input) in
  String.iter
    ~f:(fun c ->
      let open Char in
      Buffer.add_char buffer c;
      if c = '\n'
      then (
        Buffer.add_char buffer '\t';
        Buffer.add_char buffer '\t'))
    input;
  Buffer.contents buffer
;;

let get_messages top_elements =
  List.filter_map top_elements ~f:(function
    | Prompt.Chat_markdown.Msg s -> Some s
    | _ -> None)
;;

let get_config top_elements =
  List.filter_map top_elements ~f:(function
    | Prompt.Chat_markdown.Config s -> Some s
    | _ -> None)
  |> List.hd_exn
;;

let get_content ~dir ~net url is_local =
  let host = Io.Net.get_host url in
  let path = Io.Net.get_path url in
  match is_local with
  | true -> Io.load_doc ~dir path
  | false ->
    let headers = Http.Header.of_list [ "Accept", "*/*"; "Accept-Encoding", "gzip" ] in
    let doc = Io.Net.get Io.Net.Default ~net ~host ~headers path in
    doc
;;

let rec get_user_msg ~dir ~net ~cache items =
  List.map
    ~f:(fun item ->
      match item with
      | Prompt.Chat_markdown.Basic item ->
        (match item.image_url with
         | Some url ->
           (match item.is_local with
            | true ->
              print_endline "local";
              let a = sprintf "<img src=\"%s\" %s />" url.url "local" in
              print_endline a;
              a
            | false ->
              print_endline "not local";
              sprintf "<img src=\"%s\" />" url.url)
         | None ->
           (match item.document_url with
            | Some url ->
              (match item.is_local with
               | true -> sprintf "<doc src=\"%s\" %s />" url "local"
               | false ->
                 (match item.cleanup_html with
                  | true -> sprintf "<doc src=\"%s\" %s />" url "strip"
                  | false -> sprintf "<doc src=\"%s\" />" url))
            | None -> Option.value ~default:"" item.text))
      | Agent ({ url; is_local; items } as agent) ->
        let a =
          Agent_res_LRU.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
            let prompt = get_content ~dir ~net url is_local in
            run_agent ~dir ~net ~cache prompt items)
        in
        a
      (* let prompt = get_content ~dir ~net url is_local in
        let contents = run_agent ~dir ~net prompt items in
        contents) *))
    items
  |> String.concat ~sep:"\n"

and convert ~dir ~net ~cache msg =
  let { Prompt.Chat_markdown.role
      ; content
      ; name
      ; function_call
      ; tool_call
      ; tool_call_id
      ; id = _
      ; status = _
      ; type_ = _
      }
    =
    msg
  in
  let function_call =
    Option.map function_call ~f:(fun function_call ->
      { Openai.Completions.name = function_call.name
      ; arguments = function_call.arguments
      })
  in
  let tool_calls =
    Option.map tool_call ~f:(fun tool_call ->
      [ { Openai.Completions.id = Some tool_call.id
        ; function_ =
            Some
              { arguments = tool_call.function_.arguments
              ; name = tool_call.function_.name
              }
        ; type_ = Some "function"
        }
      ])
  in
  let content =
    Option.map content ~f:(fun s ->
      match s with
      | Prompt.Chat_markdown.Text t -> Openai.Completions.Text t
      | Items items ->
        Openai.Completions.Items
          (List.map items ~f:(fun item ->
             match item with
             | Basic item ->
               let image_url =
                 match item.image_url with
                 | Some url ->
                   (match item.is_local with
                    | true ->
                      Some
                        { Openai.Completions.url = Io.Base64.file_to_data_uri ~dir url.url
                        }
                    | false -> Some { Openai.Completions.url = url.url })
                 | None -> None
               in
               let text =
                 match item.document_url with
                 | Some url ->
                   (match item.is_local with
                    | true ->
                      let doc = Io.load_doc ~dir url in
                      Some doc
                    | false ->
                      let host = Io.Net.get_host url in
                      let path = Io.Net.get_path url in
                      (match item.cleanup_html with
                       | true ->
                         let headers =
                           Http.Header.of_list
                             [ "Accept", "*/*"; "Accept-Encoding", "gzip" ]
                         in
                         let doc = Io.Net.get Io.Net.Default ~net ~host ~headers path in
                         Some (clean_html doc)
                       | false ->
                         let doc = Io.Net.get Io.Net.Default ~net ~host path in
                         Some doc))
                 | None -> item.text
               in
               Openai.Completions.{ type_ = item.type_; text; image_url }
             | Agent ({ url; is_local; items } as agent) ->
               print_endline "agent hit rate";
               Agent_res_LRU.hit_rate cache |> Float.to_string_hum |> print_endline;
               let contents =
                 Agent_res_LRU.find_or_add
                   cache
                   agent
                   ~ttl:Time_ns.Span.day
                   ~default:(fun () ->
                     let prompt = get_content ~dir ~net url is_local in
                     run_agent ~dir ~net ~cache prompt items)
               in
               Agent_res_LRU.hit_rate cache |> Float.to_string_hum |> print_endline;
               Openai.Completions.
                 { type_ = "text"; text = Some contents; image_url = None })))
  in
  (* (match content with
       | None -> ()
       | Some content ->
         print_endline
         @@ Jsonaf.to_string_hum
         @@ Openai.jsonaf_of_chat_message_content content); *)
  (match tool_call_id with
   | None -> ()
   | Some tool_call_id -> print_endline tool_call_id);
  { Openai.Completions.role; content; name; function_call; tool_calls; tool_call_id }

and run_agent _prompt _items ~dir:_ ~net:_ ~cache:_ : string =
  invalid_arg
    "Chat_completion.run_agent is retired; use Chat_response.Driver with an explicit \
     selected inference context"
;;

(** The old Chat Completions executor predates canonical history and cannot
    preserve captured provider data. Retire before file/network/tool effects;
    first-party entry points use the single selected Chat_response executor. *)
let run_completion ~env:_ ~output_file:_ ~prompt_file:_ : unit =
  invalid_arg
    "Chat_completion.run_completion is retired; use Chat_response.Driver with an \
     explicit selected inference context"
;;
