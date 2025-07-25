open Core

module Markdown = struct
  type t = string [@@deriving sexp, bin_io, hash, compare]

  let to_string (md : t) : string = md
end

(* -------------------------------------------------------------- *)
(* Utilities                                                      *)
(* -------------------------------------------------------------- *)
let strip_dot ext =
  match String.chop_prefix ext ~prefix:"." with
  | Some s -> s
  | None -> ext
;;

(* add 1-based line numbers preserving original indices *)
let add_line_numbers ?(start_at = 1) (lines : string list) : string =
  List.mapi lines ~f:(fun i line -> sprintf "%d. %s" (start_at + i) line)
  |> String.concat ~sep:"\n"
;;

(* Quick pattern-match for github.com/owner/repo/blob/branch/path#Lx-Ly *)
let github_raw_url_and_lines url : (string * (int * int option)) option =
  let uri = Uri.of_string url in
  match Uri.host uri with
  | Some "github.com" ->
    let path = Uri.path uri |> String.rstrip ~drop:(Char.equal '/') in
    let segments =
      String.split ~on:'/' path |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    (match segments with
     | owner :: repo :: "blob" :: branch :: rest when not (List.is_empty rest) ->
       let raw_path = String.concat ~sep:"/" (owner :: repo :: branch :: rest) in
       let raw_url = "https://raw.githubusercontent.com/" ^ raw_path in
       (* parse fragment *)
       let frag = Uri.fragment uri in
       let line_range =
         match frag with
         | None -> 1, None
         | Some f when String.is_prefix f ~prefix:"L" ->
           let parts = String.split ~on:'-' f in
           (match parts with
            | [ single ] ->
              let n = String.drop_prefix single 1 |> Int.of_string in
              n, Some n
            | [ start_; finish ] when String.is_prefix finish ~prefix:"L" ->
              let s = String.drop_prefix start_ 1 |> Int.of_string in
              let e = String.drop_prefix finish 1 |> Int.of_string in
              s, Some e
            | _ -> 1, None)
         | _ -> 1, None
       in
       Some (raw_url, line_range)
     | _ -> None)
  | _ -> None
;;

(* Fetch GitHub raw file; return specialised Markdown if recognised *)
let try_github_fast_path ~net url : string option =
  match github_raw_url_and_lines url with
  | None -> None
  | Some (raw_url, (lstart, lend_opt)) ->
    (match Fetch.get ~net raw_url with
     | Error _ -> None
     | Ok body ->
       let ext = Stdlib.Filename.extension (Uri.path (Uri.of_string raw_url)) in
       (* Slice lines if anchor present *)
       let body =
         match lend_opt with
         | None when lstart = 1 -> body
         | _ ->
           let lines = String.split_lines body in
           let len = List.length lines in
           let lstart_idx = Int.max 1 lstart in
           let lend = Option.value lend_opt ~default:lstart_idx in
           let lend_idx = Int.min len lend in
           let slice = List.slice lines (lstart_idx - 1) lend_idx in
           String.concat ~sep:"\n" slice
       in
       let is_markdown =
         List.mem [ ".md"; ".markdown"; ".mdown"; ".mkdn" ] ext ~equal:String.equal
       in
       if is_markdown
       then Some body
       else (
         let lang = strip_dot ext in
         Some
           (sprintf
              "```%s\n%s\n```"
              lang
              (add_line_numbers ~start_at:lstart (String.split_lines body)))))
;;

let html_to_markdown_string html =
  let parse_with_soup () =
    try Ok (Soup.parse html) with
    | Soup.Parse_error _ -> Error ()
  in
  let parse_with_markup () =
    try
      let signals = Markup.string html |> Markup.parse_html |> Markup.signals in
      Ok (Soup.from_signals signals)
    with
    | _ -> Error ()
  in
  let soup_res =
    match parse_with_soup () with
    | Ok s -> Some s
    | Error () ->
      (match parse_with_markup () with
       | Ok s -> Some s
       | Error () -> None)
  in
  match soup_res with
  | Some soup ->
    (try Html_to_md.convert soup |> Md_render.to_string with
     | Soup.Parse_error _ | Failure _ | Invalid_argument _ | _ ->
       Printf.sprintf "```html\n%s\n```" html)
  | None -> Printf.sprintf "```html\n%s\n```" html
;;

let fetch_and_convert ~env ~net url =
  (* 1. GitHub-optimised path *)
  match try_github_fast_path ~net url with
  | Some md -> md
  | None ->
    (* 2. Generic HTML → Markdown path (existing) *)
    (match Fetch.get ~net url with
     | Error msg -> msg
     | Ok html ->
       (match html_to_markdown_string html with
        | "" ->
          (* Attempt to fetch the page using a headless Chrome browser for progressive web apps *)
          let try_chrome_headless (url : string) : string =
            let proc_mgr = Eio.Stdenv.process_mgr env in
            Eio.Switch.run
            @@ fun sw ->
            (* 1.  Pipe for capturing stdout. *)
            let r, w = Eio.Process.pipe ~sw proc_mgr in
            (* 2. Pipe for capturing stderr. *)
            let _, w_err = Eio.Process.pipe ~sw proc_mgr in
            match
              Eio.Process.spawn
                ~sw
                proc_mgr
                ~stdout:w
                ~stderr:w_err
                [ "chrome-dump"; url ]
            with
            | exception ex ->
              let err_msg = Fmt.str "error running %s fetch: %a" url Eio.Exn.pp ex in
              Eio.Flow.close w;
              Eio.Flow.close w_err;
              err_msg
            | _child ->
              Eio.Flow.close w;
              Eio.Flow.close w_err;
              (match
                 Eio.Buf_read.parse_exn ~max_size:1_000_000 Eio.Buf_read.take_all r
               with
               | res ->
                 let max_len = 1000000 in
                 let res =
                   if String.length res > max_len
                   then String.append (String.sub res ~pos:0 ~len:max_len) " ...truncated"
                   else res
                 in
                 res
               | exception ex -> Fmt.str "error running %s fetch: %a" url Eio.Exn.pp ex)
          in
          (* timeout functioin eio *)
          let try_chrome_headless_wto x =
            try
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60.0 (fun () ->
                try_chrome_headless x)
            with
            | Eio.Time.Timeout ->
              Printf.sprintf "timeout running chrome_dump command %s" x
            | ex ->
              Printf.sprintf
                "error running chrome_dump command %s: %ss"
                x
                (Exn.to_string ex)
          in
          html_to_markdown_string (try_chrome_headless_wto url)
        | md -> md))
;;

let convert_html_file path =
  let html = Eio.Path.load path in
  html_to_markdown_string html
;;
