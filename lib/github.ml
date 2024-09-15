open Core
open Io
open Jsonaf

let parse_files_from_response response =
  let json = of_string response in
  match list json with
  | Some entries ->
    List.filter_map
      ~f:(fun entry ->
        try
          let filename = Option.value_exn (string (member_exn "name" entry)) in
          let url = Option.value_exn (string (member_exn "download_url" entry)) in
          print_endline url;
          Some (filename, url)
        with
        | _ -> None)
      entries
  | None -> []
;;

let api_key = Sys.getenv "GITHUB_API_KEY" |> Option.value ~default:""
let github_api_base = "api.github.com"

let get_repo_contents_path owner repo path =
  Printf.sprintf "/repos/%s/%s/contents/%s" owner repo path
;;

let rec download_files_aux net files path =
  match files with
  | [] -> ()
  | (filename, url) :: rest ->
    Net.download_file net url ~dir:path ~filename;
    download_files_aux net rest path
;;

let download_files net path folder owner repo =
  let url = get_repo_contents_path owner repo folder in
  let headers =
    Http.Header.of_list
      [ "Authorization", "Bearer " ^ api_key
      ; "Content-Type", "application/json"
      ; "Accept", "application/vnd.github+json"
      ; "X-GitHub-Api-Version", "2022-11-28"
      ]
  in
  let content = Net.get Default ~net ~host:github_api_base ~headers url in
  let files = parse_files_from_response content in
  download_files_aux net files path
;;
