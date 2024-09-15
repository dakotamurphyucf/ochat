User:
Design and implement an ocaml module that downloads all the raw files in a github repo folder via http request.  
Assume we have the http module Client with the interface
```ocaml
module Client : sig
  type response = Http.Response.t * Buf_read.t
  type host = string
  type port = int
  type resource_path = host
  type 'a env = 'a constraint 'a = < net : Net.t ; .. >

  type ('a, 'b) body_disallowed_call =
    ?pipeline_requests:bool ->
    ?version:Http.Version.t ->
    ?headers:Http.Header.t ->
    ?conn:'a ->
    ?port:port ->
    'b ->
    host:host ->
    host ->
    response
    constraint 'a = #Flow.two_way
    constraint 'b = < net : Net.t ; .. >

  type ('a, 'b) body_allowed_call =
    ?pipeline_requests:bool ->
    ?version:Http.Version.t ->
    ?headers:Http.Header.t ->
    ?body:Body.t ->
    ?conn:'a ->
    ?port:port ->
    'b ->
    host:host ->
    host ->
    response
    constraint 'a = #Flow.two_way
    constraint 'b = < net : Net.t ; .. >

  val call :
    ?pipeline_requests:bool ->
    ?meth:Http.Method.t ->
    ?version:Http.Version.t ->
    ?headers:Http.Header.t ->
    ?body:Body.t ->
    ?conn:#Flow.two_way ->
    ?port:port ->
    < net : Net.t ; .. > env ->
    host:host ->
    resource_path ->
    response

  val get :
    (#Flow.two_way, < net : Net.t ; .. >) body_disallowed_call

  val head :
    (#Flow.two_way, < net : Net.t ; .. >) body_disallowed_call

  val delete :
    (#Flow.two_way, < net : Net.t ; .. >) body_disallowed_call

  val post :
    (#Flow.two_way, < net : Net.t ; .. >) body_allowed_call

  val put :
    (#Flow.two_way, < net : Net.t ; .. >) body_allowed_call

  val patch :
    (#Flow.two_way, < net : Net.t ; .. >) body_allowed_call

  val read_fixed : response -> string

  val read_chunked :
    response -> (Body.chunk -> unit) -> Http.Header.t option
end
```

And we have the module Doc to load and save a doc (file) with the interface

```ocaml
(** Document
  
    The [save_prompt path filename content] and [load_prompt path filename] functions are used for saving and loading a document from disk. *)

val save_prompt : 'a Eio.Path.t -> string -> string -> unit
val load_prompt : 'a Eio.Path.t -> string -> string
```

Model the design as ocaml interface files first, then write the implementation.

Assistant:
First, let's create the interface file for the Github module. We'll call it `github.mli`.

```ocaml
(* github.mli *)

module Github : sig
  val download_files :
    < net : Net.t ; .. > env ->
    string ->
    string ->
    unit
end
```

Now, let's implement the Github module in `github.ml`.

```ocaml
(* github.ml *)

open Client
open Doc

let github_api_base = "api.github.com"

let get_repo_contents_path owner repo path =
  Printf.sprintf "/repos/%s/%s/contents/%s" owner repo path

let download_file env url path filename =
  Eio.Net.with_tcp_connect ~service:"https" ~host (Eio.Stdenv.net env)
    @@ fun conn ->
    let conn =
      Tls_eio.client_of_flow
        tls_config
        ?host:(Domain_name.of_string_exn host |> Domain_name.host |> Result.ok)
        conn
    in
    let headers =
      Http.Header.of_list
        [ "Authorization", "Bearer " ^ api_key; "Content-Type", "application/json"; "Accept", "application/vnd.github+json"; "X-GitHub-Api-Version", "2022-11-28" ]
    in
  let response, _ = get ~conn ~port env ~host:url ~headers url in
  let content = read_fixed response in
  save_prompt path filename content

let rec download_files_aux env conn port files path =
  match files with
  | [] -> ()
  | (filename, url) :: rest ->
    download_file env conn port url path filename;
    download_files_aux env conn port rest path

let download_files  env path owner repo =
  let url = get_repo_contents_path owner repo path in
   Eio.Net.with_tcp_connect ~service:"https" ~host (Eio.Stdenv.net env)
    @@ fun conn ->
    let conn =
      Tls_eio.client_of_flow
        tls_config
        ?host:(Domain_name.of_string_exn host |> Domain_name.host |> Result.ok)
        conn
    in
  let response, _ = get ?conn ?port env ~host:github_api_base url in
  let files = parse_files_from_response response in
  download_files_aux env conn port files path
```

In this implementation, we first define the base URL for the GitHub API. Then, we create a function `get_repo_contents_url` to generate the URL for fetching the contents of a specific repository folder.

The `download_file` function takes the environment, connection, port, URL, path, and filename as arguments, and downloads the file using the `Client.get` function. It then reads the content of the response and saves it to the specified path using the `Doc.save_prompt` function.

The `download_files_aux` function is a helper function that recursively downloads all the files in the given list. It calls the `download_file` function for each file and then proceeds to the next file in the list.

Finally, the `download_files` function takes the optional connection and port, environment, owner, repo, and path as arguments. It generates the URL for the repository folder using the `get_repo_contents_url` function and fetches the contents using the `Client.get` function. It then parses the response to get the list of files and calls the `download_files_aux` function to download all the files.

Note that the implementation assumes the existence of a `parse_files_from_response` function that takes the response and returns a list of tuples containing the filename and URL for each file. You would need to implement this function based on the structure of the GitHub API response.


User:
Ok nice, that is a good start. So I checked github api docs and here is the curl command for getting repo content
```sh
curl -L \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer <YOUR-TOKEN>"\
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/OWNER/REPO/contents/PATH
  ```

the response schema is
```
{
  "title": "Content Tree",
  "description": "Content Tree",
  "type": "object",
  "properties": {
    "type": {
      "type": "string"
    },
    "size": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    },
    "path": {
      "type": "string"
    },
    "sha": {
      "type": "string"
    },
    "url": {
      "type": "string",
      "format": "uri"
    },
    "git_url": {
      "type": [
        "string",
        "null"
      ],
      "format": "uri"
    },
    "html_url": {
      "type": [
        "string",
        "null"
      ],
      "format": "uri"
    },
    "download_url": {
      "type": [
        "string",
        "null"
      ],
      "format": "uri"
    },
    "entries": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "type": {
            "type": "string"
          },
          "size": {
            "type": "integer"
          },
          "name": {
            "type": "string"
          },
          "path": {
            "type": "string"
          },
          "content": {
            "type": "string"
          },
          "sha": {
            "type": "string"
          },
          "url": {
            "type": "string",
            "format": "uri"
          },
          "git_url": {
            "type": [
              "string",
              "null"
            ],
            "format": "uri"
          },
          "html_url": {
            "type": [
              "string",
              "null"
            ],
            "format": "uri"
          },
          "download_url": {
            "type": [
              "string",
              "null"
            ],
            "format": "uri"
          },
          "_links": {
            "type": "object",
            "properties": {
              "git": {
                "type": [
                  "string",
                  "null"
                ],
                "format": "uri"
              },
              "html": {
                "type": [
                  "string",
                  "null"
                ],
                "format": "uri"
              },
              "self": {
                "type": "string",
                "format": "uri"
              }
            },
            "required": [
              "git",
              "html",
              "self"
            ]
          }
        },
        "required": [
          "_links",
          "git_url",
          "html_url",
          "download_url",
          "name",
          "path",
          "sha",
          "size",
          "type",
          "url"
        ]
      }
    },
    "_links": {
      "type": "object",
      "properties": {
        "git": {
          "type": [
            "string",
            "null"
          ],
          "format": "uri"
        },
        "html": {
          "type": [
            "string",
            "null"
          ],
          "format": "uri"
        },
        "self": {
          "type": "string",
          "format": "uri"
        }
      },
      "required": [
        "git",
        "html",
        "self"
      ]
    }
  },
  "required": [
    "_links",
    "git_url",
    "html_url",
    "download_url",
    "name",
    "path",
    "sha",
    "size",
    "type",
    "url",
    "content",
    "encoding"
  ]
}```

can you implement parse_files_from_response now given this res? We have access to the Jsonaf lib for parsing json in ocaml
with a interface 
```ocaml
sig
  type t = Jsonaf_kernel.t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val __t_of_sexp__ : Sexplib0.Sexp.t -> t
  val exactly_equal : t -> t -> bool
  val parse : string -> t Base.Or_error.t
  val parse_many : string -> t list Base.Or_error.t
  val of_string : string -> t
  val to_string : t -> string
  val to_string_hum : t -> string
  val pp : Formatter.t -> t -> unit

  module Jsonafable = Jsonaf.Jsonafable

  val t_of_jsonaf : Jsonaf_kernel__Type.t -> t
  val jsonaf_of_t : t -> Jsonaf_kernel__Type.t

  module Parser : sig
    val t : t Angstrom.t
    val run : string -> (t, string) result
  end

  module Serializer : sig
    val serialize : t -> Faraday.t -> unit
    val run : t -> string
  end

  val index : int -> t -> t option
  val index_exn : int -> t -> t
  val member : string -> t -> t option
  val member_exn : string -> t -> t
  val bool : t -> bool option
  val bool_exn : t -> bool
  val member_or_null : string -> t -> t
  val int : t -> int option
  val int_exn : t -> int
  val float : t -> float option
  val float_exn : t -> float
  val string : t -> string option
  val string_exn : t -> string
  val list : t -> t list option
  val list_exn : t -> t list
  val assoc_list : t -> (string * t) list option
  val assoc_list_exn : t -> (string * t) list
  val keys : t -> string list option
  val keys_exn : t -> string list

  module Export : Jsonaf_kernel.Conv.Primitives
end
```Given the response schema and the Jsonaf library, we can implement the `parse_files_from_response` function as follows:

```ocaml
(* Add this to github.ml *)

open Jsonaf

let parse_files_from_response response =
  let json = of_string response in
  match list json with
  | Some entries ->
    List.map
      (fun entry ->
        let filename = Option.value_exn (string (member_exn "name" entry)) in
        let url = Option.value_exn (string (member_exn "download_url" entry)) in
        (filename, url))
      entries
  | None -> []
```

In this implementation, we first parse the response string into a Jsonaf value using the `of_string` function. Then, we check if the JSON value is a list using the `list` function. If it is, we map over the list of entries and extract the "name" and "download_url" fields using the `member_exn` and `string` functions. We then create a tuple of the filename and URL for each entry and return the list of tuples. If the JSON value is not a list, we return an empty list.

Now, the `download_files` function in the Github module should be able to parse the response and download the files as expected.