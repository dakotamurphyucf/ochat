open Core
module Host = Chatml_git_host
module G = Chatml_git_catalog

module Mode = struct
  type t =
    | Regular
    | Symlink
    | Gitlink
  [@@deriving sexp_of]
end

module Interface = struct
  type t =
    | Present
    | Absent
    | Symlink
  [@@deriving sexp_of]
end

type 'a t =
  { repository : 'a Eio.Path.t
  ; old_pin : Host.Pin.t
  ; new_pin : Host.Pin.t
  }

let repository t = t.repository
let old_pin t = t.old_pin
let new_pin t = t.new_pin
let old_body = "(* exact\000bytes\r\n *)\nlet meaning = 41\n"
let new_body = "(* exact\000bytes\r\n *)\nlet meaning = 42\n"
let minimal_project = {|{"schema_version":1,"namespace":"minimal"}|}

let project ~namespace =
  {|{"schema_version":1,"namespace":"|}
  ^ namespace
  ^ {|","source_dirs":["."],"exclude":["skip.chatml"],"modules":{"Code":{"implementation":"code.chatml","interface":"code.chatmli"},"Alias":{"implementation":"code.chatml","interface":"code.chatmli"}},"entries":{"main":{"source":"code.chatml","target":"one_off_v1"}},"environments":{"own":{"modules":{"Own":"self.Code"}},"public":{}},"default_environment":"own","file_environments":{"code.chatmli":"public"}}|}
;;

let path text = Source.Portable_path.create text |> Or_error.ok_exn
let request = Chatml_project.Selection.Entry { name = "main"; environment = None }

let limits
      ?(objects = 128)
      ?(object_bytes = 65536)
      ?(total_object_bytes = 1048576)
      ?(entries = 1024)
      ?(sources = 128)
      ?(source_bytes = 65536)
      ?(total_source_bytes = 1048576)
      ?(components = 64)
      ?(work = 100000)
      ()
  =
  G.Limits.create
    ~max_object_bytes:object_bytes
    ~max_total_object_bytes:total_object_bytes
    ~max_objects:objects
    ~max_tree_entries:entries
    ~max_sources:sources
    ~max_source_bytes:source_bytes
    ~max_total_source_bytes:total_source_bytes
    ~max_path_components:components
    ~max_work_steps:work
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let reader ?(checkpoint = ignore) ?(limits = limits ()) repository ~f =
  G.Reader.with_repository repository ~limits ~checkpoint ~f
  |> Result.map_error ~f:(function `Reader e | `Callback e -> e)
;;

let declaration reader prefix =
  G.Reader.read_declaration
    reader
    ~package_root:(Some (path prefix))
    ~project:(path "project.chatml.json")
    ~namespace_override:None
;;

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected Git catalog rejection"
;;

let native_ok = function
  | Ok value -> value
  | Error (`Host error) -> raise_s [%sexp "Git host failed", (error : Host.Error.t)]
  | Error (`Callback _) -> failwith "Git catalog callback rejected"
;;

let create
      ?(source_mode = Mode.Regular)
      ?(ancestor_mode = Mode.Regular)
      ?(interface = Interface.Present)
      ?(namespace = "alpha")
      anchor
      ~(format : Host.Pin.object_format)
      ~git
  =
  let repository =
    Eio.Path.(
      anchor
      /
      match format with
      | Sha1 -> "sha1"
      | Sha256 -> "sha256")
  in
  Eio.Path.mkdir ~perm:0o700 repository;
  let run arguments stdin = git repository ~arguments ~stdin |> String.strip in
  ignore
    (run
       [ "init"
       ; "--bare"
       ; "--quiet"
       ; ("--object-format="
          ^
          match format with
          | Sha1 -> "sha1"
          | Sha256 -> "sha256")
       ]
       "");
  let blob body = run [ "hash-object"; "-w"; "--stdin" ] body in
  let tree entries =
    run
      [ "mktree" ]
      (String.concat
         ~sep:""
         (List.map entries ~f:(fun (mode, kind, id, name) ->
            mode ^ " " ^ kind ^ " " ^ id ^ "\t" ^ name ^ "\n")))
  in
  let empty = tree [] in
  let seed = run [ "commit-tree"; empty ] "seed\n" in
  let old_code = blob old_body
  and new_code = blob new_body in
  let interface_blob = blob "val meaning : int\n" in
  let link = blob "../not-followed" in
  let attributes = blob "*.chatml filter=forbidden\n" in
  let hidden =
    tree [ "100644", "blob", blob "never recursively discovered", "hidden.chatml" ]
  in
  let package namespace code =
    let mode, kind, code =
      match source_mode with
      | Mode.Regular -> "100755", "blob", code
      | Symlink -> "120000", "blob", link
      | Gitlink -> "160000", "commit", seed
    in
    let manifest = project ~namespace in
    let manifest =
      match interface with
      | Interface.Present -> manifest
      | Absent | Symlink ->
        String.substr_replace_all
          manifest
          ~pattern:",\"interface\":\"code.chatmli\""
          ~with_:""
    in
    let companion =
      match interface with
      | Interface.Present -> [ "100644", "blob", interface_blob, "code.chatmli" ]
      | Absent -> []
      | Symlink -> [ "120000", "blob", link, "code.chatmli" ]
    in
    tree
      (companion
       @ [ "100644", "blob", attributes, ".gitattributes"
         ; mode, kind, code, "code.chatml"
         ; "120000", "blob", link, "notes.txt"
         ; "100644", "blob", blob manifest, "project.chatml.json"
         ; "120000", "blob", link, "skip.chatml"
         ; "040000", "tree", hidden, "subdir"
         ; "160000", "commit", seed, "vendor"
         ])
  in
  let beta = package "beta" old_code in
  let root code =
    let mode, kind, member =
      match ancestor_mode with
      | Mode.Regular -> "040000", "tree", package namespace code
      | Symlink -> "120000", "blob", link
      | Gitlink -> "160000", "commit", seed
    in
    tree [ mode, kind, member, "pkg"; "040000", "tree", beta, "other" ]
  in
  let first = run [ "commit-tree"; root old_code ] "first\n" in
  let second = run [ "commit-tree"; root new_code; "-p"; first ] "second\n" in
  ignore (run [ "update-ref"; "refs/heads/main"; first ] "");
  ignore (run [ "symbolic-ref"; "HEAD"; "refs/heads/main" ] "");
  let marker = Eio.Path.(repository / "filter-ran") in
  ignore
    (run [ "config"; "filter.forbidden.smudge"; "touch " ^ Support.native marker ] "");
  { repository
  ; old_pin = Host.Pin.create ~object_format:format ~commit:first |> Or_error.ok_exn
  ; new_pin = Host.Pin.create ~object_format:format ~commit:second |> Or_error.ok_exn
  }
;;

let move_to_new t ~git =
  ignore
    (git
       t.repository
       ~arguments:[ "update-ref"; "refs/heads/main"; Host.Pin.commit t.new_pin ]
       ~stdin:"")
;;
