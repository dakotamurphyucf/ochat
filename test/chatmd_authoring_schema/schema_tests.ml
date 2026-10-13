open! Core
module Scope = Chatmd_tag_scope
module E = Chatmd_shell_spec.Shell_element
module Schema = Chatmd_declaration_schema
module Shell = Chatmd_shell_schema

let enter scope tag attributes =
  match Scope.enter scope ~tag ~attributes ~max_depth:16 ~checkpoint:(fun () -> ()) with
  | Ok scope -> scope
  | Error Scope.Error.Depth_limit -> failwith "test scope depth"
;;

let shell_context root ancestors element =
  let scope = enter Scope.root root [] in
  let scope =
    List.fold ancestors ~init:scope ~f:(fun scope element ->
      enter scope (Chatmd_ast.Shell_element element) [])
  in
  Shell.Context.of_scope scope ~element ~checkpoint:(fun () -> ())
;;

let selected_names root ancestors element =
  Option.bind (shell_context root ancestors element) ~f:Shell.allowed
;;

let%expect_test "actual ancestors select distinct shell validator attributes" =
  let cases =
    [ "capability read", Chatmd_ast.Shell_access, [ E.Capabilities ], E.Read
    ; "resolver executable", Shell_access, [ E.Resolver ], E.Executable
    ; "environment path", Shell_access, [ E.Environment ], E.Path
    ; "environment path-env", Shell_access, [ E.Environment ], E.Path_env
    ; "backend atom", Shell_access, [ E.Backends; E.External_backend ], E.Arg
    ; "orphan backend atom", Shell_access, [ E.Backends ], E.Arg
    ; "policy matcher", Shell_access, [ E.Policy; E.Rule ], E.Program
    ; "nested policy matcher", Shell_access, [ E.Policy; E.Rule; E.All ], E.Program
    ; "orphan policy matcher", Shell_access, [ E.Policy ], E.Program
    ; ( "interceptor matcher"
      , Shell_access
      , [ E.Interceptors; E.Interceptor; E.Match ]
      , E.Program )
    ; ( "orphan interceptor matcher"
      , Shell_access
      , [ E.Interceptors; E.Interceptor ]
      , E.Program )
    ; "secret file", Shell_access, [ E.Secrets ], E.From_file
    ]
  in
  List.iter cases ~f:(fun (label, root, ancestors, element) ->
    print_s
      [%sexp
        (label : string), (selected_names root ancestors element : string list option)]);
  [%expect
    {|
    ("capability read" ((path path_env optional)))
    ("resolver executable" ((id path sha256 trusted override)))
    ("environment path" ((prepend append)))
    ("environment path-env" ((name suffix position required)))
    ("backend atom" ((value)))
    ("orphan backend atom" ())
    ("policy matcher" ((value)))
    ("nested policy matcher" ((value)))
    ("orphan policy matcher" ())
    ("interceptor matcher" ((value)))
    ("orphan interceptor matcher" ())
    ("secret file" ((path optional strip)))
    |}]
;;

let%expect_test "lexical root route and unknown tag placement" =
  let scope = enter Scope.root Chatmd_ast.Tool [ "type", Some "shell" ] in
  let shell =
    Shell.Context.of_scope scope ~element:E.Command ~checkpoint:(fun () -> ())
  in
  let extension_scope = enter Scope.root Tool [ "type", Some "chatml" ] in
  let extension =
    Shell.Context.of_scope extension_scope ~element:E.Command ~checkpoint:(fun () -> ())
  in
  let read_scope = enter Scope.root Tool [ "name", Some "read_file" ] in
  let read =
    Shell.Context.of_scope read_scope ~element:E.Read ~checkpoint:(fun () -> ())
  in
  print_s
    [%sexp
      (Option.bind shell ~f:Shell.allowed : string list option)
    , (Option.bind extension ~f:Shell.allowed : string list option)
    , (Option.bind read ~f:Shell.allowed : string list option)
    , (Scope.recognize Scope.root ~name:"unknown" : Chatmd_ast.tag option)
    , (Scope.recognize Scope.root ~name:"doc" : Chatmd_ast.tag option)];
  [%expect
    {|
    (((program executable_ref)) () ((id path description)) () (Doc))
    |}]
;;

let%expect_test "contextual suggestions agree with strict parser acceptance" =
  let source =
    let position = Chatmd_shell_spec.Source_ref.{ offset = 0; line = 1; column = 0 } in
    Chatmd_shell_spec.Source_ref.create
      ~file:"schema.chatmd"
      ~source_dir:"."
      ~prompt_dir:"."
      ~namespace:None
      ~start_pos:position
      ~end_pos:position
      ~source:"schema fixture"
  in
  let element tag attributes children = Chatmd_ast.Element (tag, attributes, children) in
  let nested tag attributes children =
    element (Chatmd_ast.Shell_element tag) attributes children
  in
  let root child = element Shell_access [ "id", Some "context" ] [ child ] in
  let cases =
    [ "capability enum accepted", nested E.Capabilities [ "sandbox", Some "preferred" ] []
    ; "capability enum rejected", nested E.Capabilities [ "sandbox", Some "bogus" ] []
    ; "capability bool accepted", nested E.Capabilities [ "network", Some "true" ] []
    ; "capability bool rejected", nested E.Capabilities [ "network", Some "bogus" ] []
    ; ( "environment path accepted"
      , nested E.Environment [] [ nested E.Path [ "prepend", Some "/tmp" ] [] ] )
    ; ( "environment path rejects backend attr"
      , nested E.Environment [] [ nested E.Path [ "flag", Some "x" ] [] ] )
    ; ( "policy matcher accepted"
      , nested
          E.Policy
          []
          [ nested
              E.Rule
              [ "id", Some "allow"; "action", Some "allow" ]
              [ nested E.Program [ "value", Some "rg" ] [] ]
          ] )
    ; ( "policy matcher rejects environment attr"
      , nested
          E.Policy
          []
          [ nested
              E.Rule
              [ "id", Some "allow"; "action", Some "allow" ]
              [ nested E.Program [ "prepend", Some "/tmp" ] [] ]
          ] )
    ; ( "reviewer kind accepted"
      , nested
          E.Reviewers
          []
          [ nested E.Reviewer [ "id", Some "r"; "kind", Some "ui" ] [] ] )
    ; ( "reviewer kind rejected"
      , nested
          E.Reviewers
          []
          [ nested E.Reviewer [ "id", Some "r"; "kind", Some "bogus" ] [] ] )
    ]
  in
  List.iter cases ~f:(fun (label, child) ->
    let accepted =
      Chatmd_shell_declaration.parse_runtime ~source (root child) |> Result.is_ok
    in
    print_s [%sexp (label : string), (accepted : bool)]);
  let tool_mode mode =
    Chatmd_shell_spec.Shell_tool_spec.parse_compact_attributes
      ~source
      [ "name", "t"; "type", "shell"; "runtime", "context"; "mode", mode ]
    |> Result.is_ok
  in
  print_s [%sexp (tool_mode "structured" : bool), (tool_mode "bogus" : bool)];
  [%expect
    {|
    ("capability enum accepted" true)
    ("capability enum rejected" false)
    ("capability bool accepted" true)
    ("capability bool rejected" false)
    ("environment path accepted" true)
    ("environment path rejects backend attr" false)
    ("policy matcher accepted" true)
    ("policy matcher rejects environment attr" false)
    ("reviewer kind accepted" true)
    ("reviewer kind rejected" false)
    (true false)
    |}]
;;

let%expect_test "contextual finite values use parser mappings" =
  let choices root ancestors element attribute =
    shell_context root ancestors element
    |> Option.bind ~f:(fun context -> Shell.choices context ~attribute)
  in
  print_s
    [%sexp
      (choices Shell_access [] E.Capabilities "sandbox" : Shell.Value_choices.t option)
    , (choices Shell_access [] E.Capabilities "network" : Shell.Value_choices.t option)
    , (choices Shell_access [ E.Resolver ] E.Executable "trusted"
       : Shell.Value_choices.t option)
    , (choices Shell_access [ E.Policy; E.Rule ] E.Effect "name"
       : Shell.Value_choices.t option)
    , (choices Shell_access [] E.Approvals "scopes" : Shell.Value_choices.t option)
    , (choices Shell_access [ E.Audit ] E.Filter "failure" : Shell.Value_choices.t option)
    , (choices Shell_access [ E.Interceptors ] E.Interceptor "failure"
       : Shell.Value_choices.t option)
    , (choices Shell_access [ E.Reviewers ] E.Reviewer "kind"
       : Shell.Value_choices.t option)];
  [%expect
    {|
    (((Closed (required preferred direct_unsafe))) ((Closed (true false)))
     ((Closed (true false)))
     ((Closed
       (read_path write_path network child_processes arbitrary_code
        privilege_change unknown)))
     ((Comma_separated_tokens (once exact_session prefix_session durable_exact)))
     () ((Closed (deny error no_match unknown keep)))
     ((Closed (ui chatml model executable))))
    |}]
;;

let family tag attributes =
  Schema.family_of_complete ~tag ~attributes |> Schema.Family.sexp_of_t |> Sexp.to_string
;;

let%expect_test "complete parser routes and unfinished family choices" =
  let cases =
    [ "legacy script", Chatmd_ast.Script, [ "kind", Some "moderator" ]
    ; ( "extension script"
      , Script
      , [ "api", Some "extensibility-v1"; "kind", Some "moderator" ] )
    ; "tool script", Script, [ "kind", Some "tool" ]
    ; "legacy tool", Tool, [ "name", Some "custom" ]
    ; "shell tool", Tool, [ "type", Some "shell" ]
    ; "extension tool", Tool, [ "type", Some "chatml" ]
    ; "read-file tool", Tool, [ "name", Some " read_file " ]
    ; "inherited tool", Tool, [ "type", Some "inherited" ]
    ]
  in
  List.iter cases ~f:(fun (label, tag, attributes) ->
    print_s [%sexp (label : string), (family tag attributes : string)]);
  [%expect
    {|
    ("legacy script" Script_legacy)
    ("extension script" Script_extension)
    ("tool script" Script_extension)
    ("legacy tool" Tool_legacy)
    ("shell tool" Tool_shell)
    ("extension tool" Tool_extension)
    ("read-file tool" Tool_read_file)
    ("inherited tool" Tool_inherited)
    |}]
;;

(* Frozen reference of Prompt's former routing, before the production call
   sites were redirected to Schema.family_of_complete. This is intentionally
   independent of the new predicate helpers. *)
let reference_family tag attributes =
  let has (name, value) =
    List.exists attributes ~f:(fun attribute ->
      match name, value, attribute with
      | name, None, (candidate, _) -> String.equal name candidate
      | name, Some value, (candidate, Some actual) ->
        String.equal name candidate && String.equal value actual
      | _, Some _, (_, None) -> false)
  in
  match tag with
  | Chatmd_ast.Script ->
    if has ("api", None) || has ("kind", Some "tool")
    then Schema.Family.Script_extension
    else Script_legacy
  | Tool ->
    let last = Hashtbl.create (module String) in
    List.iter attributes ~f:(fun (name, value) ->
      Hashtbl.set last ~key:name ~data:(Option.value value ~default:""));
    if has ("type", Some "inherited")
    then Tool_inherited
    else if has ("type", Some "moderator") || has ("type", Some "chatml")
    then Tool_extension
    else if
      Option.value_map (Hashtbl.find last "type") ~default:false ~f:(String.equal "shell")
      || Hashtbl.mem last "runtime"
    then Tool_shell
    else if
      List.for_all [ "command"; "agent"; "mcp_server" ] ~f:(fun name ->
        not (Hashtbl.mem last name))
      && Option.value_map (Hashtbl.find last "name") ~default:false ~f:(fun name ->
        let name = String.strip name in
        String.equal name "read_file" || String.equal name "get_contents")
    then Tool_read_file
    else Tool_legacy
  | _ -> Plain_tag tag
;;

let%expect_test "bounded complete-route reference matrix" =
  let script_kind =
    [ []
    ; [ "kind", Some "moderator" ]
    ; [ "kind", Some "tool" ]
    ; [ "kind", Some "shell_matcher" ]
    ; [ "kind", None ]
    ]
  in
  let script_api =
    [ []; [ "api", None ]; [ "api", Some "extensibility-v1" ]; [ "api", Some "bad" ] ]
  in
  let tool_type =
    [ []
    ; [ "type", Some "inherited" ]
    ; [ "type", Some "moderator" ]
    ; [ "type", Some "chatml" ]
    ; [ "type", Some "shell" ]
    ; [ "type", Some "other" ]
    ; [ "type", Some "shell"; "type", Some "moderator" ]
    ]
  in
  let tool_name =
    [ []
    ; [ "name", Some "read_file" ]
    ; [ "name", Some " get_contents " ]
    ; [ "name", Some "custom" ]
    ]
  in
  let tool_extra =
    [ []
    ; [ "runtime", Some "r" ]
    ; [ "command", Some "run" ]
    ; [ "agent", Some "a" ]
    ; [ "mcp_server", Some "m" ]
    ]
  in
  let cases =
    List.concat_map script_kind ~f:(fun kind ->
      List.map script_api ~f:(fun api -> Chatmd_ast.Script, kind @ api))
    @ List.concat_map tool_type ~f:(fun type_ ->
      List.concat_map tool_name ~f:(fun name ->
        List.map tool_extra ~f:(fun extra -> Chatmd_ast.Tool, type_ @ name @ extra)))
  in
  let mismatches =
    List.filter cases ~f:(fun (tag, attributes) ->
      not
        (Schema.Family.equal
           (Schema.family_of_complete ~tag ~attributes)
           (reference_family tag attributes)))
  in
  print_s [%sexp (List.length cases : int), (List.length mismatches : int)];
  [%expect {| (160 0) |}]
;;

let%expect_test "closed discriminators, legacy duplicates and bounded prefix work" =
  let classify tag attributes ~uncertain =
    Schema.families_of_prefix
      ~scope:Scope.root
      ~tag
      ~completed_attributes:attributes
      ~uncertain_discriminator:uncertain
      ~checkpoint:(fun () -> ())
    |> Schema.Prefix.sexp_of_t
    |> print_s
  in
  classify Script [ "kind", Some "unknown" ] ~uncertain:false;
  classify Script [ "api", Some "unknown" ] ~uncertain:false;
  classify Script [ "kind", Some "unknown" ] ~uncertain:true;
  classify Tool [ "name", Some "x"; "name", Some "y" ] ~uncertain:false;
  classify Tool [ "type", Some "chatml"; "type", Some "shell" ] ~uncertain:false;
  classify Doc [ "src", Some "a"; "src", Some "b" ] ~uncertain:false;
  classify Import [ "src", Some "a"; "src", Some "b" ] ~uncertain:false;
  classify Script [ "id", Some (String.make 4_097 'x') ] ~uncertain:false;
  classify Script (List.init 129 ~f:(fun _ -> "id", Some "x")) ~uncertain:false;
  [%expect
    {|
    (Malformed (Invalid_complete_discriminator kind))
    (Malformed (Invalid_complete_discriminator api))
    (Uncertain (Script_legacy Script_extension))
    (Uncertain
     (Tool_legacy Tool_extension Tool_shell Tool_read_file Tool_inherited))
    (Uncertain
     (Tool_legacy Tool_extension Tool_shell Tool_read_file Tool_inherited))
    (Uncertain ((Plain_tag Doc)))
    (Malformed (Duplicate_attribute src))
    Limit_exceeded
    Limit_exceeded
    |}]
;;

let%expect_test "unfinished read-file tool keeps valid continuation families" =
  let prefix = [ "name", Some "read_file" ] in
  let result =
    Schema.families_of_prefix
      ~scope:Scope.root
      ~tag:Tool
      ~completed_attributes:prefix
      ~uncertain_discriminator:false
      ~checkpoint:(fun () -> ())
  in
  let candidates =
    Schema.attributes_for_prefix
      result
      ~completed_attributes:prefix
      ~checkpoint:(fun () -> ())
    |> List.map ~f:Schema.Attribute_candidate.name
  in
  let continuations =
    [ [ "command", Some "run" ]
    ; [ "agent", Some "assistant" ]
    ; [ "mcp_server", Some "server" ]
    ; [ "type", Some "inherited" ]
    ; [ "type", Some "chatml" ]
    ; [ "type", Some "shell" ]
    ]
  in
  let families =
    match result with
    | Schema.Prefix.Viable families | Uncertain families -> families
    | Malformed _ | Limit_exceeded -> []
  in
  let routes_present =
    List.for_all continuations ~f:(fun suffix ->
      List.mem
        families
        (Schema.family_of_complete ~tag:Tool ~attributes:(prefix @ suffix))
        ~equal:Schema.Family.equal)
  in
  print_s
    [%sexp
      (routes_present : bool)
    , (List.for_all
         [ "command"; "agent"; "mcp_server"; "type" ]
         ~f:(List.mem candidates ~equal:String.equal)
       : bool)];
  [%expect {| (true true) |}]
;;
