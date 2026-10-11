open Core
open Omd
module B = Conversion_budget

(******************************************************************************)
(* Helpers                                                                    *)
(******************************************************************************)

let ocaml_regexs =
  [ (* let <name> = <expr> *)
    Re2.create "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* let rec <name> = <expr> *)
    Re2.create "let\\s+rec\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* module <name> = struct ... end *)
    Re2.create "module\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*struct.*end"
  ; (* type <name> = <expr> *)
    Re2.create "type\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* exception <name> ( <args> ) -> <result> *)
    Re2.create "exception\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)\\s*->"
  ; Re2.create "type\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* let <name> = <expr> in *)
    Re2.create "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)\\s*in"
  ; (* let <name> = <expr> and <name2> = <expr2> *)
    Re2.create
      "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)\\s*and\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* let <name> = <expr> in <expr2> *)
    Re2.create "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)\\s*in\\s*\\(.*\\)"
  ; (* match <pattern> with <cases> *)
    Re2.create "match\\s+\\(.*\\)\\s+with\\s+\\(.*\\)"
  ; (* if <condition> then <expr> else <expr2> *)
    Re2.create "if\\s+\\(.*\\)\\s+then\\s+\\(.*\\)\\s+else\\s+\\(.*\\)"
  ; (* let <name> = fun <args> -> <expr> *)
    Re2.create "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)"
  ; (* let <name> = fun <args> -> <expr> in *)
    Re2.create
      "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)\\s*in"
  ; (* let <name> = fun <args> -> <expr> and <name2> = fun <args2> -> <expr2> *)
    Re2.create
      "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)\\s*and\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)"
  ; (* let rec <name> = fun <args> -> <expr> in *)
    Re2.create
      "let\\s+rec\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)\\s*in"
  ; (* let rec <name> = fun <args> -> <expr> and <name2> = fun <args2> -> <expr2> *)
    Re2.create
      "let\\s+rec\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)\\s*and\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*fun\\s+\\(.*\\)\\s*->\\s*\\(.*\\)"
  ; (* let rec <name> = function | <pattern> -> <expr> *)
    Re2.create
      "let\\s+rec\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*function\\s+|\\s+\\(.*\\)\\s+->\\s+\\(.*\\)"
  ; (* type <name> = | <case> | <case2>  *)
    Re2.create "type\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*|\\s+\\(.*\\)\\s+|\\s+\\(.*\\)"
  ; (* type <name> =
              | <case>
              | <case2>
              | ... *)
    Re2.create
      "type\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*|\\s+\\(.*\\)\\s+|\\s+\\(.*\\)\\s+|\\s+\\(.*\\)"
  ; (* module <name> = struct ... end *)
    Re2.create "module\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*struct.*end"
  ; (* module <name> = struct ... end *)
    Re2.create "module\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*struct.*end"
  ; (* open <module> *)
    Re2.create "open\\s+[a-zA-Z_][a-zA-Z0-9_.']*"
  ; (* include <module> *)
    Re2.create "include\\s+[a-zA-Z_][a-zA-Z0-9_.']*"
  ; (* let open <module> in *)
    Re2.create "let\\s+open\\s+[a-zA-Z_][a-zA-Z0-9_.']*\\s+in"
  ]
;;

let python_regexs =
  [ (* def <name>(<args>): *)
    Re2.create "def\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\):"
  ; (* class <name>: *)
    Re2.create "class\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*:\\s*"
  ; (* class <name>(): *)
    Re2.create "class\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\):"
  ; (* class <name>(<params>): *)
    Re2.create "class\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\):"
  ; (* class <name>(<params>): *)
    Re2.create "class\\s+[A-Z][a-zA-Z0-9_']*\\s*\\(.*\\):"
  ; (* import <module> *)
    Re2.create "import\\s+[a-zA-Z_][a-zA-Z0-9_.]*"
  ; (* from <module> import <name> *)
    Re2.create "from\\s+[a-zA-Z_][a-zA-Z0-9_.]*\\s+import\\s+[a-zA-Z_][a-zA-Z0-9_.]*"
  ; (* if <condition>: *)
    Re2.create "if\\s+\\(.*\\):"
  ; (* for <var> in <iterable>: *)
    Re2.create "for\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s+in\\s+\\(.*\\):"
  ; (* while <condition>: *)
    Re2.create "while\\s+\\(.*\\):"
  ; (* try: *)
    Re2.create "try:\\s*"
  ; (* except <exception>: *)
    Re2.create "except\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*:\\s*"
  ; (* finally: *)
    Re2.create "finally:\\s*"
  ; (* with <exception> as <var>: *)
    Re2.create "with\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s+as\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*:\\s*"
  ; (* with <Class> as <var>: *)
    Re2.create "with\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s+as\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*:\\s*"
  ; (* with <Class> as <var>: *)
    Re2.create "with\\s+[A-Z][a-zA-Z0-9_']*\\s+as\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*:\\s*"
  ; (* lambda <args>: *)
    Re2.create "lambda\\s+\\(.*\\)\\s*:\\s*"
  ; (* async def <name>(<args>): *)
    Re2.create "async\\s+def\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\):"
  ; (* with <class>() as <var>: *)
    Re2.create
      "with\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)\\s+as\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*:\\s*"
  ; (* <var> = <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* <var> += <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*+=\\s*\\(.*\\)"
  ; (* <var> -= <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*-=\\s*\\(.*\\)"
  ; (* <var> *= <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* <var> /= <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*/=\\s*\\(.*\\)"
  ; (* return <expr> *)
    Re2.create "return\\s+\\(.*\\)"
  ; (* for <var> in <iterable> *)
    Re2.create "for\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s+in\\s+\\(.*\\)\\s*:"
  ; (* for <var> in <name>(params): *)
    Re2.create
      "for\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s+in\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)\\s*:"
  ; (* for <var> in <name>.<method>(params): *)
    Re2.create
      "for\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s+in\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)\\s*:"
  ; (* <var> = <name>(params) *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)"
  ; (* <var> = <name>.<method>(params) *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)"
  ; (* <var> = <name>.<method>....<expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)"
  ; (* <var> = [{key: val}, ...]  || <var> = <name>([..] | {...})*)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(\\[.*\\]\\|{.*}\\)"
  ; (* [{key: val}, ...] |  <name>([..] | {...}) | {...} *)
    Re2.create "\\(\\[.*\\]\\|{.*}\\)\\|[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(\\[.*\\]\\|{.*}\\)"
  ]
;;

let javascript_regexs =
  [ (* function <name>(<args>) { ... } *)
    Re2.create "function\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)\\s*{"
  ; (* const <name> = <expr>; *)
    Re2.create "const\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\);"
  ; (* let <name> = <expr>; *)
    Re2.create "let\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\);"
  ; (* var <name> = <expr>; *)
    Re2.create "var\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\);"
  ; (* if (<condition>) { ... } *)
    Re2.create "if\\s+\\(.*\\)\\s+{"
  ; (* for (<init>; <condition>; <increment>) { ... } *)
    Re2.create "for\\s+\\(.*;.*;.*\\)\\s+{"
  ; (* while (<condition>) { ... } *)
    Re2.create "while\\s+\\(.*\\)\\s+{"
  ; (* switch (<expr>) { ... } *)
    Re2.create "switch\\s+\\(.*\\)\\s+{"
  ; (* try { ... } catch (<exception>) { ... } *)
    Re2.create "try\\s+{.*}\\s+catch\\s+\\(.*\\)\\s+{"
  ; (* async function <name>(<args>) { ... } *)
    Re2.create "async\\s+function\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*\\(.*\\)\\s*{"
  ; (* arrow function <name> = (<args>) => { ... } *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)\\s*=>\\s*{"
  ; (* <var> = <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* <var> += <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*+=\\s*\\(.*\\)"
  ; (* <var> -= <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*-=\\s*\\(.*\\)"
  ; (* <var> *= <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*\\(.*\\)"
  ; (* <var> /= <expr> *)
    Re2.create "[a-zA-Z_][a-zA-Z0-9_']*\\s*/=\\s*\\(.*\\)"
  ; (* return <expr> *)
    Re2.create "return\\s+\\(.*\\)"
  ; (* var <name> = { <key>: <value>, ... }; *)
    Re2.create "var\\s+[a-zA-Z_][a-zA-Z0-9_']*\\s*=\\s*{.*};"
  ]
;;

let json_regex =
  [ (* JSON key-value pair *)
    Re2.create "\"[^\"]*\"\\s*:\\s*"
  ; (* json object with key-value pairs *)
    Re2.create "{\\s*(\"[^\"]*\"\\s*:\\s*.*?\\s*,?\\s*)*}"
  ; (* json array with values *)
    Re2.create "\\[\\s*(\"[^\"]*\"\\s*,?\\s*)*\\]"
  ]
;;

type attr = (string * string) list

(* Short aliases for constructors to keep code readable *)
let txt budget s =
  B.nodes budget 2;
  Text ([], s)
;;

let concat budget lst =
  B.nodes budget 2;
  Concat ([], lst)
;;

(* Collapse runs of whitespace to a single space (HTML rendering rule) *)
let collapse_whitespace budget s =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      B.scan budget s;
      let len = String.length s in
      let buf = Buffer.create len in
      let rec loop i last_ws =
        if i = len
        then ()
        else (
          B.work budget 1;
          match s.[i] with
          | ' ' | '\n' | '\t' | '\r' ->
            if not last_ws then Buffer.add_char buf ' ';
            loop (i + 1) true
          | c ->
            Buffer.add_char buf c;
            loop (i + 1) false)
      in
      loop 0 false;
      Buffer.contents buf)
;;

(******************************************************************************)
(* Noise filtering helpers                                                    *)
(******************************************************************************)

(* Heuristic: text node likely CSS / style junk *)
let is_css_noise budget (s : string) : bool =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      B.scan budget s;
      B.is_substring budget s ~substring:"/*!sc*/"
      ||
      let len = String.length s in
      len > 400 && not (String.exists s ~f:Char.is_whitespace))
;;

(* Remove script/style/chrome and GitHub UI noise in-place *)
let prune_noise budget (soup : _ Soup.node) : unit =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 64;
      (* 1. Obvious non-content tags *)
      let noise_tags =
        [ "script"; "style"; "noscript"; "template"; "svg"; "canvas"; "iframe" ]
      in
      (* Avoid CSS selector engine entirely: traverse descendants and match tag *)
      let nodes_by_tag (root : _ Soup.node) (tag : string) : _ Soup.node list =
        Soup.descendants root
        |> B.soup_nodes budget
        |> B.filter budget ~f:(fun n ->
          match Soup.element n with
          | Some el -> String.equal (Soup.name el) tag
          | None -> false)
      in
      B.iter budget noise_tags ~f:(fun tag ->
        nodes_by_tag soup tag |> B.iter budget ~f:(B.soup_delete budget));
      (* 2. Site chrome tags *)
      [ "header"; "nav"; "footer"; "aside" ]
      |> B.iter budget ~f:(fun tag ->
        nodes_by_tag soup tag
        |> B.iter budget ~f:(fun node ->
          match tag with
          | "header" ->
            (match
               B.soup_attribute budget "class" (Soup.element node |> Option.value_exn)
             with
             | Some cls when B.is_substring budget cls ~substring:"odoc-preamble" -> ()
             | _ -> B.soup_delete budget node)
          | _ -> B.soup_delete budget node));
      (* 3. Heuristic class/id substrings *)
      let junk_subs = [ "octicon"; "tooltipped"; "sr-only"; "Tooltip__" ] in
      let attr_is_junk = function
        | None -> false
        | Some v ->
          B.exists budget junk_subs ~f:(fun sub -> B.is_substring budget v ~substring:sub)
      in
      (* Using [Soup.descendants] instead of the CSS selector "*" avoids potential
     crashes in the selector parser on some pathological pages where the CSS
     selector may get confused. [descendants] enumerates the full tree in a
     safe way. *)
      Soup.descendants soup
      |> B.soup_nodes budget
      |> B.iter budget ~f:(fun node ->
        let el_opt = Soup.element node in
        match el_opt with
        | None -> ()
        | Some el ->
          if
            attr_is_junk (B.soup_attribute budget "class" el)
            || attr_is_junk (B.soup_attribute budget "id" el)
          then B.soup_delete budget node))
;;

(* 4. If GitHub/GitLab style rendered markdown article exists, keep only that *)
let isolate_main_content budget (soup : _ Soup.node) : _ Soup.node =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      let article =
        Soup.descendants soup
        |> B.soup_nodes budget
        |> B.find_map budget ~f:(fun n ->
          match Soup.element n with
          | Some el when String.equal (Soup.name el) "article" ->
            (match B.soup_attribute budget "class" el with
             | Some cls when B.is_substring budget cls ~substring:"markdown-body" ->
               Some (Soup.coerce el)
             | _ -> None)
          | _ -> None)
      in
      match article with
      | Some art -> Soup.coerce art
      | None -> soup)
;;

(******************************************************************************)
(* Inline conversion                                                          *)
(******************************************************************************)

(* Forward declarations to allow mutual recursion *)
let rec inline_of_node budget node =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      match Soup.element node with
      | None ->
        let raw =
          B.soup_texts budget node
          |> B.concat budget ~sep:""
          |> collapse_whitespace budget
        in
        if String.is_empty (B.strip budget raw) || is_css_noise budget raw
        then []
        else [ txt budget raw ]
      | Some el ->
        (match Soup.name el with
         | "strong" | "b" -> [ Strong ([], concat budget (inline_children budget el)) ]
         | "em" | "i" -> [ Emph ([], concat budget (inline_children budget el)) ]
         | "code" ->
           let code =
             B.soup_texts budget el |> B.concat budget ~sep:"" |> B.strip budget
           in
           [ Code ([], code) ]
         | "a" ->
           let href = Option.value ~default:"" (B.soup_attribute budget "href" el) in
           let label_children = inline_children budget el in
           (* Detect anchors with no visible text: if the concatenated children are
             empty (after stripping whitespace), skip emitting the link to avoid
             artifacts like "[](#anchor)". *)
           let is_empty_inline lst =
             B.for_all budget lst ~f:(function
               | Text (_, s) -> String.is_empty (B.strip budget s)
               | Concat (_, inner) ->
                 let rec flat l =
                   B.for_all budget l ~f:(function
                     | Text (_, s) -> String.is_empty (B.strip budget s)
                     | Concat (_, sub) -> flat sub
                     | _ -> false)
                 in
                 flat inner
               | Soft_break _ | Hard_break _ -> true
               | Html _ -> true
               | _ -> false)
           in
           if String.is_prefix href ~prefix:"#" && is_empty_inline label_children
           then label_children
           else if is_empty_inline label_children
           then label_children
           else (
             let label = concat budget label_children in
             let link = { label; destination = href; title = None } in
             [ Link ([], link) ])
         | "img" ->
           let src = Option.value ~default:"" (B.soup_attribute budget "src" el) in
           let alt = Option.value ~default:"" (B.soup_attribute budget "alt" el) in
           let link = { label = txt budget alt; destination = src; title = None } in
           [ Image ([], link) ]
         | "br" -> [ Soft_break [] ]
         | _ -> inline_children budget el))

and inline_children budget el =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      Soup.children el
      |> B.soup_nodes budget
      |> B.concat_map budget ~f:(inline_of_node budget))
;;

(******************************************************************************)
(* Block conversion                                                           *)
(******************************************************************************)

(* Safely convert a node to blocks, swallowing any Soup.Parse_error or other
   parsing exceptions so that one malformed subtree does not abort the entire
   document.  We mutually recurse with [block_of_node] defined just below. *)
let rec safe_block_of_node budget (node : _ Soup.node) =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      if B.is_bounded budget
      then block_of_node budget node
      else (
        try block_of_node budget node with
        | Soup.Parse_error _ | Failure _ | Invalid_argument _ -> []))

and block_of_node budget (node : _ Soup.node) =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 64;
      match Soup.element node with
      | None ->
        let txt_raw =
          B.soup_texts budget node
          |> B.concat budget ~sep:""
          |> collapse_whitespace budget
          |> B.strip budget
        in
        if String.is_empty txt_raw || is_css_noise budget txt_raw
        then []
        else [ Paragraph ([], txt budget txt_raw) ]
      | Some el ->
        (match Soup.name el with
         | tag when String.is_prefix tag ~prefix:"h" ->
           let lvl =
             try Int.of_string (String.drop_prefix tag 1) with
             | _ -> 1
           in
           let inls = concat budget (inline_children budget el) in
           [ Heading ([], Int.min 6 lvl, inls) ]
         | "p" -> [ Paragraph ([], concat budget (inline_children budget el)) ]
         | "hr" -> [ Thematic_break [] ]
         | "blockquote" ->
           let inner = children_blocks budget el in
           [ Blockquote ([], inner) ]
         | "pre" ->
           (* Attempt to infer a language identifier for syntax highlighting.  We
          check the [class] attribute of the <pre> element itself as well as
          the first nested <code> tag (a common pattern in many HTML
          renderers).  When a known language cannot be detected we fall back
          to ["ocaml"].  The heuristic purposefully stays lightweight: only
          a handful of language substrings are matched to avoid an
          over-complicated or brittle implementation. *)
           let code =
             B.soup_texts budget el |> B.concat budget ~sep:"" |> B.rstrip budget
           in
           let normalise s = B.lowercase budget (B.strip budget s) in
           let extract_class_attr node =
             match Soup.element node with
             | None -> []
             | Some el ->
               let cls = B.soup_attribute budget "class" el |> Option.value ~default:"" in
               B.split budget cls ~on:' ' |> B.map budget ~f:normalise
           in
           let class_names = extract_class_attr el in
           (* also inspect a nested <code> child if present *)
           let class_names =
             let code_child_opt =
               Soup.descendants el
               |> B.soup_nodes budget
               |> B.find budget ~f:(fun n ->
                 match Soup.element n with
                 | Some el' -> String.equal (Soup.name el') "code"
                 | None -> false)
             in
             match code_child_opt with
             | None -> class_names
             | Some code_child ->
               B.append budget class_names (extract_class_attr code_child)
           in
           let lang_of_class names : string option =
             let table : (string * string list) list =
               [ "ocaml", [ "ocaml"; "reason"; "ml"; "ocamlrepl" ]
               ; "sh", [ "shell"; "bash"; "sh" ]
               ; "c", [ "c" ]
               ; "cpp", [ "cpp"; "c++"; "cxx" ]
               ; "python", [ "python"; "py" ]
               ; "javascript", [ "javascript"; "js" ]
               ; "json", [ "json" ]
               ; "yaml", [ "yaml"; "yml" ]
               ; "html", [ "html" ]
               ]
             in
             B.find_map budget table ~f:(fun (lang, variants) ->
               if
                 B.exists budget names ~f:(fun cls ->
                   List.mem variants cls ~equal:String.equal)
               then Some lang
               else None)
           in
           let lang_class_opt = lang_of_class class_names in
           (* Heuristic 2: look at code content if no class-based hint. *)
           let guess_lang_from_content str : string option =
             let open String in
             let first_non_empty =
               B.lstrip budget str
               |> B.split_lines budget
               |> B.find budget ~f:(fun l -> not (String.is_empty (B.strip budget l)))
             in
             match first_non_empty with
             | None -> None
             | Some _ ->
               let line_trim = B.strip budget str in
               let ocaml_matches =
                 B.count budget ocaml_regexs ~f:(fun re ->
                   Option.value_map (Result.ok re) ~default:false ~f:(fun re ->
                     B.scan budget line_trim;
                     Re2.matches re line_trim))
               in
               let python_matches =
                 B.count budget python_regexs ~f:(fun re ->
                   Option.value_map (Result.ok re) ~default:false ~f:(fun re ->
                     B.scan budget line_trim;
                     Re2.matches re line_trim))
               in
               let javascript_matches =
                 B.count budget javascript_regexs ~f:(fun re ->
                   Option.value_map (Result.ok re) ~default:false ~f:(fun re ->
                     B.scan budget line_trim;
                     Re2.matches re line_trim))
               in
               let json_regex_matches =
                 B.count budget json_regex ~f:(fun re ->
                   Option.value_map (Result.ok re) ~default:false ~f:(fun re ->
                     B.scan budget line_trim;
                     Re2.matches re line_trim))
               in
               if
                 is_prefix line_trim ~prefix:"$ "
                 || is_prefix line_trim ~prefix:"#!/bin/sh"
               then Some "sh"
               else if
                 let open Int in
                 ocaml_matches > 0
                 && (python_matches = 0 || ocaml_matches >= python_matches)
                 && (javascript_matches = 0 || ocaml_matches >= javascript_matches)
                 && (json_regex_matches = 0 || ocaml_matches >= json_regex_matches)
               then Some "ocaml"
               else if
                 let open Int in
                 python_matches > 0
                 && (javascript_matches = 0 || python_matches >= javascript_matches)
                 && (json_regex_matches = 0 || python_matches >= json_regex_matches)
               then Some "python"
               else if
                 let open Int in
                 javascript_matches > 0
                 && (json_regex_matches = 0 || javascript_matches >= json_regex_matches)
               then Some "javascript"
               else if
                 is_prefix line_trim ~prefix:"<!--"
                 || is_prefix line_trim ~prefix:"<!DOCTYPE html>"
               then Some "html"
               else if
                 is_prefix line_trim ~prefix:"<!" || is_prefix line_trim ~prefix:"<?xml"
               then Some "xml"
               else if
                 is_prefix line_trim ~prefix:"#include <"
                 || is_prefix line_trim ~prefix:"#define "
               then Some "c"
               else if
                 is_prefix line_trim ~prefix:"#include \""
                 || is_prefix line_trim ~prefix:"#include <"
                 || is_prefix line_trim ~prefix:"#define "
               then Some "cpp"
               else if
                 let open Int in
                 json_regex_matches > 0
               then Some "json"
               else None
           in
           let lang =
             match lang_class_opt with
             | Some l -> l
             | None -> Option.value (guess_lang_from_content code) ~default:""
           in
           [ Code_block ([], lang, code) ]
         | "code" ->
           (* Stand-alone <code> outside <pre>: render as an inline-code paragraph so
             API signatures keep their monospace formatting. *)
           let code_txt =
             B.soup_texts budget el |> B.concat budget ~sep:"" |> B.strip budget
           in
           [ Paragraph ([], Code ([], code_txt)) ]
         | "ul" -> list_block budget ~ordered:false el
         | "ol" -> list_block budget ~ordered:true el
         | "table" -> table_block budget el
         | "dl" -> definition_list_block budget el
         | "br" -> [ Paragraph ([], concat budget [ Soft_break [] ]) ]
         | _ ->
           (* Unknown container: first try to recurse into its children, and if that
             yields blocks keep them; otherwise fall back to plain-text paragraph *)
           let inner = children_blocks budget el in
           if not (List.is_empty inner)
           then inner
           else (
             let txt_raw =
               B.soup_texts budget el
               |> B.concat budget ~sep:""
               |> collapse_whitespace budget
               |> B.strip budget
             in
             if String.is_empty txt_raw || is_css_noise budget txt_raw
             then []
             else [ Paragraph ([], txt budget txt_raw) ])))

and children_blocks budget (el : _ Soup.node) =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      Soup.children el
      |> B.soup_nodes budget
      |> B.concat_map budget ~f:(safe_block_of_node budget))

and list_block budget ~ordered el =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      let li_nodes =
        Soup.children el
        |> B.soup_nodes budget
        |> B.filter budget ~f:(fun n ->
          match Soup.element n with
          | Some li -> String.equal (Soup.name li) "li"
          | None -> false)
      in
      let items =
        B.map budget li_nodes ~f:(fun li ->
          Soup.children li
          |> B.soup_nodes budget
          |> B.concat_map budget ~f:(block_of_node budget))
      in
      let list_type =
        if ordered
        then (
          let start_attr = B.soup_attribute budget "start" el in
          let start_n =
            Option.value_map start_attr ~default:1 ~f:(fun value ->
              if B.is_bounded budget
              then (
                match Int.of_string_opt value with
                | Some n -> n
                | None ->
                  B.reject_html budget;
                  1)
              else Int.of_string value)
          in
          Ordered (start_n, '.'))
        else Bullet '-'
      in
      let spacing = Tight in
      [ List ([], list_type, spacing, items) ])

and table_block budget tbl =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 64;
      let all_rows =
        Soup.descendants tbl
        |> B.soup_nodes budget
        |> B.filter budget ~f:(fun n ->
          match Soup.element n with
          | Some el -> String.equal (Soup.name el) "tr"
          | None -> false)
      in
      let cell_inline cell = concat budget (inline_children budget cell) in
      let collect_cells tr =
        Soup.children tr
        |> B.soup_nodes budget
        |> B.filter_map budget ~f:(fun child ->
          match Soup.element child with
          | Some el ->
            (match Soup.name el with
             | "th" | "td" ->
               let inl = cell_inline el in
               (* Skip cells that are visually empty to avoid stray pipes. *)
               (match inl with
                | Text (_, s) when String.is_empty (B.strip budget s) -> None
                | Concat (_, lst) when List.is_empty lst -> None
                | _ -> Some inl)
             | _ -> None)
          | None -> None)
      in
      let rows =
        all_rows
        |> B.filter_map budget ~f:(fun tr ->
          let cells = collect_cells tr in
          if List.is_empty cells then None else Some cells)
      in
      match rows with
      | [] -> []
      | first :: _ as all_rows_nonempty ->
        (* Heuristic 1: if every row contains exactly one cell, treat this as a
         “simple list” table (odoc uses this for variant listings). Convert it
         to a bullet list instead of a Markdown pipe-table to avoid the noisy
         “Col1 | ---” header and triple-backtick artefacts. *)
        let single_col =
          B.for_all budget all_rows_nonempty ~f:(fun r -> B.length budget r = 1)
        in
        if single_col
        then (
          let clean_variant_string raw =
            let s = B.strip budget raw in
            (* Strip surrounding backticks of any length, but keep leading '|' *)
            let first = ref 0
            and last = ref (String.length s) in
            while
              !last - !first >= 2
              && Char.equal s.[!first] '`'
              && Char.equal s.[!last - 1] '`'
            do
              B.work budget 2;
              incr first;
              decr last
            done;
            let len = !last - !first in
            B.work budget len;
            String.sub s ~pos:!first ~len |> B.strip budget
          in
          let rec inline_to_string inl =
            B.enter budget;
            Exn.protect
              ~finally:(fun () -> B.leave budget)
              ~f:(fun () ->
                B.nodes budget 4;
                match inl with
                | Text (_, s) | Code (_, s) -> s
                | Concat (_, lst) ->
                  B.concat budget ~sep:"" (B.map budget lst ~f:inline_to_string)
                | Emph (_, inner) | Strong (_, inner) -> inline_to_string inner
                | _ -> "")
          in
          let row_to_blocks cells =
            match cells with
            | inl :: _ ->
              let raw = inline_to_string inl |> clean_variant_string in
              if String.is_empty raw then [] else [ Paragraph ([], Code ([], raw)) ]
            | [] -> []
          in
          let items = B.map budget all_rows_nonempty ~f:row_to_blocks in
          [ List ([], Bullet '-', Tight, items) ])
        else (
          (* Regular table conversion path *)
          (* Determine whether [first] is a header row by checking if the original
           HTML row had <th> elements. *)
          let is_header_row tr =
            Soup.children tr
            |> B.soup_nodes budget
            |> B.exists budget ~f:(fun child ->
              match Soup.element child with
              | Some el -> String.equal (Soup.name el) "th"
              | None -> false)
          in
          let header_opt =
            match all_rows with
            | hdr :: _ when is_header_row hdr -> Some first
            | _ -> None
          in
          let body_rows =
            if Option.is_some header_opt
            then List.tl_exn all_rows_nonempty
            else all_rows_nonempty
          in
          let header_cells, alignments =
            match header_opt with
            | Some hdr_cells -> hdr_cells, B.map budget hdr_cells ~f:(fun _ -> Left)
            | None ->
              (* Synthesize a header row from body cell count. *)
              let synthetic =
                B.mapi budget first ~f:(fun i _ ->
                  txt budget (Printf.sprintf "Col%d" (i + 1)))
              in
              synthetic, B.map budget synthetic ~f:(fun _ -> Left)
          in
          let header = B.zip_exn budget header_cells alignments in
          let body = B.map budget body_rows ~f:(fun r -> r) in
          [ Table ([], header, body) ]))

and definition_list_block budget dl : attr block list =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      (* Convert <dl><dt>term</dt><dd>definition</dd> … </dl> into
     a sequence of blocks where each term is a bold paragraph followed by
     the blocks of its definition. *)
      let children = Soup.children dl |> B.soup_nodes budget in
      let rec loop acc current_term = function
        | [] -> B.rev budget acc
        | node :: tl ->
          (match Soup.element node with
           | Some el when String.equal (Soup.name el) "dt" ->
             let term_inl = concat budget (inline_children budget el) in
             loop acc (Some term_inl) tl
           | Some el when String.equal (Soup.name el) "dd" ->
             (match current_term with
              | None -> loop acc None tl (* malformed, skip *)
              | Some term ->
                let term_para = Paragraph ([], Strong ([], term)) in
                let def_blocks = children_blocks budget el in
                loop (B.rev_append budget (term_para :: def_blocks) acc) None tl)
           | _ -> loop acc current_term tl)
      in
      loop [] None children)
;;

(******************************************************************************)
(* Public API                                                                 *)
(******************************************************************************)

let convert budget (soup_original : Soup.soup Soup.node) : Omd.doc =
  B.enter budget;
  Exn.protect
    ~finally:(fun () -> B.leave budget)
    ~f:(fun () ->
      B.nodes budget 8;
      (* 1. Trim to main content if recognisable, then prune generic noise *)
      let soup_poly = Soup.coerce soup_original in
      let soup = isolate_main_content budget soup_poly in
      prune_noise budget soup;
      (* 2. Proceed with normal conversion *)
      let root =
        (* Find <body> tag without using selector engine *)
        Soup.descendants soup
        |> B.soup_nodes budget
        |> B.find_map budget ~f:(fun n ->
          match Soup.element n with
          | Some el when String.equal (Soup.name el) "body" -> Some (Soup.coerce el)
          | _ -> None)
        |> Option.value ~default:(Soup.coerce soup)
      in
      Soup.children root
      |> B.soup_nodes budget
      |> B.concat_map budget ~f:(safe_block_of_node budget))
;;
