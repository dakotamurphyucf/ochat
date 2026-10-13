open! Core
module Ast = Chatmd_ast
module Source_ref = Chatmd_shell_spec.Source_ref

type sourced_node =
  { node : Ast.node
  ; source : Source_ref.t
  ; source_node : Source_loader.source
  ; range : Chatmd_parser_input.Range.t option
  ; text_fragments : Chatmd_parser_builder.Text_fragment.t list
  ; input : Chatmd_parser_input.t option
  ; children : sourced_node list option
  }

type context =
  { dir : Eio.Fs.dir_ty Eio.Path.t
  ; loader : Source_loader.t
  ; source_node : Source_loader.source
  ; file : string
  ; prompt_dir : string
  ; namespace : string option
  ; source : string
  ; active_imports : String.Set.t
  ; import_depth : int
  ; canonical_sources : bool
  ; mutable cached_source : Source_ref.t option
  }

let source_attribute = "ochat-source-context"

let source_ref (context : context) range =
  match range with
  | Some range ->
    Source_ref.
      { file = context.file
      ; source_dir = Eio.Path.native_exn context.dir
      ; prompt_dir = context.prompt_dir
      ; namespace = context.namespace
      ; start_pos = Chatmd_parser_input.Range.start_position range
      ; end_pos = Chatmd_parser_input.Range.end_position range
      ; source_sha256 =
          Chatmd_parser_input.Range.input range |> Chatmd_parser_input.effective_sha256
      }
  | None ->
    (match context.cached_source with
     | Some source -> source
     | None ->
       let position : Source_ref.position = { offset = 0; line = 1; column = 1 } in
       (* The legacy parser has no per-node ranges. Its whole-input provenance
          still needs consistent byte/line/column endpoints. Reuse the shared
          CR/LF and byte-column convention, once per cached source. *)
       let index = Source.Position_index.create context.source ~checkpoint:ignore in
       let last =
         Source.Position_index.position
           index
           ~byte_offset:(String.length context.source)
           ~checkpoint:ignore
         |> Or_error.ok_exn
       in
       let end_pos : Source_ref.position =
         { offset = last.offset; line = last.line; column = last.column + 1 }
       in
       let source =
         Source_ref.create
           ~file:context.file
           ~source_dir:(Eio.Path.native_exn context.dir)
           ~prompt_dir:context.prompt_dir
           ~namespace:context.namespace
           ~start_pos:position
           ~end_pos
           ~source:context.source
       in
       context.cached_source <- Some source;
       source)
;;

let can_have_imports = function
  | Ast.User | Agent | System | Developer -> true
  | _ -> false
;;

let attribute_values attributes name =
  List.filter_map attributes ~f:(fun (candidate, value) ->
    Option.some_if (String.equal candidate name) value)
;;

let import_attribute attributes name =
  match attribute_values attributes name with
  | [] -> None
  | [ Some value ] when not (String.is_empty value) -> Some value
  | [ Some _ ] -> failwithf "<import> attribute %S cannot be empty." name ()
  | [ None ] -> failwithf "<import> attribute %S requires a value." name ()
  | _ -> failwithf "<import> has duplicate %S attributes." name ()
;;

let validate_import_attributes attributes =
  List.iter attributes ~f:(fun (name, _) ->
    if not (List.mem [ "src"; "namespace" ] name ~equal:String.equal)
    then failwithf "<import> does not support attribute %S." name ())
;;

let combine_namespace parent child =
  match parent, child with
  | None, None -> None
  | Some namespace, None | None, Some namespace -> Some namespace
  | Some parent, Some child -> Some (parent ^ ":" ^ child)
;;

let valid_namespace namespace =
  (not (String.is_empty namespace))
  && (match namespace.[0] with
      | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
      | _ -> false)
  && String.for_all (String.drop_prefix namespace 1) ~f:(function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '-' -> true
    | _ -> false)
;;

let namespace_attribute attributes =
  let namespace = import_attribute attributes "namespace" in
  Option.iter namespace ~f:(fun value ->
    if not (valid_namespace value)
    then failwithf "<import> has invalid namespace %S." value ());
  namespace
;;

let normalize_native_path value =
  let value = String.substr_replace_all value ~pattern:"\\" ~with_:"/" in
  let absolute = String.is_prefix value ~prefix:"/" in
  let parts =
    String.split value ~on:'/'
    |> List.fold ~init:[] ~f:(fun parts -> function
      | "" | "." -> parts
      | ".." -> List.drop parts 1
      | part -> part :: parts)
    |> List.rev
  in
  (if absolute then "/" else "") ^ String.concat ~sep:"/" parts
;;

let import_key context src =
  match Source_loader.resolve context.loader ~base:context.source_node ~reference:src with
  | Ok source -> Source_loader.relative_path source |> normalize_native_path
  | Error _ -> normalize_native_path src
;;

let reject_import_cycle context src =
  if context.import_depth >= 128
  then failwith "ChatMD import nesting exceeds the maximum depth of 128.";
  let key = import_key context src in
  if Set.mem context.active_imports key
  then failwithf "ChatMD import cycle detected at %S." src ();
  key
;;

let import_context ~read_import (context : context) attributes =
  validate_import_attributes attributes;
  let src =
    match import_attribute attributes "src" with
    | Some src -> src
    | None -> failwith "<import> requires a src attribute."
  in
  let key = reject_import_cycle context src in
  let source_node =
    Source_loader.resolve context.loader ~base:context.source_node ~reference:src
    |> Result.ok_or_failwith
  in
  let source = read_import context.loader source_node |> Result.ok_or_failwith in
  let namespace = combine_namespace context.namespace (namespace_attribute attributes) in
  let dir = Source_loader.materialized_dir source_node in
  { dir
  ; loader = context.loader
  ; source_node
  ; file =
      (if context.canonical_sources then Source_loader.relative_path source_node else src)
  ; prompt_dir = context.prompt_dir
  ; namespace
  ; source
  ; active_imports = Set.add context.active_imports key
  ; import_depth = context.import_depth + 1
  ; canonical_sources = context.canonical_sources
  ; cached_source = None
  }
;;

let validate_namespace_aliases ~node nodes =
  let aliases =
    List.filter_map nodes ~f:(fun item ->
      match node item with
      | Ast.Element (Ast.Import, attributes, _) -> namespace_attribute attributes
      | _ -> None)
  in
  match List.find_a_dup aliases ~compare:String.compare with
  | None -> ()
  | Some alias -> failwithf "Duplicate ChatMD import namespace %S." alias ()
;;

let rec check_generated_depth depth = function
  | Ast.Text _ -> ()
  | Ast.Element (_, _, children) ->
    if depth >= 128 then failwith "generated expanded markup nesting limit exceeded";
    List.iter children ~f:(check_generated_depth (depth + 1))
;;

module Make (Node : sig
    type t

    val node : t -> Ast.node
    val range : t -> Chatmd_parser_input.Range.t option
    val children : t -> t list
    val text_fragments : t -> Chatmd_parser_builder.Text_fragment.t list
    val input : t -> Chatmd_parser_input.t option
    val retain_children : bool
  end) =
struct
  let make context node range text_fragments input children =
    { node
    ; range
    ; text_fragments
    ; input
    ; source = source_ref context range
    ; source_node = context.source_node
    ; children
    }
  ;;

  let rec sourced ~check_node ~depth context node =
    check_node ~depth (Node.node node);
    let children =
      if Node.retain_children
      then
        Some
          (List.map
             (Node.children node)
             ~f:(sourced ~check_node ~depth:(depth + 1) context))
      else None
    in
    make
      context
      (Node.node node)
      (Node.range node)
      (Node.text_fragments node)
      (Node.input node)
      children
  ;;

  let rec expand_nodes ~checkpoint ~check_node ~read_import ~parse ~depth context nodes =
    validate_namespace_aliases ~node:Node.node nodes;
    List.concat_map
      nodes
      ~f:(expand_node ~checkpoint ~check_node ~read_import ~parse ~depth context)

  and expand_node ~checkpoint ~check_node ~read_import ~parse ~depth context node =
    let semantic = Node.node node in
    (match semantic with
     | Ast.Element (Ast.Import, _, _) -> checkpoint ()
     | _ -> check_node ~depth semantic);
    if context.canonical_sources && not Node.retain_children
    then check_generated_depth depth semantic;
    match semantic with
    | Ast.Element (Ast.Import, attributes, _) ->
      let imported = import_context ~read_import context attributes in
      expand_nodes
        ~checkpoint
        ~check_node
        ~read_import
        ~parse
        ~depth
        imported
        (parse imported.source_node imported.source)
    | Ast.Element (Ast.Msg, attributes, _) ->
      let role = List.Assoc.find attributes ~equal:String.equal "role" in
      (match role with
       | Some (Some "user") | Some (Some "system") | Some (Some "developer") ->
         sourced_parent
           ~checkpoint
           ~check_node
           ~read_import
           ~parse
           ~depth
           context
           node
           Ast.Msg
           attributes
       | _ -> [ sourced ~check_node ~depth context node ])
    | Ast.Element (tag, attributes, _) when can_have_imports tag ->
      sourced_parent
        ~checkpoint
        ~check_node
        ~read_import
        ~parse
        ~depth
        context
        node
        tag
        attributes
    | _ -> [ sourced ~check_node ~depth context node ]

  and sourced_parent
        ~checkpoint
        ~check_node
        ~read_import
        ~parse
        ~depth
        context
        node
        tag
        attributes
    =
    let children =
      expand_nodes
        ~checkpoint
        ~check_node
        ~read_import
        ~parse
        ~depth:(depth + 1)
        context
        (Node.children node)
    in
    [ make
        context
        (Ast.Element (tag, attributes, List.map children ~f:(fun child -> child.node)))
        (Node.range node)
        (Node.text_fragments node)
        (Node.input node)
        (Some children)
    ]
  ;;

  let expand
        ~checkpoint
        ~check_node
        ~canonical_sources
        ~read_import
        ~parse
        ~loader
        ~root_source
        ~dir
        ~file
        ~source
        document
    =
    let active_imports =
      if String.is_prefix file ~prefix:"<"
      then String.Set.empty
      else String.Set.singleton (Source_loader.relative_path root_source)
    in
    let context =
      { dir =
          (if canonical_sources then Source_loader.materialized_dir root_source else dir)
      ; loader
      ; source_node = root_source
      ; file
      ; prompt_dir = Eio.Path.native_exn dir
      ; namespace = None
      ; source
      ; active_imports
      ; import_depth = 0
      ; canonical_sources
      ; cached_source = None
      }
    in
    expand_nodes ~checkpoint ~check_node ~read_import ~parse ~depth:0 context document
  ;;
end

module Legacy = Make (struct
    type t = Ast.node

    let node node = node
    let range _ = None
    let text_fragments _ = []
    let input _ = None

    let children = function
      | Ast.Text _ -> []
      | Ast.Element (_, _, children) -> children
    ;;

    let retain_children = false
  end)

module Located = Make (struct
    type t = Chatmd_located_ast.t

    let node = Chatmd_located_ast.node
    let range = Chatmd_located_ast.range
    let text_fragments = Chatmd_located_ast.text_fragments
    let input node = Some (Chatmd_located_ast.input node)
    let children = Chatmd_located_ast.children
    let retain_children = true
  end)

let expand
      ?(canonical_sources = false)
      ~parse
      ~loader
      ~root_source
      ~dir
      ~file
      ~source
      document
  =
  Legacy.expand
    ~checkpoint:ignore
    ~check_node:(fun ~depth:_ _ -> ())
    ~canonical_sources
    ~read_import:Source_loader.read
    ~parse:(fun _ contents -> parse contents)
    ~loader
    ~root_source
    ~dir
    ~file
    ~source
    document
;;

let expand_located = Located.expand
