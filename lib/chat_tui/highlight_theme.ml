(** Implementation of {!Chat_tui.Highlight_theme}.

    Represent a theme as an ordered list of [prefix -> attr] rules and
    implement the dot-segment-aware resolution described in the interface.

    The public API and matching semantics are documented in
    {!Chat_tui.Highlight_theme}. *)

module Styles = Highlight_styles

type rule =
  { prefix : string
  ; attr : Notty.A.t
  }

open Core

(* VSCode/GitHub-dark truecolor palette. Matches tokens from the supplied
   theme JSON as closely as Notty allows. *)
let github_dark_rules : rule list =
  [ { prefix = "comment"; attr = Styles.(fg_hex "#6A737D") } (* Headings – azure, bold. *)
  ; { prefix = "heading.1.markdown"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "heading.2.markdown"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "heading.3.markdown"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "heading.4.markdown"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "heading.5.markdown"; attr = Styles.(fg_hex "#E1E4E8" ++ bold) }
  ; { prefix = "heading.6.markdown"; attr = Styles.(fg_hex "#E1E4E8" ++ bold) }
    (* Setext headings *)
  ; { prefix = "markup.heading.setext.1.markdown"
    ; attr = Styles.(fg_hex "#79B8FF" ++ bold)
    }
  ; { prefix = "markup.heading.setext.2.markdown"
    ; attr = Styles.(fg_hex "#79B8FF" ++ bold)
    }
  ; { prefix = "entity.name.section"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "punctuation.definition.heading"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "meta.paragraph.markdown"; attr = Styles.(fg_hex "#D1D5DA") }
  ; { prefix = "source"; attr = Styles.(fg_hex "#D1D5DA") } (* Lists & quotes *)
  ; { prefix = "markup.list.numbered"; attr = Styles.(fg_hex "#FFAB70") }
  ; { prefix = "markup.list.unnumbered"; attr = Styles.(fg_hex "#FFAB70") }
  ; { prefix = "punctuation.definition.list"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.list.begin.markdown"
    ; attr = Styles.(fg_hex "#FFAB70")
    }
  ; { prefix = "markup.quote"; attr = Styles.(fg_hex "#85E89D") }
  ; { prefix = "punctuation.definition.quote"; attr = Styles.(fg_hex "#6A737D") }
    (* Links & images *)
  ; { prefix = "meta.link"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "markup.underline.link"; attr = Styles.(fg_hex "#79B8FF" ++ underline) }
  ; { prefix = "constant.other.reference.link"
    ; attr = Styles.(fg_hex "#DBEDFF" ++ underline)
    }
  ; { prefix = "string.other.link.title"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "string.other.link.description"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "punctuation.definition.link"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.image"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "markup.image"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "meta.image"; attr = Styles.(fg_hex "#79B8FF") }
    (* Tables, separators, inline code chip *)
  ; { prefix = "markup.table"; attr = Styles.(fg_hex "#FFAB70") }
  ; { prefix = "punctuation.separator.table"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.table"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "meta.separator.markdown"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "meta.separator"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "markup.inline.code"
    ; attr = Styles.(fg_hex "#D1D5DA" ++ bg_hex "#2F363D")
    }
  ; { prefix = "markup.inline.raw.string.markdown"
    ; attr = Styles.(fg_hex "#D1D5DA" ++ bg_hex "#2F363D")
    }
  ; { prefix = "markup.inline.raw"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "markup.raw"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "markup.raw.block.markdown"
    ; attr = Styles.(fg_hex "#D1D5DA" ++ bg_hex "#2F363D")
    }
  ; { prefix = "punctuation.definition.fenced"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.markdown"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.metadata.markdown"
    ; attr = Styles.(fg_hex "#6A737D")
    }
  ; { prefix = "punctuation.separator.key-value.markdown"
    ; attr = Styles.(fg_hex "#6A737D")
    }
  ; { prefix = "punctuation.definition.raw.markdown"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "fenced_code.block.language.markdown"
    ; attr = Styles.(fg_hex "#79B8FF" ++ italic)
    }
  ; { prefix = "fenced_code.block.language.attributes.markdown"
    ; attr = Styles.(fg_gray 11 ++ italic)
    }
  ; { prefix = "meta.embedded.block"; attr = Styles.(bg_gray 2 ++ fg_gray 15) }
  ; { prefix = "meta.embedded.block.frontmatter"
    ; attr = Styles.(bg_hex "#1F2428" ++ fg_hex "#E1E4E8" ++ italic)
    }
  ; { prefix = "comment.frontmatter"; attr = Styles.(fg_gray 9 ++ italic) }
  ; { prefix = "punctuation.definition.begin.frontmatter"
    ; attr = Styles.(fg_hex "#6A737D")
    }
  ; { prefix = "punctuation.definition.end.frontmatter"
    ; attr = Styles.(fg_hex "#6A737D")
    }
  ; { prefix = "punctuation.definition.bold"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.italic"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "markup.bold.markdown"; attr = Styles.(fg_hex "#E1E4E8" ++ bold) }
  ; { prefix = "markup.italic.markdown"; attr = Styles.(fg_hex "#E1E4E8" ++ italic) }
  ; { prefix = "markup.bold"; attr = Styles.(fg_hex "#E1E4E8" ++ bold) }
  ; { prefix = "markup.italic"; attr = Styles.(fg_hex "#E1E4E8" ++ italic) }
  ; { prefix = "markup.underline"; attr = Styles.(underline) }
  ; { prefix = "markup.strike"; attr = Styles.(fg_hex "#F97583") }
  ; { prefix = "punctuation.definition.strikethrough.markdown"
    ; attr = Styles.(fg_hex "#F97583")
    }
  ; { prefix = "markup.list.checked"; attr = Styles.(fg_hex "#85E89D" ++ bold) }
  ; { prefix = "markup.list.unchecked"; attr = Styles.(fg_hex "#FFAB70") }
  ; { prefix = "punctuation.definition.checkbox"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "constant.character.escape"; attr = Styles.(fg_hex "#85E89D" ++ bold) }
    (* Core code tokens – GitHub Dark Default *)
  ; { prefix = "string"; attr = Styles.(fg_hex "#9ECBFF") }
  ; { prefix = "string.regexp"; attr = Styles.(fg_hex "#DBEDFF") }
  ; { prefix = "source.regexp"; attr = Styles.(fg_hex "#DBEDFF") }
  ; { prefix = "string.regexp constant.character.escape"
    ; attr = Styles.(fg_hex "#85E89D" ++ bold)
    }
  ; { prefix = "keyword"; attr = Styles.(fg_hex "#F97583") }
  ; { prefix = "keyword.operator"; attr = Styles.(fg_hex "#D1D5DA") }
  ; { prefix = "constant.numeric"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "constant"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "support"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "support.constant"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "support.variable"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "meta.property-name"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "meta.module-reference"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "entity.name.function"; attr = Styles.(fg_hex "#B392F0") }
  ; { prefix = "entity.name.type"; attr = Styles.(fg_hex "#B392F0") }
  ; { prefix = "entity"; attr = Styles.(fg_hex "#B392F0") }
  ; { prefix = "entity.name"; attr = Styles.(fg_hex "#B392F0") }
  ; { prefix = "entity.name.tag"; attr = Styles.(fg_hex "#85E89D") }
  ; { prefix = "entity.other.attribute-name"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "variable.parameter"; attr = Styles.(fg_hex "#E1E4E8") }
  ; { prefix = "variable.language"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "variable.other.constant"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "variable.other.enummember"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "variable.other"; attr = Styles.(fg_hex "#E1E4E8") }
  ; { prefix = "variable"; attr = Styles.(fg_hex "#FFAB70") }
  ; { prefix = "punctuation"; attr = Styles.(fg_hex "#D1D5DA") }
  ; { prefix = "punctuation.definition.tag"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "punctuation.definition.comment"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "operator"; attr = Styles.(fg_hex "#D1D5DA") }
  ; { prefix = "invalid.broken"; attr = Styles.(fg_hex "#FDAEB7" ++ italic) }
  ; { prefix = "invalid.deprecated"; attr = Styles.(fg_hex "#FDAEB7" ++ italic) }
  ; { prefix = "invalid.illegal"; attr = Styles.(fg_hex "#FDAEB7" ++ italic) }
  ; { prefix = "invalid.unimplemented"; attr = Styles.(fg_hex "#FDAEB7" ++ italic) }
  ; { prefix = "carriage-return"
    ; attr = Styles.(bg_hex "#F97583" ++ fg_hex "#24292E" ++ italic ++ underline)
    }
  ; { prefix = "markup.heading.markdown"; attr = Styles.(fg_hex "#79B8FF" ++ bold) }
  ; { prefix = "markup.list"; attr = Styles.(fg_hex "#FFAB70") }
  ; { prefix = "markup.deleted"; attr = Styles.(fg_hex "#FDAEB7" ++ bg_hex "#86181D") }
  ; { prefix = "markup.inserted"; attr = Styles.(fg_hex "#85E89D" ++ bg_hex "#144620") }
  ; { prefix = "markup.inserted.ochatpatch"; attr = Styles.(fg_hex "#85E89D") }
  ; { prefix = "markup.changed"; attr = Styles.(fg_hex "#FFAB70" ++ bg_hex "#C24E00") }
  ; { prefix = "meta.diff.index"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "meta.diff.file.a"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "meta.diff.file.b"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "meta.diff.hunk"; attr = Styles.(fg_hex "#B392F0" ++ bold) }
  ; { prefix = "meta.diff.range"; attr = Styles.(fg_hex "#B392F0" ++ bold) }
  ; { prefix = "meta.diff.header"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "text.diff.context"; attr = Styles.(fg_hex "#D1D5DA") }
  ; { prefix = "punctuation.definition.header.ochatpatch"
    ; attr = Styles.(fg_hex "#6A737D")
    }
  ; { prefix = "entity.name.filename.ochatpatch"
    ; attr = Styles.(fg_hex "#79B8FF" ++ bold)
    }
  ; { prefix = "punctuation.definition.header.trailing.ochatpatch"
    ; attr = Styles.(fg_gray 8)
    }
  ; { prefix = "constant.numeric.line-number.ochatpatch"
    ; attr = Styles.(fg_hex "#F97583")
    }
  ; { prefix = "brackethighlighter"; attr = Styles.(fg_hex "#D1D5DA") }
  ; { prefix = "brackethighlighter.unmatched"; attr = Styles.(fg_hex "#FDAEB7") }
  ; { prefix = "token.info-token"; attr = Styles.(fg_hex "#6796E6") }
  ; { prefix = "token.warn-token"; attr = Styles.(fg_hex "#CD9731") }
  ; { prefix = "token.error-token"; attr = Styles.(fg_hex "#F44747") }
  ; { prefix = "token.debug-token"; attr = Styles.(fg_hex "#B267E6") }
  ; { prefix = "comment.line.number-sign.shell"; attr = Styles.(fg_hex "#6A737D") }
  ; { prefix = "variable.parameter.option.shell"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "string.quoted.double.shell"; attr = Styles.(fg_hex "#79B8FF") }
  ; { prefix = "constant.character.escape.shell"; attr = Styles.(fg_hex "#85E89D") }
  ; { prefix = "string.quoted.single.shell"; attr = Styles.(fg_hex "#FDAEB7") }
  ; { prefix = "keyword.control.shell"; attr = Styles.(fg_hex "#F97583") }
  ; { prefix = "variable.other.shell"; attr = Styles.(fg_hex "#CD9731") }
  ; { prefix = "string.interpolated.command-substitution.shell"
    ; attr = Styles.(fg_hex "#231ce4ff")
    }
  ; { prefix = "string.other.backtick.shell"; attr = Styles.(fg_hex "#a22782ff") }
  ; { prefix = "meta.arithmetic.shell"; attr = Styles.(fg_hex "#d0bb1bff") }
  ; { prefix = "operator.redirection.shell"; attr = Styles.(fg_hex "#c71818ff") }
  ; { prefix = "punctuation.separator.shell"; attr = Styles.(fg_hex "#d4df3bff") }
  ; { prefix = "operator.equal.shell"; attr = Styles.(fg_hex "#F44747") }
  ; { prefix = "entity.name.type.shell"; attr = Styles.(fg_hex "#E1E4E8") }
  ; { prefix = "entity.name.keyword.function.shell"; attr = Styles.(fg_hex "#B392F0") }
  ; { prefix = "entity.name.keyword.echo.shell"; attr = Styles.(fg_hex "#79B8FF") }
  ]
;;

module Rule = struct
  type t =
    { attr : Notty.A.t
    ; index : int
    ; segments : string array
    ; seg_count : int
    }
end

module Trie = struct
  type node =
    { children : node String.Table.t
    ; mutable rules_here : Rule.t list
    }

  type t = node

  let create () = { children = String.Table.create (); rules_here = [] }

  let rec insert_segments node segments i rule =
    if i = Array.length segments
    then node.rules_here <- rule :: node.rules_here
    else (
      let seg = segments.(i) in
      let child =
        match Hashtbl.find node.children seg with
        | Some c -> c
        | None ->
          let c = create () in
          Hashtbl.set node.children ~key:seg ~data:c;
          c
      in
      insert_segments child segments (i + 1) rule)
  ;;

  let insert t rule = insert_segments t rule.Rule.segments 0 rule
end

module Scope_cache = struct
  type t = Notty.A.t String.Table.t

  let create ~initial_capacity = String.Table.create ~size:initial_capacity ()
end

type compiled =
  { rules : Rule.t array
  ; trie : Trie.t
  ; cache : Scope_cache.t
  }

let sort_uniq_scopes scopes =
  let sorted = List.sort ~compare:String.compare scopes in
  let rec dedup acc = function
    | [] -> List.rev acc
    | x :: y :: tl when String.(x = y) -> dedup acc (y :: tl)
    | x :: tl -> dedup (x :: acc) tl
  in
  dedup [] sorted
;;

let scope_key_separator = "."
let key_of_scopes scopes = String.concat ~sep:scope_key_separator scopes

let split_segments prefix =
  let len = String.length prefix in
  let rec loop i start acc =
    if i = len
    then (
      let segment = String.sub prefix ~pos:start ~len:(i - start) in
      List.rev (segment :: acc))
    else if Char.(prefix.[i] = '.')
    then (
      let segment = String.sub prefix ~pos:start ~len:(i - start) in
      loop (i + 1) (i + 1) (segment :: acc))
    else loop (i + 1) start acc
  in
  Array.of_list (loop 0 0 [])
;;

let compile (rules : rule list) : compiled =
  let rules_array =
    Array.of_list
      (List.mapi
         ~f:(fun index { prefix; attr } ->
           let segments = split_segments prefix in
           let seg_count = Array.length segments in
           { Rule.attr; index; segments; seg_count })
         rules)
  in
  let trie = Trie.create () in
  Array.iter ~f:(Trie.insert trie) rules_array;
  let cache = Scope_cache.create ~initial_capacity:1024 in
  { rules = rules_array; trie; cache }
;;

type t = { compiled : compiled }

let make_theme (rules : rule list) : t =
  let compiled = compile rules in
  { compiled }
;;

let empty = make_theme []
let github_dark : t = make_theme github_dark_rules

let attr_of_scopes (t : t) ~scopes =
  let compiled = t.compiled in
  let rules = compiled.rules in
  let n_rules = Array.length rules in
  if n_rules = 0
  then Styles.empty
  else (
    let scopes = sort_uniq_scopes scopes in
    match scopes with
    | [] -> Styles.empty
    | _ ->
      let key = key_of_scopes scopes in
      let compiled_cache = compiled.cache in
      (match Hashtbl.find compiled_cache key with
       | Some attr -> attr
       | None ->
         let exact0 = Array.init n_rules ~f:(fun _ -> 0) in
         let exact1 = Array.init n_rules ~f:(fun _ -> 0) in
         let process_scope scope =
           let segments = split_segments scope in
           let scope_seg_count = Array.length segments in
           let rec traverse node depth =
             if depth = scope_seg_count
             then ()
             else (
               match Hashtbl.find node.Trie.children segments.(depth) with
               | None -> ()
               | Some child ->
                 List.iter
                   ~f:(fun (rule : Rule.t) ->
                     let idx = rule.index in
                     if rule.seg_count = scope_seg_count
                     then exact1.(idx) <- exact1.(idx) + 1
                     else exact0.(idx) <- exact0.(idx) + 1)
                   child.Trie.rules_here;
                 traverse child (depth + 1))
           in
           traverse compiled.trie 0
         in
         List.iter ~f:process_scope scopes;
         let best_segments = ref (-1) in
         let best_exact = ref (-1) in
         let best_exact_for_rule = Array.init n_rules ~f:(fun _ -> -1) in
         for j = 0 to n_rules - 1 do
           let e_j = if exact1.(j) > 0 then 1 else if exact0.(j) > 0 then 0 else -1 in
           best_exact_for_rule.(j) <- e_j;
           if e_j >= 0
           then (
             let s_j = rules.(j).seg_count in
             if s_j > !best_segments
             then (
               best_segments := s_j;
               best_exact := e_j)
             else if s_j = !best_segments && e_j > !best_exact
             then best_exact := e_j)
         done;
         let result =
           if !best_segments < 0
           then Styles.empty
           else (
             let acc = ref Styles.empty in
             for j = 0 to n_rules - 1 do
               let rule = rules.(j) in
               let s_j = rule.seg_count in
               let e_j = best_exact_for_rule.(j) in
               if s_j = !best_segments && e_j = !best_exact
               then (
                 let count = if !best_exact = 1 then exact1.(j) else exact0.(j) in
                 for _ = 1 to count do
                   acc := Styles.(!acc ++ rule.attr)
                 done)
             done;
             !acc)
         in
         if Hashtbl.length compiled_cache >= 1000000 then Hashtbl.clear compiled_cache;
         Hashtbl.set compiled_cache ~key ~data:result;
         result))
;;

let%test_unit "attr_of_scopes_golden_same_specificity_composes_in_order" =
  let rules =
    [ { prefix = "scope"; attr = Styles.fg_red }
    ; { prefix = "scope"; attr = Styles.bold }
    ; { prefix = "other"; attr = Styles.italic }
    ]
  in
  let theme = make_theme rules in
  let scopes = [ "scope" ] in
  let got = attr_of_scopes theme ~scopes in
  let expected = Styles.(fg_red ++ bold) in
  if not (Notty.A.equal got expected)
  then failwith "attr_of_scopes should compose equal-specificity rules in order"
;;

let%test_unit "attr_of_scopes_golden_longest_prefix_wins_across_scopes" =
  let rules =
    [ { prefix = "a"; attr = Styles.fg_red }
    ; { prefix = "a.b"; attr = Styles.fg_green }
    ; { prefix = "a.b.c"; attr = Styles.fg_blue }
    ]
  in
  let theme = make_theme rules in
  let scopes = [ "a"; "a.b.c.d" ] in
  let got = attr_of_scopes theme ~scopes in
  let expected = Styles.fg_blue in
  if not (Notty.A.equal got expected)
  then failwith "attr_of_scopes should pick the longest matching prefix"
;;

let%test_unit "attr_of_scopes_golden_empty_scopes" =
  let scopes = [] in
  let trie = attr_of_scopes github_dark ~scopes in
  if not (Notty.A.equal trie Styles.empty)
  then failwith "attr_of_scopes should return empty for empty scopes"
;;

let%test_unit "attr_of_scopes_golden_scope_order_irrelevant" =
  let scopes = [ "keyword"; "source.ocaml"; "keyword.operator" ] in
  let scopes_permuted = [ "source.ocaml"; "keyword.operator"; "keyword" ] in
  let trie1 = attr_of_scopes github_dark ~scopes in
  let trie2 = attr_of_scopes github_dark ~scopes:scopes_permuted in
  if not (Notty.A.equal trie1 trie2)
  then failwith "attr_of_scopes should not depend on scope order"
;;
