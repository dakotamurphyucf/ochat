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

type t = rule list

let empty = []

(* VSCode/GitHub-dark truecolor palette. Matches tokens from the supplied
   theme JSON as closely as Notty allows. *)
let github_dark : t =
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

let is_selector_match ~selector scope =
  let sel_len = String.length selector in
  let scope_len = String.length scope in
  scope_len >= sel_len
  && String.sub scope 0 sel_len = selector
  && (scope_len = sel_len || scope.[sel_len] = '.')
;;

let count_segments s =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then acc else loop (i + 1) (acc + if s.[i] = '.' then 1 else 0)
  in
  1 + loop 0 0
;;

let cmp_int (a : int) (b : int) = if a < b then -1 else if a > b then 1 else 0

let attr_of_scopes (t : t) ~scopes =
  let best_key = ref None in
  let best_attr = ref None in
  List.iteri
    (fun rule_index rule ->
       let selector = rule.prefix in
       let segments = count_segments selector in
       let sel_len = String.length selector in
       List.iter
         (fun scope ->
            if is_selector_match ~selector scope
            then (
              let exact = if String.length scope = sel_len then 1 else 0 in
              let key_segments = segments in
              let key_exact = exact in
              let key_len = sel_len in
              let key_rule = -rule_index in
              match !best_key with
              | None ->
                best_key := Some (key_segments, key_exact, key_len, key_rule);
                best_attr := Some rule.attr
              | Some (s', e', l', r') ->
                let c1 = cmp_int key_segments s' in
                let better, combine =
                  if c1 <> 0
                  then c1 > 0, false
                  else (
                    let c2 = cmp_int key_exact e' in
                    if c2 <> 0
                    then c2 > 0, false
                    else (
                      let c3 = cmp_int key_len l' in
                      if c3 <> 0 then c3 > 0, true else cmp_int key_rule r' > 0, true))
                in
                (match better, combine with
                 | true, false ->
                   best_key := Some (key_segments, key_exact, key_len, key_rule);
                   best_attr := Some rule.attr
                 | true, true ->
                   best_key := Some (key_segments, key_exact, key_len, key_rule);
                   best_attr := Some Styles.(Option.get !best_attr ++ rule.attr)
                 | false, true ->
                   best_attr := Some Styles.(Option.get !best_attr ++ rule.attr)
                 | false, false -> ())))
         scopes)
    t;
  match !best_attr with
  | None -> Styles.empty
  | Some a -> a
;;
