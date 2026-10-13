open Core
module Snapshot = Language_highlighting.Highlight_snapshot
module Theme = Terminal_highlighting.Highlight_theme
module Resolver = Terminal_highlighting.Highlight_theme_resolver

module Limits = struct
  type t =
    { max_lines : int
    ; max_line_bytes : int
    }

  let create ~max_lines ~max_line_bytes =
    if max_lines <= 0 || max_line_bytes <= 0
    then Or_error.error_string "terminal source limits must be positive"
    else Ok { max_lines; max_line_bytes }
  ;;
end

module Error = struct
  type t =
    | Too_many_lines
    | Line_too_long of int
  [@@deriving compare, equal, sexp_of]
end

type span = Terminal_highlighting.Highlight_tm_engine.span

type t =
  { snapshot : Snapshot.t
  ; lines : span list list
  }

let snapshot t = t.snapshot
let lines t = t.lines

let scopes_of_role = function
  | Snapshot.Role.Tag_name -> [ "entity.name.tag.chatmd" ]
  | Attribute_name -> [ "entity.other.attribute-name.chatmd" ]
  | Attribute_value -> [ "string.quoted.chatmd" ]
  | Keyword -> [ "keyword.control.chatml" ]
  | Identifier -> [ "variable.other.chatml" ]
  | Type_variable -> [ "variable.other.type.chatml" ]
  | Variant_tag -> [ "constant.other.variant.chatml" ]
  | String_literal -> [ "string.quoted.double.chatml" ]
  | Number_literal -> [ "constant.numeric.chatml" ]
  | Boolean_literal -> [ "constant.language.boolean.chatml" ]
  | Comment -> [ "comment.block" ]
  | Raw_delimiter -> [ "punctuation.definition.raw.chatmd" ]
  | Raw_text -> [ "string.unquoted.raw.chatmd" ]
  | Script_body -> [ "string.unquoted.script.chatmd" ]
  | Operator -> [ "keyword.operator.chatml" ]
  | Punctuation -> [ "punctuation" ]
;;

let line_intervals text limits ~checkpoint =
  let len = String.length text in
  let i = ref 0 in
  let line_start = ref 0 in
  let line_number = ref 0 in
  let intervals_rev = ref [] in
  let error = ref None in
  while !i < len && Option.is_none !error do
    if !i mod 4096 = 0 then checkpoint ();
    let c = text.[!i] in
    if Char.equal c '\n' || Char.equal c '\r'
    then
      if !i - !line_start > limits.Limits.max_line_bytes
      then error := Some (Error.Line_too_long !line_number)
      else (
        intervals_rev := (!line_start, !i) :: !intervals_rev;
        let next =
          if Char.equal c '\r' && !i + 1 < len && Char.equal text.[!i + 1] '\n'
          then !i + 2
          else !i + 1
        in
        i := next;
        line_start := next;
        Int.incr line_number;
        if !line_number >= limits.max_lines then error := Some Error.Too_many_lines)
    else if !i - !line_start >= limits.max_line_bytes
    then error := Some (Error.Line_too_long !line_number)
    else Int.incr i
  done;
  match !error with
  | Some error -> Error error
  | None ->
    if len - !line_start > limits.max_line_bytes
    then Error (Error.Line_too_long !line_number)
    else Ok (List.rev ((!line_start, len) :: !intervals_rev))
;;

let render snapshot ~theme ~limits ~checkpoint =
  let source = Snapshot.source_contents snapshot in
  match line_intervals source limits ~checkpoint with
  | Error _ as error -> error
  | Ok intervals ->
    let resolver = Resolver.create ~theme in
    let remaining = ref (Snapshot.spans snapshot) in
    let fragment_work = ref 0 in
    let poll_fragment () =
      Int.incr fragment_work;
      if !fragment_work mod 4096 = 0 then checkpoint ()
    in
    let rows =
      List.map intervals ~f:(fun (line_start, line_end) ->
        checkpoint ();
        let pos = ref line_start in
        let row_rev = ref [] in
        while !pos < line_end do
          while
            match !remaining with
            | span :: _ -> Snapshot.Range.end_byte (Snapshot.Span.range span) <= !pos
            | [] -> false
          do
            poll_fragment ();
            remaining := List.tl_exn !remaining
          done;
          let end_byte, attr =
            match !remaining with
            | span :: _ ->
              let range = Snapshot.Span.range span in
              if Snapshot.Range.start_byte range > !pos
              then Int.min line_end (Snapshot.Range.start_byte range), Notty.A.empty
              else
                ( Int.min line_end (Snapshot.Range.end_byte range)
                , Resolver.attr_of_scopes
                    resolver
                    ~scopes:(scopes_of_role (Snapshot.Span.role span)) )
            | [] -> line_end, Notty.A.empty
          in
          assert (end_byte > !pos);
          row_rev
          := (attr, String.sub source ~pos:!pos ~len:(end_byte - !pos)) :: !row_rev;
          poll_fragment ();
          pos := end_byte
        done;
        List.rev !row_rev)
    in
    Ok { snapshot; lines = rows }
;;
