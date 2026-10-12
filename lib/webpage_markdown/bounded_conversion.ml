open Core
module B = Conversion_budget
module Limits = B.Limits
module Error = B.Error
module Accounting = B.Accounting

module Mode = struct
  type t =
    | Verbatim
    | Strip_html
    | Html_to_markdown
  [@@deriving equal, sexp_of]
end

module Output = struct
  type t =
    { contents : string
    ; accounting : Accounting.t
    }

  let contents t = t.contents
  let accounting t = t.accounting
end

let strip_html budget soup =
  let parts =
    B.soup_texts budget soup
    |> B.map budget ~f:(B.strip budget)
    |> B.filter budget ~f:(Fn.non String.is_empty)
  in
  let pieces =
    B.concat_map
      budget
      (B.mapi budget parts ~f:(fun i s ->
         B.nodes budget 2;
         if i = 0
         then [ Conversion_output.text budget s ]
         else [ Conversion_output.text budget "\n"; Conversion_output.text budget s ]))
      ~f:Fn.id
  in
  Conversion_output.to_string budget (Conversion_output.concat budget pieces)
;;

let convert mode ~contents ~limits ~checkpoint =
  B.run limits ~input_bytes:(String.length contents) ~checkpoint ~f:(fun budget ->
    match mode with
    | Mode.Verbatim ->
      B.scan budget contents;
      B.output_bytes budget (String.length contents);
      contents
    | Strip_html | Html_to_markdown ->
      let soup =
        try Conversion_dom.parse budget contents with
        | Soup.Parse_error _ ->
          B.reject_html budget;
          assert false
      in
      (match mode with
       | Strip_html -> strip_html budget soup
       | Html_to_markdown ->
         Html_to_md_core.convert budget soup |> Md_render_core.to_string budget
       | Verbatim -> assert false))
  |> Result.map ~f:(fun (contents, accounting) -> ({ contents; accounting } : Output.t))
;;
