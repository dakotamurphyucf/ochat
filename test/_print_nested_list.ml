open Core
module WB = Webpage_markdown

let () =
  let html =
    "<ul><li>Fruit<ul><li>Apple</li><li>Banana</li></ul></li><li>Veg</li></ul>"
  in
  let md = Soup.parse html |> WB.Html_to_md.convert in
  Stdlib.print_endline "----";
  Stdlib.print_endline md;
  Stdlib.print_endline "----"
;;
