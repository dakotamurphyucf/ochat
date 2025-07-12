open Core
module WB = Webpage_markdown

let%expect_test "simple heading + paragraph" =
  let html = "<html><body><h1>Hello</h1><p>World</p></body></html>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|# Hello

World|}]
;;

let%expect_test "unordered list" =
  let html = "<ul><li>Apple</li><li>Banana<li>Cherry</li></li></ul>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|* Apple

* Banana

* Cherry|}]
;;

let%expect_test "inline code with backtick" =
  let html = "<p>Use <code>a`b</code> as key</p>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect {|Use `` a`b `` as key|}]
;;

let%expect_test "link with closing bracket in text" =
  let html = "<p><a href=\"/path\">foo ] bar</a></p>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect {|[foo \] bar](/path)|}]
;;

let%expect_test "ordered list with start" =
  let html = "<ol start=\"3\"><li>Three</li><li>Four</li></ol>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|3. Three

4. Four|}]
;;

let%expect_test "nested unordered list" =
  let html =
    "<ul><li>Fruit<ul><li>Apple</li><li>Banana</li></ul></li><li>Veg</li></ul>"
  in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|* Fruit

  * Apple

  * Banana

* Veg|}]
;;

let%expect_test "multi-paragraph list item" =
  let html = "<ul><li><p>Line one</p><p>Line two</p></li><li>Second item</li></ul>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|* Line one

  Line two

* Second item|}]
;;

(* Table support test re-enabled after whitespace stabilisation *)
let%expect_test "simple table" =
  let html =
    "<table><tr><th>A</th><th>B</th></tr><tr><td>1</td><td>2</td></tr></table>"
  in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|
| A | B |

| --- | --- |

| 1 | 2 |
|}]
;;

let%expect_test "image element" =
  let html = "<p><img src=\"/foo.png\" alt=\"Foo\"></p>" in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect {|![Foo](/foo.png)|}]
;;

let%expect_test "nested ordered list multi-paragraph item" =
  let html =
    "<ol \
     start=\"2\"><li><p>Alpha</p><p>Beta</p><ol><li>One</li><li>Two</li></ol></li></ol>"
  in
  let soup = Soup.parse html in
  let md = WB.Html_to_md.convert soup |> WB.Md_render.to_string in
  print_string md;
  [%expect
    {|2. Alpha

  Beta

  1. One

  2. Two|}]
;;
