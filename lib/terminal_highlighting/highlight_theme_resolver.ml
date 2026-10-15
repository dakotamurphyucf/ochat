open Core

type cache =
  { attrs : Notty.A.t String.Map.t
  ; entries : int
  ; key_bytes : int
  }

type t =
  { theme : Highlight_theme.t
  ; cache : cache Atomic.t
  ; max_entries : int
  ; max_key_bytes : int
  }

let create_with_limits ~theme ~max_entries ~max_key_bytes =
  if max_entries < 0 || max_key_bytes < 0
  then invalid_arg "negative highlight cache limit";
  { theme
  ; cache = Atomic.make { attrs = String.Map.empty; entries = 0; key_bytes = 0 }
  ; max_entries
  ; max_key_bytes
  }
;;

let create ~theme = create_with_limits ~theme ~max_entries:4096 ~max_key_bytes:262_144

let digits value =
  let rec loop value count =
    if value < 10 then count else loop (value / 10) (count + 1)
  in
  loop value 1
;;

(* Preflight total key bytes before creating a buffer. Length prefixes make
   ["foo"; "keyword"] distinct from ["foo.keyword"] and permit arbitrary bytes. *)
let key_of_scopes ~max_key_bytes scopes =
  let rec size used = function
    | [] -> Some used
    | scope :: rest ->
      let length = String.length scope in
      let prefix_bytes = digits length + 1 in
      let remaining = max_key_bytes - used in
      if prefix_bytes > remaining || length > remaining - prefix_bytes
      then None
      else size (used + prefix_bytes + length) rest
  in
  match size 0 scopes with
  | None -> None
  | Some key_bytes ->
    let buffer = Buffer.create key_bytes in
    List.iter scopes ~f:(fun scope ->
      Buffer.add_string buffer (Int.to_string (String.length scope));
      Buffer.add_char buffer ':';
      Buffer.add_string buffer scope);
    Some (Buffer.contents buffer, key_bytes)
;;

let attr_of_scopes t ~scopes =
  let scopes = List.dedup_and_sort scopes ~compare:String.compare in
  let compute () = Highlight_theme.attr_of_scopes t.theme ~scopes in
  match key_of_scopes ~max_key_bytes:t.max_key_bytes scopes with
  | None -> compute ()
  | Some (key, key_bytes) ->
    let current = Atomic.get t.cache in
    (match Map.find current.attrs key with
     | Some attr -> attr
     | None ->
       let attr = compute () in
       if
         current.entries < t.max_entries
         && key_bytes <= t.max_key_bytes - current.key_bytes
       then (
         let next =
           { attrs = Map.set current.attrs ~key ~data:attr
           ; entries = current.entries + 1
           ; key_bytes = current.key_bytes + key_bytes
           }
         in
         ignore (Atomic.compare_and_set t.cache current next : bool));
       attr)
;;

let%test_unit "canonical key distinguishes scope groups and ordering" =
  let key scopes = key_of_scopes ~max_key_bytes:256 scopes |> Option.value_exn |> fst in
  assert (not (String.equal (key [ "foo"; "keyword" ]) (key [ "foo.keyword" ])));
  let theme = Highlight_theme.github_dark in
  let resolver = create_with_limits ~theme ~max_entries:2 ~max_key_bytes:256 in
  let combined = attr_of_scopes resolver ~scopes:[ "foo"; "keyword" ] in
  let dotted = attr_of_scopes resolver ~scopes:[ "foo.keyword" ] in
  assert (not (Notty.A.equal combined dotted));
  let cases =
    [ [ "foo"; "keyword" ]
    ; [ "foo.keyword" ]
    ; [ "keyword"; "foo"; "foo" ]
    ; [ "source.ocaml"; "keyword" ]
    ]
  in
  List.iter cases ~f:(fun scopes ->
    let expected = Highlight_theme.attr_of_scopes theme ~scopes in
    let cold = attr_of_scopes resolver ~scopes in
    let warm = attr_of_scopes resolver ~scopes in
    assert (Notty.A.equal cold expected && Notty.A.equal warm expected));
  let scope =
    Quickcheck.Generator.of_list
      [ "foo"; "keyword"; "foo.keyword"; "source.ocaml"; "string"; ""; "a:b" ]
  in
  let generator = Quickcheck.Generator.list_with_length 5 scope in
  Quickcheck.test ~trials:80 generator ~f:(fun scopes ->
    let generated_resolver =
      create_with_limits ~theme ~max_entries:2 ~max_key_bytes:256
    in
    let expected = Highlight_theme.attr_of_scopes theme ~scopes in
    let cold = attr_of_scopes generated_resolver ~scopes in
    let warm = attr_of_scopes generated_resolver ~scopes in
    let permuted = attr_of_scopes generated_resolver ~scopes:(List.rev scopes) in
    let duplicated = attr_of_scopes generated_resolver ~scopes:(scopes @ scopes) in
    assert (List.for_all [ cold; warm; permuted; duplicated ] ~f:(Notty.A.equal expected));
    assert (Int.equal (Atomic.get generated_resolver.cache).entries 1));
  let final = Atomic.get resolver.cache in
  assert (final.entries <= 2 && final.key_bytes <= 256)
;;

let%test_unit "oversized keys and saturation do not grow retained state" =
  let resolver =
    create_with_limits ~theme:Highlight_theme.github_dark ~max_entries:1 ~max_key_bytes:12
  in
  let oversized = [ String.make 1000 'x' ] in
  ignore (attr_of_scopes resolver ~scopes:oversized : Notty.A.t);
  assert (Int.equal (Atomic.get resolver.cache).entries 0);
  List.iter [ [ "keyword" ]; [ "string" ]; [ "comment" ] ] ~f:(fun scopes ->
    ignore (attr_of_scopes resolver ~scopes : Notty.A.t));
  let final = Atomic.get resolver.cache in
  assert (final.entries <= 1 && final.key_bytes <= 12)
;;

let%test_unit "concurrent readers preserve pure results and cache bounds" =
  let theme = Highlight_theme.github_dark in
  let resolver = create_with_limits ~theme ~max_entries:64 ~max_key_bytes:4096 in
  let scope_sets =
    [ [ "keyword"; "source.ocaml"; "keyword.operator" ]
    ; [ "string"; "source.ocaml" ]
    ; [ "foo"; "keyword" ]
    ; [ "foo.keyword" ]
    ]
  in
  let reader () =
    for _ = 1 to 100 do
      List.iter scope_sets ~f:(fun scopes ->
        let expected = Highlight_theme.attr_of_scopes theme ~scopes in
        let actual = attr_of_scopes resolver ~scopes in
        assert (Notty.A.equal actual expected))
    done
  in
  let domains = List.init 2 ~f:(fun _ -> Domain.spawn reader) in
  reader ();
  List.iter domains ~f:Domain.join;
  let final = Atomic.get resolver.cache in
  assert (final.entries <= 64 && final.key_bytes <= 4096)
;;
