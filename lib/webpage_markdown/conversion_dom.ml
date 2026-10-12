open Core
module B = Conversion_budget

let parse budget contents =
  let body_attributes = ref [] in
  let report _location = function
    | `Misnested_tag ("body", _, attributes) ->
      List.iter attributes ~f:(fun (n, v) ->
        B.scan budget n;
        B.scan budget v;
        B.nodes budget 4);
      List.iter !body_attributes ~f:(fun _ -> B.node budget);
      body_attributes := !body_attributes @ attributes
    | _ -> ()
  in
  let level = ref 0 in
  let admit signal =
    B.work budget 1;
    (match signal with
     | `Start_element ((ns, name), attributes) ->
       incr level;
       B.depth budget !level;
       B.nodes budget 8;
       B.scan budget ns;
       B.scan budget name;
       List.iter attributes ~f:(fun ((ns, name), value) ->
         B.nodes budget 4;
         B.scan budget ns;
         B.scan budget name;
         B.scan budget value)
     | `End_element -> decr level
     | `Text parts ->
       B.nodes budget 8;
       List.iter parts ~f:(fun part ->
         B.node budget;
         B.scan budget part)
     | `Comment text -> B.scan budget text
     | `Doctype _ -> B.nodes budget 4
     | `Xml _ | `PI _ -> B.reject_html budget);
    signal
  in
  B.nodes budget 8;
  (* Soup document root and tree-builder root list. *)
  let signals =
    Markup.string contents
    |> Markup.map (fun c ->
      B.work budget 1;
      c)
    |> Markup.parse_html ~report
    |> Markup.signals
    |> Markup.map admit
  in
  let soup = Soup.from_signals signals in
  (* Match Soup.parse's recovered body attributes without bypassing signal
     admission or retaining a second document. Every new attribute is admitted. *)
  Soup.iter
    (fun node ->
       B.work budget 1;
       match Soup.element node with
       | Some element when String.equal (Soup.name element) "body" ->
         List.iter !body_attributes ~f:(fun (name, value) ->
           B.work budget 1;
           if Option.is_none (B.soup_attribute budget name element)
           then (
             Soup.fold_attributes
               (fun () old_name _ ->
                  B.scan budget old_name;
                  B.node budget)
               ()
               element;
             B.nodes budget 2;
             Soup.set_attribute name value element))
       | None | Some _ -> ())
    (Soup.descendants soup);
  soup
;;
