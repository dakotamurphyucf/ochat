open Core
open Support

let%test_unit "recursive alias instantiations preserve lexical binders and leaf types" =
  List.iter [ "tree"; "renamed" ] ~f:(fun name ->
    let prefix =
      Printf.sprintf "type 'a %s = [ `Leaf('a) | `Branch('a %s, 'a %s) ]\n" name name name
    in
    for depth = 1 to 4 do
      let annotation =
        String.concat ~sep:" " ("int" :: List.init depth ~f:(fun _ -> name))
      in
      let nested atom =
        List.fold (List.init depth ~f:Fn.id) ~init:atom ~f:(fun value _ ->
          "`Leaf(" ^ value ^ ")")
      in
      List.iter
        [ "7", true; "true", false ]
        ~f:(fun (atom, valid) ->
          let source = prefix ^ "let result : " ^ annotation ^ " = " ^ nested atom in
          require source valid;
          if valid then ignore (run source : L.value))
    done)
;;

let%test_unit "alias validity is independent of parameter names and declaration context" =
  let constructor_names =
    "type array = int array\n\
     type task = int task\n\
     let values : array = [1]\n\
     let pending : task = Task.pure(1)\n"
  in
  require constructor_names true;
  List.iter [ "a"; "item" ] ~f:(fun parameter ->
    let p = "'" ^ parameter in
    List.iter
      [ "type (" ^ p ^ ", " ^ p ^ ") bad = int"
      ; "type " ^ p ^ " bad = 'unbound"
      ; "type " ^ p ^ " bad = " ^ p ^ " bad"
      ; "type " ^ p ^ " bad = [ `More((" ^ p ^ " array) bad) ]"
      ; "type " ^ p ^ " one = " ^ p ^ "\nlet value : one = 1"
      ; "type " ^ p ^ " one = " ^ p ^ "\nlet value : (int, bool) one = 1"
      ; "type ("
        ^ p
        ^ ", 'b) pair = {x:"
        ^ p
        ^ ";y:'b}\nlet value : int pair = {x=1;y=true}"
      ; "let identity : fun(" ^ p ^ ") -> " ^ p ^ " = fun x -> x"
      ]
      ~f:(fun source ->
        require source false;
        require ("type earlier = int\n" ^ source) false;
        require (constructor_names ^ source) false;
        (* A rejected compilation must not leave its type namespace installed. *)
        require "type bad = int\nlet result : bad = 1" true))
;;

let%test_unit "alias conversion cannot bypass equality, exactness or mutable constraints" =
  List.iter
    [ {|type 'a box = {value:'a}
        let equal x y = x == y
        let box : (fun(int) -> int) box = {value = fun x -> x}
        let result = equal(box, box)|}
    ; {|type 'a box = {value:'a}
        let take x = let exact : int box = x in exact.value
        let result = take({value=1;extra=true})|}
    ; {|type 'a box = {value:'a}
        let cell : int box array = [{value=1}]
        let result = cell[0] <- {value=true}|}
    ]
    ~f:(fun source -> require source false);
  require
    {|type 'a box = {value:'a}
    let get r = r.value
    let first : int box = {value=1}
    let result = get(first) + get({value=2;extra=true})|}
    true
;;

let%test_unit
    "alias traversal cooperates with cancellation without interpreting declarations"
  =
  let exception Cancelled in
  let declaration =
    let rec nested depth =
      if depth = 0 then "'a" else "{left:" ^ nested (depth - 1) ^ ";right:'a}"
    in
    match (parse ("type 'a nested = " ^ nested 80)).stmts with
    | [ { value = L.SType declaration; _ } ] -> declaration
    | _ -> failwith "Expected one declaration"
  in
  let polls = ref 0 in
  let checkpoint () =
    Int.incr polls;
    if !polls = 20 then raise Cancelled
  in
  match T.Type_alias.declare T.Type_alias.empty declaration ~checkpoint with
  | _ -> failwith "Alias elaboration ignored cancellation"
  | exception Cancelled -> [%test_eq: int] 20 !polls
;;
