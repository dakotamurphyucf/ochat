open Core
module T = Chatml_typechecker
module Tests = Signature_check_tests

let require implementation signature expected =
  Tests.require (Tests.implementation implementation) signature expected
;;

let application implementation annotation =
  let _, _, types = Tests.implementation implementation in
  match
    T.Type_alias.elaborate
      types
      (Signature_scheme_tests.annotation annotation)
      ~checkpoint:ignore
  with
  | Ok ty -> ty
  | Error error -> failwith error.message
;;

let%expect_test "generic alias equality retains conditional argument and result arity" =
  List.iter
    [ "type 'a callback = 'a -> int", "type 'b callback = fun('b) -> int"
    ; "type 'a producer = int -> 'a", "type 'b producer = fun(int) -> 'b"
    ]
    ~f:(fun (implementation, signature) ->
      match Tests.check (Tests.implementation implementation) signature with
      | Error error -> print_endline error.message
      | Ok _ -> failwith "substitution-dependent aliases were declared equivalent");
  [%expect
    {|
    Cannot establish universal equivalence of type alias 'callback': normalized symbolic expansions differ
    Cannot establish universal equivalence of type alias 'producer': normalized symbolic expansions differ
    |}];
  (* Independently check the concrete substitutions that expose the differences.
     The universal matcher does not use these examples to establish equality. *)
  (match
     ( application "type 'a callback = 'a -> int" "unit callback"
     , application "type 'b callback = fun('b) -> int" "unit callback" )
   with
   | Fun ([], TInt), Fun ([ Unit ], TInt) -> ()
   | _ -> failwith "ordinary unit-argument alias normalization changed");
  match
    ( application "type 'a producer = int -> 'a" "(fun(bool) -> string) producer"
    , application "type 'b producer = fun(int) -> 'b" "(fun(bool) -> string) producer" )
  with
  | Fun ([ TInt; Boolean ], String), Fun ([ TInt ], Fun ([ Boolean ], String)) -> ()
  | _ -> failwith "ordinary function-result alias normalization changed"
;;

let%test_unit "private lexical aliases and alpha-renamed parameters remain transparent" =
  require
    {|type fixed = int
      type 'item private_box = {value:'item;fixed:fixed}
      type 'a box = 'a private_box|}
    "type 'renamed box = {fixed:int;value:'renamed}"
    true;
  require
    {|type 'a private_callback = 'a -> int
      type 'item callback = 'item private_callback|}
    "type 'renamed callback = 'renamed -> int"
    true;
  require
    {|type 'a private_callback = 'a -> int
      type 'item callback = 'item private_callback|}
    "type 'renamed callback = fun('renamed) -> int"
    false;
  require
    "type ('left, 'right) pair = {left:'left;right:'right}"
    "type ('second, 'first) pair = {left:'second;right:'first}"
    true
;;

let%test_unit "known arrow shapes normalize and deferred arrow chains flatten" =
  List.iter
    [ "type 'a callback = unit -> 'a -> int", "type 'b callback = 'b -> int"
    ; ( "type 'a callback = int -> bool -> 'a array"
      , "type 'b callback = fun(int, bool) -> 'b array" )
    ; ( "type 'a callback = unit -> fun('a, bool) -> int"
      , "type 'b callback = fun('b, bool) -> int" )
    ; "type 'a callback = int -> unit -> 'a", "type 'b callback = int -> 'b"
    ]
    ~f:(fun (implementation, signature) -> require implementation signature true)
;;

let%test_unit "recursive aliases rename binders without capturing argument recursion" =
  require
    {|type 'a private_tree = [ `Leaf('a) | `Branch('a private_tree, 'a private_tree) ]
      type 'a tree = 'a private_tree|}
    "type 'item tree = [ `Branch('item tree, 'item tree) | `Leaf('item) ]"
    true;
  (* The recursive argument belongs to [outer], while [tree]/[branch] allocate
     their own recursive binder around it. Expanding the private alias must keep
     those identities distinct even though its parameter receives recursion. *)
  require
    {|type 'a tree = {payload:'a;next:'a tree}
      type 'a branch = 'a tree
      type 'a outer = {leaf:('a outer) tree}|}
    {|type 'b branch = {payload:'b;next:'b branch}
      type 'c outer = {leaf:('c outer) branch}|}
    true;
  require
    "type ('a, 'b) tree = [ `Leaf('a) | `Branch(('a, 'b) tree) ]"
    "type ('x, 'y) tree = [ `Leaf('y) | `Branch(('x, 'y) tree) ]"
    false
;;

let%expect_test "different recursive unfoldings conservatively report unproved equality" =
  match
    Tests.check
      (Tests.implementation "type 'a stream = {head:'a;tail:'a stream}")
      "type 'b stream = {head:'b;tail:{head:'b;tail:'b stream}}"
  with
  | Ok _ ->
    failwith "symbolic comparison silently accepted a different recursive unfolding"
  | Error error ->
    print_endline error.message;
    [%expect
      {| Cannot establish universal equivalence of type alias 'stream': normalized symbolic expansions differ |}]
;;
