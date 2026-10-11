open Core
module Source = Chatml_source
module Parse = Chatml.Chatml_parse
module Sites = Chatml.Chatml_syntax_sites
module Name = Chatml.Chatml_syntax_name
module L = Chatml.Chatml_lang
module Type = Chatml.Chatml_type_expr
module Signature = Chatml.Chatml_module_signature
module Parser = Chatml.Chatml_parser.Make (Sites.Disabled)

let checkpoint () = ()

let capture path contents =
  let id =
    Source.Id.create ~package_namespace:"sites" ~relative_path:path |> Or_error.ok_exn
  in
  Source.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let limits sites bytes =
  Sites.Limits.create ~max_sites:sites ~max_name_bytes:bytes |> Or_error.ok_exn
;;

let generous = limits 100 1000

let unwrap = function
  | Ok value -> value
  | Error _ -> failwith "unexpected parse failure"
;;

let table_of_unit unit = Parse.Unit.sites unit |> Option.value_exn

let row table name =
  let id = Name.site name |> Option.value_exn in
  Sites.find table id |> Option.value_exn
;;

let require_site table name spelling role start_byte end_byte =
  let site = row table name in
  let location = Sites.Site.location site in
  let span = Source.Location.span location in
  assert (String.equal (Name.name name) spelling);
  assert (String.equal (Sites.Site.name site) spelling);
  assert (Sites.Role.equal (Sites.Site.role site) role);
  assert (Int.equal span.left.offset start_byte);
  assert (Int.equal span.right.offset end_byte);
  assert (
    Source.Id.equal (Source.Location.source location) (Source.id (Sites.source table)));
  assert (
    Source.Digest.equal
      (Source.Location.source_digest location)
      (Source.digest (Sites.source table)))
;;

(* These literal byte coordinates are independent of parser/site collection.
   The UTF-8 comment is eight bytes and its CRLF is two; the type starts at 10. *)
let type_text = "(* π *)\r\ntype ('a, 'b) pair = fun(A.t, B.Inner.t) -> C.t\r\n"

let type_names unit =
  match (Parse.Unit.program unit).stmts with
  | [ { value =
          SType
            { name
            ; parameters = [ a; b ]
            ; body = { desc = TEFunction ([ first; second ], result); _ }
            }
      ; _
      }
    ] ->
    let components syntax =
      match syntax.Type.desc with
      | TEName name -> name.modules @ [ name.name ]
      | _ -> assert false
    in
    [ a; b; name ] @ components first @ components second @ components result
  | _ -> assert false
;;

let type_oracle =
  [ "a", Sites.Role.Type_parameter_binder, 16, 18
  ; "b", Type_parameter_binder, 20, 22
  ; "pair", Type_binder, 24, 28
  ; "A", Module_reference, 35, 36
  ; "t", Type_reference, 37, 38
  ; "B", Module_reference, 40, 41
  ; "Inner", Module_reference, 42, 47
  ; "t", Type_reference, 48, 49
  ; "C", Module_reference, 54, 55
  ; "t", Type_reference, 56, 57
  ]
;;

let%expect_test "nested qualified names retain exact authored token bytes" =
  let source = capture "types.chatml" type_text in
  let parsed =
    Parse.parse_captured_with_sites source ~limits:generous ~checkpoint |> unwrap
  in
  let table = table_of_unit parsed in
  [%test_eq: int] (Sites.site_count table) 10;
  [%test_eq: int] (Sites.name_bytes table) 17;
  List.iter2_exn
    (type_names parsed)
    type_oracle
    ~f:(fun name (spelling, role, left, right) ->
      require_site table name spelling role left right;
      let location = Sites.Site.location (row table name) in
      let span = Source.Location.span location in
      [%test_eq: int] span.left.line 2;
      [%test_eq: int] span.left.column (left - 10);
      let token = Source.read source location |> Or_error.ok_exn in
      print_s
        [%sexp
          (spelling : string)
        , (role : Sites.Role.t)
        , (left : int)
        , (right : int)
        , (token : string)]);
  [%expect
    {|
    (a Type_parameter_binder 16 18 'a)
    (b Type_parameter_binder 20 22 'b)
    (pair Type_binder 24 28 pair)
    (A Module_reference 35 36 A)
    (t Type_reference 37 38 t)
    (B Module_reference 40 41 B)
    (Inner Module_reference 42 47 Inner)
    (t Type_reference 48 49 t)
    (C Module_reference 54 55 C)
    (t Type_reference 56 57 t)
    |}]
;;

let%test_unit "recursive and repeated match names are keyed on their actual nodes" =
  let text =
    "let rec first x = x and second y = y;;\r\n\
     let out = match { item = 1 } with | { item = left } -> left | { item = right; _ } \
     -> right;;"
  in
  let parsed =
    Parse.parse_captured_with_sites
      (capture "patterns.chatml" text)
      ~limits:generous
      ~checkpoint
    |> unwrap
  in
  let table = table_of_unit parsed in
  match (Parse.Unit.program parsed).stmts with
  | [ { value = SLetRec [ (first, _); (second, _) ]; _ }
    ; { value =
          SLet
            ( _
            , { value =
                  EMatch
                    ( { value = ERecord [ (literal_label, _) ]; _ }
                    , [ { pat = PRecord ([ (first_label, PVar left) ], false); _ }
                      ; { pat = PRecord ([ (second_label, PVar right) ], true); _ }
                      ] )
              ; _
              } )
      ; _
      }
    ] ->
    List.iter
      [ first, "first", Sites.Role.Value_binder, 8, 13
      ; second, "second", Value_binder, 24, 30
      ; literal_label, "item", Field_label, 58, 62
      ; first_label, "item", Field_label, 78, 82
      ; left, "left", Value_binder, 85, 89
      ; second_label, "item", Field_label, 104, 108
      ; right, "right", Value_binder, 111, 116
      ]
      ~f:(fun (name, spelling, role, start_byte, end_byte) ->
        require_site table name spelling role start_byte end_byte);
    assert (Name.equal first_label second_label);
    assert (Int.equal (Name.compare first_label second_label) 0);
    assert (
      not
        (Sites.Id.equal
           (Option.value_exn (Name.site first_label))
           (Option.value_exn (Name.site second_label))));
    [%test_eq: int] (Sites.site_count table) 14
  | _ -> assert false
;;

let%test_unit "companion declarations and constraints keep their own token coordinates" =
  let text =
    "type 'a id = 'a\r\n\
     val run : fun(A.t, B.t) -> C.t where equality('a)\r\n\
     module M : sig val value : int end"
  in
  let parsed =
    Parse.parse_captured_interface_with_sites
      (capture "types.chatmli" text)
      ~limits:generous
      ~checkpoint
    |> unwrap
  in
  let table = Parse.Interface.sites parsed |> Option.value_exn in
  match Parse.Interface.signature parsed with
  | [ { desc = Type { name = id; parameters = [ parameter ]; _ }; _ }
    ; { desc = Value { name = run; requirements = [ requirement ]; _ }; _ }
    ; { desc =
          Module
            { name = module_name
            ; signature = [ { desc = Value { name = value; _ }; _ } ]
            }
      ; _
      }
    ] ->
    List.iter
      [ parameter, "a", Sites.Role.Type_parameter_binder, 5, 7
      ; id, "id", Type_binder, 8, 10
      ; run, "run", Signature_value_binder, 21, 24
      ; requirement.name, "equality", Constraint_name, 54, 62
      ; requirement.parameter, "a", Constraint_parameter_reference, 63, 65
      ; module_name, "M", Module_binder, 75, 76
      ; value, "value", Signature_value_binder, 87, 92
      ]
      ~f:(fun (name, spelling, role, start_byte, end_byte) ->
        require_site table name spelling role start_byte end_byte);
    [%test_eq: int] (Sites.site_count table) 14
  | _ -> assert false
;;

let%test_unit "foreign invocation IDs and unlocated decoded names never acquire ownership"
  =
  let source = capture "types.chatml" type_text in
  let parse source =
    Parse.parse_captured_with_sites source ~limits:generous ~checkpoint |> unwrap
  in
  let first = parse source in
  let second = parse source in
  let other = parse (capture "other.chatml" type_text) in
  List.iter (type_names first) ~f:(fun name ->
    let id = Name.site name |> Option.value_exn in
    assert (Option.is_none (Sites.find (table_of_unit second) id));
    assert (Option.is_none (Sites.find (table_of_unit other) id));
    let decoded = Name.t_of_sexp (Name.sexp_of_t name) in
    assert (Name.equal name decoded);
    assert (Option.is_none (Name.site decoded)));
  let ordinary = Parse.parse_captured source ~checkpoint |> unwrap in
  assert (Option.is_none (Parse.Unit.sites ordinary));
  assert (
    List.for_all (type_names ordinary) ~f:(fun name -> Option.is_none (Name.site name)));
  assert (
    Sexp.equal
      (L.sexp_of_program (Parse.Unit.program first))
      (L.sexp_of_program (Parse.Unit.program ordinary)))
;;

let%test_unit "module, open, selector and tag sites exclude generated task sugar" =
  let text =
    "module M = struct let r = { item = `Tag } end;;\r\nopen M;;\r\nlet out = r.item"
  in
  let parsed =
    Parse.parse_captured_with_sites
      (capture "names.chatml" text)
      ~limits:generous
      ~checkpoint
    |> unwrap
  in
  let table = table_of_unit parsed in
  (match (Parse.Unit.program parsed).stmts with
   | [ { value =
           SModule
             { name = module_name
             ; body =
                 [ { value =
                       SLet
                         ( _
                         , { value =
                               ERecord [ (label, { value = EVariant (tag, []); _ }) ]
                           ; _
                           } )
                   ; _
                   }
                 ]
             ; _
             }
       ; _
       }
     ; { value = SOpen opened; _ }
     ; { value = SLet (_, { value = EFieldGet (_, selected); _ }); _ }
     ] ->
     List.iter
       [ module_name, "M", Sites.Role.Module_binder, 7, 8
       ; label, "item", Field_label, 28, 32
       ; tag, "Tag", Variant_tag, 35, 39
       ; opened, "M", Module_reference, 54, 55
       ; selected, "item", Field_label, 71, 75
       ]
       ~f:(fun (name, spelling, role, start_byte, end_byte) ->
         require_site table name spelling role start_byte end_byte);
     [%test_eq: int] (Sites.site_count table) 8
   | _ -> assert false);
  let parsed =
    Parse.parse_captured_with_sites
      (capture
         "sugar.chatml"
         "let work = let* value = Task.return(1) in Task.return(value)")
      ~limits:generous
      ~checkpoint
    |> unwrap
  in
  let table = table_of_unit parsed in
  match (Parse.Unit.program parsed).stmts with
  | [ { value =
          SLet (_, { value = EApp ({ value = EFieldGet (_, generated_method); _ }, _); _ })
      ; _
      }
    ] ->
    assert (String.equal (Name.name generated_method) "bind");
    assert (Option.is_none (Name.site generated_method));
    let actual =
      Sites.sites table
      |> List.map ~f:(fun site ->
        let span = Sites.Site.location site |> Source.Location.span in
        Sites.Site.name site, Sites.Site.role site, span.left.offset, span.right.offset)
      |> List.sort ~compare:(fun (_, _, left, _) (_, _, right, _) ->
        Int.compare left right)
    in
    assert (
      List.equal
        (fun (name, role, left, right)
          (expected_name, expected_role, expected_left, expected_right) ->
           String.equal name expected_name
           && Sites.Role.equal role expected_role
           && Int.equal left expected_left
           && Int.equal right expected_right)
        actual
        [ "work", Value_binder, 4, 8
        ; "value", Value_binder, 16, 21
        ; "Task", Value_reference, 24, 28
        ; "return", Field_label, 29, 35
        ; "Task", Value_reference, 42, 46
        ; "return", Field_label, 47, 53
        ; "value", Value_reference, 54, 59
        ])
  | _ -> assert false
;;

let%test_unit "metadata cannot alter checked or lowered language behavior" =
  let source =
    capture
      "program.chatml"
      "type pair = int * string;; let rec first x = x and second y = first(y);; module M \
       = struct let field = { value = 7 } end;; open M;; let result = match `Pair(3, \
       \"ok\") with | `Pair(left, right) -> (|first(left), right|);; let output = \
       field.value"
  in
  let ordinary = Parse.parse_captured source ~checkpoint |> unwrap in
  let located =
    Parse.parse_captured_with_sites source ~limits:generous ~checkpoint |> unwrap
  in
  let lower parsed =
    let checked =
      Chatml_typechecker.check_captured_program_with_surface
        ~checkpoint
        Chatml.Chatml_builtin_surface.core_surface
        parsed
      |> unwrap
    in
    Chatml_resolver.resolve_captured_program checked parsed |> unwrap
  in
  assert (
    Sexp.equal
      (L.sexp_of_resolved_program (lower ordinary))
      (L.sexp_of_resolved_program (lower located)))
;;

let%test_unit "site and spelling-byte bounds are exact and failed admissions are reusable"
  =
  let source = capture "types.chatml" type_text in
  let parse limits = Parse.parse_captured_with_sites source ~limits ~checkpoint in
  assert (Result.is_ok (parse (limits 10 17)));
  (match parse (limits 9 17) with
   | Error (Sites Site_limit) -> ()
   | _ -> assert false);
  (match parse (limits 10 16) with
   | Error (Sites Name_byte_limit) -> ()
   | _ -> assert false);
  assert (Result.is_ok (parse (limits 10 17)));
  assert (Result.is_error (Sites.Limits.create ~max_sites:0 ~max_name_bytes:17));
  assert (Result.is_error (Sites.Limits.create ~max_sites:10 ~max_name_bytes:0));
  match
    Parse.parse_captured_with_sites
      (capture "bad.chatml" "type broken =")
      ~limits:generous
      ~checkpoint
  with
  | Error (Syntax diagnostic) -> assert (Option.is_some diagnostic.Chatml_diagnostic.site)
  | _ -> assert false
;;

let sweep run =
  let count = ref 0 in
  assert (Result.is_ok (run (fun () -> Int.incr count)));
  assert (!count > 0);
  List.iter [ Failure "checkpoint"; Parser.Error ] ~f:(fun original ->
    for position = 1 to !count do
      let seen = ref 0 in
      match
        run (fun () ->
          Int.incr seen;
          if Int.equal !seen position then raise original)
      with
      | _ -> failwith "checkpoint interruption was swallowed"
      | exception actual -> assert (phys_equal original actual)
    done);
  assert (Result.is_ok (run checkpoint))
;;

let%test_unit
    "every observed enabled and disabled checkpoint preserves exception identity"
  =
  let source = capture "types.chatml" type_text in
  sweep (fun checkpoint ->
    Parse.parse_captured_with_sites source ~limits:generous ~checkpoint);
  sweep (fun checkpoint -> Parse.parse_captured source ~checkpoint);
  let source =
    capture "types.chatmli" "type 'a id = 'a val run : fun('a) -> 'a where equality('a)"
  in
  sweep (fun checkpoint ->
    Parse.parse_captured_interface_with_sites source ~limits:generous ~checkpoint);
  sweep (fun checkpoint -> Parse.parse_captured_interface source ~checkpoint)
;;
