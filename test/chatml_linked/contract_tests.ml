open Core
module L = Chatml.Chatml_lang
module B = Chatml.Chatml_builtin_spec
module Surface = Chatml.Chatml_builtin_surface
module Static = Chatml.Chatml_static_surface
module C = Chatml.Chatml_compiler_contract

let checkpoint () = ()

(* Independent unchanged legacy schema; do not compare two aliases of the new encoder. *)
let legacy_identity target surface requirements =
  let bindings entries = List.map entries ~f:(fun (b : B.builtin) -> b.name, b.scheme) in
  [%sexp
    ("ochat.chatml.compiler.v1" : string)
  , (target : C.Target.t)
  , (bindings surface.Surface.globals : (string * B.ty) list)
  , (List.map surface.modules ~f:(fun (m : B.builtin_module) ->
       m.name, bindings m.exports)
     : (string * (string * B.ty) list) list)
  , (List.map surface.type_aliases ~f:(fun (a : Surface.builtin_type_alias) ->
       a.name, a.body)
     : (string * B.ty) list)
  , (requirements : (string * B.ty) list)]
;;

let%test_unit "standard static contracts preserve exact old compiler identities" =
  let module X = Chatml.Chatml_extension_surface in
  List.iter
    [ C.Target.One_off_v1, X.one_off_v1, X.one_off_entrypoints
    ; Tool_v1, X.tool_v1, X.tool_entrypoints
    ; Moderator_v1, X.moderator_v1, X.moderator_entrypoints
    ; Delegated_moderator_v1, X.delegated_moderator_v1, X.moderator_entrypoints
    ]
    ~f:(fun (target, surface, requirements) ->
      let actual = C.for_target target ~checkpoint in
      let expected = legacy_identity target surface requirements in
      assert (Sexp.equal expected (C.identity actual ~checkpoint));
      assert (
        Chatml_source.Digest.equal
          (C.fingerprint actual ~checkpoint)
          (Chatml_source.Digest.of_contents (Sexp.to_string expected) ~checkpoint)))
;;

let%test_unit
    "cooperative codec agrees with every pinned type constructor and binary atom"
  =
  let types =
    [ B.TCon ("native", [ TVar "a" ])
    ; TTuple [ TInt; TString ]
    ; TArray (TRef TBool)
    ; TRecord (TRow_extend ([ "a", TFloat ], TRow_var "row"))
    ; TModule (TRow_extend ([ "identity", TFun ([ TVar "a" ], TVar "a") ], TRow_empty))
    ; TField_receiver ("kind", TRow_empty)
    ; TVariant
        (TVariant_row_extend
           ([ "None", []; "Pair", [ TUnit; TString ] ], TVariant_row_var "r"))
    ; TVariant TVariant_row_empty
    ; TMu ("self", TRec_var "self")
    ]
  in
  let atoms =
    [ ""
    ; "plain"
    ; "a b"
    ; "#|comment"
    ; "|#"
    ; "quote\"slash\\"
    ; "\n\r\t\b"
    ; String.init 256 ~f:Char.of_int_exn
    ; String.make 20000 'n'
    ]
  in
  List.iter atoms ~f:(fun name ->
    let globals =
      List.mapi types ~f:(fun index scheme ->
        B.{ name = name ^ Int.to_string index; scheme; impl = (fun _ -> L.VUnit) })
    in
    let surface =
      Surface.
        { globals
        ; modules = [ B.{ name; exports = globals } ]
        ; type_aliases =
            List.mapi types ~f:(fun index body ->
              Surface.{ name = name ^ Int.to_string index; body })
        }
    in
    let requirements =
      List.mapi types ~f:(fun index scheme -> name ^ Int.to_string index, scheme)
    in
    let contract =
      C.for_static_surface
        (Static.of_runtime_surface surface ~checkpoint)
        ~required_bindings:requirements
    in
    let old = legacy_identity C.Target.One_off_v1 surface requirements in
    let expected =
      match old with
      | Sexp.List (_tag :: _target :: rest) ->
        Sexp.List (Atom "ochat.chatml.compiler.native.v1" :: Atom "Native" :: rest)
      | _ -> assert false
    in
    assert (Sexp.equal (C.identity contract ~checkpoint) expected);
    assert (
      Chatml_source.Digest.equal
        (C.fingerprint contract ~checkpoint)
        (Chatml_source.Digest.of_contents (Sexp.to_string expected) ~checkpoint)));
  let long_surface =
    Surface.
      { globals =
          [ B.
              { name = String.make 20000 '\000'
              ; scheme = TTuple types
              ; impl = (fun _ -> L.VUnit)
              }
          ]
      ; modules = []
      ; type_aliases = []
      }
  in
  let contract =
    C.for_static_surface
      (Static.of_runtime_surface long_surface ~checkpoint)
      ~required_bindings:[]
  in
  let observed = ref 0 in
  ignore
    (C.fingerprint contract ~checkpoint:(fun () -> incr observed)
     : Chatml_source.Digest.t);
  assert (!observed > 0);
  let exception Stop of int in
  for target = 1 to !observed do
    let count = ref 0 in
    let sentinel = Stop target in
    match
      C.fingerprint contract ~checkpoint:(fun () ->
        incr count;
        if !count = target then raise sentinel)
    with
    | _ -> failwith "Identity codec swallowed interruption"
    | exception actual -> assert (phys_equal actual sentinel)
  done
;;

let%test_unit
    "projection does not execute native implementations and fingerprint owns real \
     metadata"
  =
  let called = ref 0 in
  let surface =
    Surface.
      { globals =
          [ B.
              { name = "native"
              ; scheme = TInt
              ; impl =
                  (fun _ ->
                    incr called;
                    L.VInt 7)
              }
          ]
      ; modules = []
      ; type_aliases = []
      }
  in
  let static = Static.of_runtime_surface surface ~checkpoint in
  let c1 = C.for_static_surface static ~required_bindings:[] in
  let c2 = C.for_static_surface static ~required_bindings:[ "main", B.TInt ] in
  assert (
    not
      (Chatml_source.Digest.equal
         (C.fingerprint c1 ~checkpoint)
         (C.fingerprint c2 ~checkpoint)));
  ignore
    (Support.compiled c1 (Support.input c1 [ "entry.chatml", "let held = native", [] ])
     : Chatml_linked_program.t);
  [%test_eq: int] !called 0
;;
