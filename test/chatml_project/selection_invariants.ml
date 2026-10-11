open Core
module P = Chatml_project
module M = Selection_model
module S = Support

let checkpoint () = ()
let path source = Source.Portable_path.create source |> Or_error.ok_exn

let request = function
  | M.Request.Entry environment -> P.Selection.Entry { name = "main"; environment }
  | M.Request.Library environment -> P.Selection.Library { environment }
;;

let requests =
  [ M.Request.Entry None
  ; Entry (Some "default")
  ; Entry (Some "entry")
  ; Entry (Some "app")
  ; Entry (Some "missing")
  ; Library None
  ; Library (Some "app")
  ; Library (Some "missing")
  ]
;;

let assert_selection declaration model selected_request =
  let actual = P.select declaration ~request:(request selected_request) ~checkpoint in
  match M.select model ~request:selected_request, actual with
  | Ok expected, Ok selected ->
    [%test_eq: string] (P.Scope_spec.name (P.Selection.environment selected)) expected;
    [%test_eq: bool]
      (Option.is_some (P.Selection.entry selected))
      (match selected_request with
       | Entry _ -> true
       | Library _ -> false)
  | Error (Missing_selector | Unknown_environment _), Error error ->
    S.assert_source (P.source declaration) ~actual:(P.Error.source error);
    [%test_eq: string list] (P.Error.field_path error) [ "environments" ];
    (match P.Error.details error with
     | Document_schema.Error.Invalid_field { path; reason = _ } ->
       [%test_eq: string list] path [ "environments" ]
     | details ->
       raise_s
         [%sexp "Expected a selector field error", (details : Document_schema.Error.t)])
  | expected, actual ->
    raise_s
      [%sexp
        "Independent selection model disagreed"
      , (selected_request : M.Request.t)
      , (expected : (string, M.Error.t) Result.t)
      , (Result.is_ok actual : bool)]
;;

let%test_unit "finite selector model covers authored precedence and missing names" =
  List.iter Fixtures.selection ~f:(fun fixture ->
    let declaration = S.capture fixture.json |> S.decode_exn in
    List.iter requests ~f:(assert_selection declaration fixture.model))
;;

module Profile = struct
  type t =
    { name : string
    ; bindings : (string * string * string) list option
    ; compiler : string option
    ; targets : string list option
    }
  [@@deriving equal, sexp_of]

  let of_scope scope =
    let requirement = P.Scope_spec.compiler_requirement scope in
    { name = P.Scope_spec.name scope
    ; bindings =
        Option.map (P.Scope_spec.bindings scope) ~f:(fun bindings ->
          List.map bindings ~f:(fun binding ->
            ( binding.name
            , (match P.Module_ref.catalog binding.module_ref with
               | Self -> "self"
               | Dependency name -> name)
            , P.Module_ref.name binding.module_ref ))
          |> List.sort ~compare:(fun (left, _, _) (right, _, _) ->
            String.compare left right))
    ; compiler = P.Compiler_requirement.compiler_family requirement
    ; targets = P.Compiler_requirement.allowed_targets requirement
    }
  ;;
end

let%expect_test "file overrides replace bindings and compiler constraints independently" =
  let declaration = S.capture Fixtures.replacement |> S.decode_exn in
  let selected = S.select_exn declaration ~request:(Library { environment = None }) in
  let model : M.t =
    { environments = [ "app"; "isolated"; "none"; "catalog" ]
    ; package_default = Some "app"
    ; entry_environment = None
    ; files =
        [ "src/a.chatml", "isolated"
        ; "src/a.chatmli", "none"
        ; "src/all.chatml", "catalog"
        ]
    }
  in
  let profiles : Profile.t list =
    [ { name = "app"
      ; bindings = Some [ "Root", "self", "Root" ]
      ; compiler = Some "root-family"
      ; targets = Some [ "root-target" ]
      }
    ; { name = "isolated"
      ; bindings = Some [ "Only", "self", "Other" ]
      ; compiler = Some "other-family"
      ; targets = Some [ "other-target" ]
      }
    ; { name = "none"; bindings = Some []; compiler = None; targets = None }
    ; { name = "catalog"; bindings = None; compiler = None; targets = None }
    ]
  in
  List.iter
    [ "src/a.chatml"; "src/a.chatmli"; "src/all.chatml"; "other.chatml" ]
    ~f:(fun source ->
      let actual =
        P.Selection.scope_for selected ~source:(path source) |> Profile.of_scope
      in
      let expected_name = M.scope_for model ~selected:"app" ~source in
      let expected =
        List.find_exn profiles ~f:(fun profile -> String.equal profile.name expected_name)
      in
      if not (Profile.equal actual expected)
      then raise_s [%sexp (source : string), (expected : Profile.t), (actual : Profile.t)];
      print_s
        [%sexp
          (source : string), (actual.name : string), (actual.compiler : string option)]);
  [%expect
    {|
    (src/a.chatml isolated (other-family))
    (src/a.chatmli none ())
    (src/all.chatml catalog ())
    (other.chatml app (root-family)) |}]
;;

let%expect_test
    "an omitted dependency selector uses the separately decoded package default"
  =
  let root_source =
    S.capture
      ~namespace:"root"
      {|{"schema_version":1,"namespace":"root","environments":{"application":{}},"default_environment":"application","dependencies":{"Shared":{"source":{"kind":"local","path":"../shared"},"project":"chatml.project.json"}}}|}
  in
  let dependency_source =
    S.capture
      ~namespace:"shared"
      {|{"schema_version":1,"namespace":"shared","environments":{"library":{}},"default_environment":"library"}|}
  in
  let root = S.decode_exn root_source in
  let dependency = S.decode_exn dependency_source in
  let requirement = List.hd_exn (P.dependencies root) in
  [%test_eq: string option] (P.Dependency.environment requirement) None;
  let root_selection = S.select_exn root ~request:(Library { environment = None }) in
  let selected =
    S.select_exn
      dependency
      ~request:(Library { environment = P.Dependency.environment requirement })
  in
  [%test_eq: string]
    (P.Scope_spec.name (P.Selection.environment root_selection))
    "application";
  [%test_eq: string] (P.Scope_spec.name (P.Selection.environment selected)) "library";
  assert (
    Result.is_error
      (P.select
         dependency
         ~request:(Library { environment = Some "application" })
         ~checkpoint));
  S.assert_source dependency_source ~actual:(P.source dependency);
  print_s
    [%sexp
      "root"
    , (P.Scope_spec.name (P.Selection.environment root_selection) : string)
    , "dependency"
    , (P.Scope_spec.name (P.Selection.environment selected) : string)];
  [%expect {| (root application dependency library) |}]
;;
