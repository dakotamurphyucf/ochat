open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source

let create input ~original ~end_byte ~max_input_bytes ~checkpoint =
  let open Result.Let_syntax in
  let reject result = Result.map_error result ~f:(fun _ -> ()) in
  let%bind prefix =
    Captured.create
      (Captured.id original)
      ~contents:(String.prefix (Captured.contents original) end_byte)
      ~expected_digest:None
      ~max_bytes:(Some max_input_bytes)
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> ())
  in
  let same_source location =
    Captured.Id.equal (Captured.Location.source location) (Captured.id original)
  in
  let rebase location =
    checkpoint ();
    if not (same_source location)
    then Ok location
    else (
      let span = Captured.Location.span location in
      if span.right.offset > end_byte
      then Error ()
      else
        Captured.locate
          prefix
          ~start_byte:span.left.offset
          ~end_byte:span.right.offset
          ~checkpoint
        |> Result.map_error ~f:(fun _ -> ()))
  in
  let rebase_option = function
    | None ->
      checkpoint ();
      Ok None
    | Some location -> Result.map (rebase location) ~f:Option.some
  in
  let environment = Prepared.environment input in
  let%bind declared_at = rebase_option (Prepared.Environment.declared_at environment) in
  let%bind environment =
    Prepared.Environment.create
      ~name:(Prepared.Environment.name environment)
      ~required_contract:(Prepared.Environment.required_contract environment)
      ~declared_at
    |> reject
  in
  let%bind definitions =
    List.map (Prepared.definitions input) ~f:(fun definition ->
      checkpoint ();
      let%bind declared_at = rebase (Prepared.Definition.declared_at definition) in
      Prepared.Definition.create
        ~implementation:(Prepared.Definition.implementation definition)
        ~interface:(Prepared.Definition.interface definition)
        ~declared_at
      |> reject)
    |> Result.all
  in
  let%bind scopes =
    List.map (Prepared.scopes input) ~f:(fun scope ->
      checkpoint ();
      let%bind selected_at = rebase_option (Prepared.Scope.selected_at scope) in
      let%bind bindings =
        List.map (Prepared.Scope.bindings scope) ~f:(fun binding ->
          checkpoint ();
          let%map declared_at = rebase binding.Prepared.Scope.declared_at in
          { binding with declared_at })
        |> Result.all
      in
      Prepared.Scope.create
        (Prepared.Scope.source scope)
        ~bindings
        ~selected_at
        ~checkpoint
      |> reject)
    |> Result.all
  in
  let sources =
    List.map (Prepared.sources input) ~f:(fun source ->
      checkpoint ();
      if Captured.Id.equal (Captured.id source) (Captured.id original)
      then prefix
      else source)
  in
  Prepared.create
    ~max_input_bytes:(Some max_input_bytes)
    ~compiler_contract:(Prepared.compiler_contract input)
    ~environment
    ~provenance:(Prepared.provenance input)
    ~entry:(Prepared.entry input)
    ~sources
    ~definitions
    ~scopes
    ~checkpoint
    ()
  |> reject
;;
