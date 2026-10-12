open Core
module Captured = Chatml_source

(* Comparison also cooperates for opaque, arbitrarily long namespace/name bytes. *)
let compare_string a b ~checkpoint =
  let length = Int.min (String.length a) (String.length b) in
  let rec loop index =
    if index mod 4096 = 0 then checkpoint ();
    if index = length
    then Int.compare (String.length a) (String.length b)
    else (
      let comparison = Char.compare (String.get a index) (String.get b index) in
      if comparison = 0 then loop (index + 1) else comparison)
  in
  loop 0
;;

let compare_id a b ~checkpoint =
  let comparison =
    compare_string
      (Captured.Id.package_namespace a)
      (Captured.Id.package_namespace b)
      ~checkpoint
  in
  if comparison = 0
  then
    compare_string (Captured.Id.relative_path a) (Captured.Id.relative_path b) ~checkpoint
  else comparison
;;

module Module_id = struct
  module T = struct
    type t = Captured.Id.t [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let of_implementation t = t
  let implementation t = t
end

module Environment = struct
  type t =
    { name : string
    ; required_contract : Captured.Digest.t
    ; declared_at : Captured.Location.t option
    }

  let create ~name ~required_contract ~declared_at =
    if String.is_empty name
    then Or_error.error_string "environment name is empty"
    else Ok { name; required_contract; declared_at }
  ;;

  let implicit ~required_contract =
    { name = "implicit"; required_contract; declared_at = None }
  ;;

  let name t = t.name
  let required_contract t = t.required_contract
  let declared_at t = t.declared_at
end

module Provenance = struct
  type t =
    | Implicit
    | Project of
        { declaration : Captured.Id.t
        ; lock : Captured.Id.t option
        }
end

module Definition = struct
  type t =
    { implementation : Captured.Id.t
    ; interface : Captured.Id.t option
    ; declared_at : Captured.Location.t
    }

  let create ~implementation ~interface ~declared_at =
    if Option.exists interface ~f:(Captured.Id.equal implementation)
    then Or_error.error_string "module implementation and interface are the same source"
    else Ok { implementation; interface; declared_at }
  ;;

  let id t = Module_id.of_implementation t.implementation
  let implementation t = t.implementation
  let interface t = t.interface
  let declared_at t = t.declared_at
end

module Scope = struct
  type binding =
    { name : string
    ; module_id : Module_id.t
    ; declared_at : Captured.Location.t
    }

  type t =
    { source : Captured.Id.t
    ; bindings : binding list
    ; selected_at : Captured.Location.t option
    }

  let valid_name name ~checkpoint =
    let length = String.length name in
    if length = 0
    then false
    else (
      let rec loop index =
        if index mod 4096 = 0 then checkpoint ();
        if index = length
        then true
        else (
          match String.get name index with
          | 'A' .. 'Z' -> loop (index + 1)
          | ('a' .. 'z' | '0' .. '9' | '_') when index > 0 -> loop (index + 1)
          | _ -> false)
      in
      loop 0)
  ;;

  let create source ~bindings ~selected_at ~checkpoint =
    checkpoint ();
    if
      not
        (List.for_all bindings ~f:(fun binding ->
           checkpoint ();
           valid_name binding.name ~checkpoint))
    then Or_error.error_string "configured module name is not a ChatML UIDENT"
    else (
      let bindings =
        List.sort bindings ~compare:(fun a b -> compare_string a.name b.name ~checkpoint)
      in
      let rec duplicate = function
        | [] | [ _ ] -> false
        | a :: (b :: _ as rest) ->
          compare_string a.name b.name ~checkpoint = 0 || duplicate rest
      in
      if duplicate bindings
      then Or_error.error_string "duplicate configured module name in file scope"
      else Ok { source; bindings; selected_at })
  ;;

  let source t = t.source
  let bindings t = t.bindings
  let selected_at t = t.selected_at

  let find t ~name =
    List.find t.bindings ~f:(fun binding -> String.equal binding.name name)
  ;;
end

module Accounting = struct
  type t =
    { source_count : int
    ; source_bytes : int
    ; metadata_bytes : int
    }

  let source_count t = t.source_count
  let source_bytes t = t.source_bytes
  let metadata_bytes t = t.metadata_bytes
  let input_bytes t = t.source_bytes + t.metadata_bytes
end

module Error = struct
  type problem =
    | Duplicate_source of Captured.Id.t
    | Portable_source_collision of Captured.Id.t * Captured.Id.t
    | Missing_source of Captured.Id.t
    | Duplicate_module of Module_id.t
    | Missing_module of Module_id.t
    | Duplicate_scope of Captured.Id.t
    | Missing_scope of Captured.Id.t
    | Invalid_location of Captured.Location.t
    | Incompatible_contract of
        { required : Captured.Digest.t
        ; selected : Captured.Digest.t
        }
    | Input_limit of
        { limit_bytes : int
        ; observed_bytes : int
        }
    | Invalid_input_limit of int
    | Accounting_overflow
  [@@deriving sexp_of]

  type t =
    { problem : problem
    ; locations : Captured.Location.t list
    ; message : string
    }
  [@@deriving sexp_of]
end

let error ?(locations = []) problem message = Error { Error.problem; locations; message }

let add a b =
  if b > Int.max_value - a
  then error Accounting_overflow "input accounting overflow"
  else Ok (a + b)
;;

type t =
  { entry : Captured.Id.t
  ; compiler_contract : Captured.Digest.t
  ; environment : Environment.t
  ; provenance : Provenance.t
  ; sources : Captured.t list
  ; definitions : Definition.t list
  ; scopes : Scope.t list
  ; accounting : Accounting.t
  ; fingerprint : Captured.Digest.t
  }

let entry t = t.entry
let compiler_contract t = t.compiler_contract
let environment t = t.environment
let provenance t = t.provenance
let sources t = t.sources
let definitions t = t.definitions
let scopes t = t.scopes
let accounting t = t.accounting
let fingerprint t = t.fingerprint

let find_source t id =
  List.find t.sources ~f:(fun source -> Captured.Id.equal (Captured.id source) id)
;;

let find_definition t id =
  List.find t.definitions ~f:(fun definition ->
    Module_id.equal (Definition.id definition) id)
;;

let find_scope t id =
  List.find t.scopes ~f:(fun scope -> Captured.Id.equal (Scope.source scope) id)
;;

module Input = struct
  type t =
    { entry : Captured.Id.t
    ; compiler_contract : Captured.Digest.t
    ; environment : Environment.t
    ; provenance : Provenance.t
    ; sources : Captured.t list
    ; definitions : Definition.t list
    ; scopes : Scope.t list
    }
end

(* Canonical length-coded atoms streamed without a whole-payload allocation.
   The same encoder is used first for bounded accounting, then sorted hashing. *)
let encode (input : Input.t) ~atom ~checkpoint =
  let open Result.Let_syntax in
  let number n = atom (Int.to_string n) in
  let id id =
    let%bind () = atom (Captured.Id.package_namespace id) in
    atom (Captured.Id.relative_path id)
  in
  let location location =
    let%bind () = id (Captured.Location.source location) in
    let%bind () =
      atom (Captured.Digest.to_hex (Captured.Location.source_digest location))
    in
    let position (position : Source.position) =
      let%bind () = number position.offset in
      let%bind () = number position.line in
      number position.column
    in
    let span = Captured.Location.span location in
    let%bind () = position span.left in
    position span.right
  in
  let option f = function
    | None -> atom "none"
    | Some value ->
      let%bind () = atom "some" in
      f value
  in
  let items tag values f =
    let%bind () = atom tag in
    let%bind () =
      List.fold_result values ~init:() ~f:(fun () value ->
        checkpoint ();
        let%bind () = atom "item" in
        f value)
    in
    atom "end"
  in
  let%bind () = atom "ochat.chatml.prepared-input.v1" in
  let%bind () = atom (Captured.Digest.to_hex input.compiler_contract) in
  let%bind () = atom (Environment.name input.environment) in
  let%bind () =
    atom (Captured.Digest.to_hex (Environment.required_contract input.environment))
  in
  let%bind () = option location (Environment.declared_at input.environment) in
  let%bind () = id input.entry in
  let%bind () =
    match input.provenance with
    | Provenance.Implicit -> atom "implicit"
    | Project { declaration; lock } ->
      let%bind () = atom "project" in
      let%bind () = id declaration in
      option id lock
  in
  let%bind () =
    items "sources" input.sources (fun source ->
      let%bind () = id (Captured.id source) in
      let%bind () = atom (Captured.Digest.to_hex (Captured.digest source)) in
      number (Captured.byte_length source))
  in
  let%bind () =
    items "definitions" input.definitions (fun definition ->
      let%bind () = id (Definition.implementation definition) in
      let%bind () = option id (Definition.interface definition) in
      location (Definition.declared_at definition))
  in
  items "scopes" input.scopes (fun scope ->
    let%bind () = id (Scope.source scope) in
    let%bind () = option location (Scope.selected_at scope) in
    items "bindings" (Scope.bindings scope) (fun (binding : Scope.binding) ->
      let%bind () = atom binding.name in
      let%bind () = id (Module_id.implementation binding.module_id) in
      location binding.declared_at))
;;

let account_input (input : Input.t) ~max_input_bytes ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    match max_input_bytes with
    | Some maximum when maximum <= 0 ->
      error (Invalid_input_limit maximum) "invalid input byte limit"
    | None | Some _ -> Ok ()
  in
  let check_limit total =
    match max_input_bytes with
    | Some maximum when total > maximum ->
      error
        (Input_limit { limit_bytes = maximum; observed_bytes = total })
        "input byte limit exceeded"
    | None | Some _ -> Ok ()
  in
  let%bind source_count, source_bytes =
    List.fold_result input.sources ~init:(0, 0) ~f:(fun (count, bytes) source ->
      checkpoint ();
      let%bind bytes = add bytes (Captured.byte_length source) in
      let%bind () = check_limit bytes in
      let%map count = add count 1 in
      count, bytes)
  in
  let metadata_bytes = ref 0 in
  let atom text =
    checkpoint ();
    let%bind atom_bytes =
      add (String.length text) (1 + String.length (Int.to_string (String.length text)))
    in
    let%bind metadata = add !metadata_bytes atom_bytes in
    let%bind total = add source_bytes metadata in
    let%map () = check_limit total in
    metadata_bytes := metadata
  in
  let%map () = encode input ~atom ~checkpoint in
  { Accounting.source_count; source_bytes; metadata_bytes = !metadata_bytes }
;;

let canonicalize (input : Input.t) ~checkpoint =
  let compare_id = compare_id ~checkpoint in
  let sources =
    List.sort input.sources ~compare:(fun a b ->
      compare_id (Captured.id a) (Captured.id b))
  in
  let definitions =
    List.sort input.definitions ~compare:(fun a b ->
      compare_id (Definition.implementation a) (Definition.implementation b))
  in
  let scopes =
    List.sort input.scopes ~compare:(fun a b ->
      compare_id (Scope.source a) (Scope.source b))
  in
  let check_duplicates items ~key ~problem =
    let rec loop = function
      | [] | [ _ ] -> Ok ()
      | a :: (b :: _ as rest) ->
        checkpoint ();
        if compare_id (key a) (key b) = 0
        then error (problem (key a)) "duplicate input identity"
        else loop rest
    in
    loop items
  in
  let open Result.Let_syntax in
  let%bind () =
    check_duplicates sources ~key:Captured.id ~problem:(fun id -> Duplicate_source id)
  in
  let%bind () =
    check_duplicates definitions ~key:Definition.implementation ~problem:(fun id ->
      Duplicate_module (Module_id.of_implementation id))
  in
  let%map () =
    check_duplicates scopes ~key:Scope.source ~problem:(fun id -> Duplicate_scope id)
  in
  { input with sources; definitions; scopes }
;;

let validate_portable_paths (input : Input.t) ~checkpoint =
  let open Result.Let_syntax in
  let portable =
    List.map input.sources ~f:(fun source ->
      checkpoint ();
      let id = Captured.id source in
      id, String.lowercase (Captured.Id.relative_path id))
    |> List.sort ~compare:(fun (a, pa) (b, pb) ->
      let comparison =
        compare_string
          (Captured.Id.package_namespace a)
          (Captured.Id.package_namespace b)
          ~checkpoint
      in
      if comparison = 0 then compare_string pa pb ~checkpoint else comparison)
  in
  (* Map comparisons are limited to portable paths of at most 1024 bytes.
     Arbitrary namespace bytes are compared through the cooperative helper. *)
  let rec groups = function
    | [] -> Ok ()
    | (first, _) :: _ as pending ->
      let namespace = Captured.Id.package_namespace first in
      let rec take group = function
        | (id, path) :: rest
          when compare_string namespace (Captured.Id.package_namespace id) ~checkpoint = 0
          -> take ((id, path) :: group) rest
        | rest -> List.rev group, rest
      in
      let group, rest = take [] pending in
      let paths =
        List.map group ~f:(fun (id, _) ->
          checkpoint ();
          Source.Portable_path.create (Captured.Id.relative_path id) |> Or_error.ok_exn)
      in
      let%bind () =
        match Source.Portable_path.first_collision paths ~checkpoint with
        | None -> Ok ()
        | Some (first, second) ->
          let id path =
            Captured.Id.create
              ~package_namespace:namespace
              ~relative_path:(Source.Portable_path.to_string path)
            |> Or_error.ok_exn
          in
          let message =
            if
              String.equal
                (String.lowercase (Source.Portable_path.to_string first))
                (String.lowercase (Source.Portable_path.to_string second))
            then "portable source paths collide"
            else "source path conflicts with a directory"
          in
          error (Portable_source_collision (id first, id second)) message
      in
      groups rest
  in
  groups portable
;;

let validate_associations (input : Input.t) ~checkpoint =
  let open Result.Let_syntax in
  let compare_id = compare_id ~checkpoint in
  let source id =
    match
      List.find input.sources ~f:(fun source ->
        checkpoint ();
        compare_id id (Captured.id source) = 0)
    with
    | Some source -> Ok source
    | None -> error (Missing_source id) "referenced source is absent"
  in
  let location location =
    let%bind captured = source (Captured.Location.source location) in
    if
      not
        (Captured.Digest.equal
           (Captured.digest captured)
           (Captured.Location.source_digest location))
    then
      error
        ~locations:[ location ]
        (Invalid_location location)
        "location belongs to another captured revision"
    else (
      match Captured.of_span captured (Captured.Location.span location) ~checkpoint with
      | Ok _ -> Ok ()
      | Error _ ->
        error
          ~locations:[ location ]
          (Invalid_location location)
          "location coordinates disagree with captured source")
  in
  let option f = function
    | None -> Ok ()
    | Some value -> f value
  in
  let require_scope id =
    let%bind _ = source id in
    if
      List.exists input.scopes ~f:(fun scope ->
        checkpoint ();
        compare_id id (Scope.source scope) = 0)
    then Ok ()
    else error (Missing_scope id) "executable source has no selected file scope"
  in
  let%bind () = option location (Environment.declared_at input.environment) in
  let%bind () =
    if
      Captured.Digest.equal
        input.compiler_contract
        (Environment.required_contract input.environment)
    then Ok ()
    else
      error
        ~locations:(Option.to_list (Environment.declared_at input.environment))
        (Incompatible_contract
           { required = Environment.required_contract input.environment
           ; selected = input.compiler_contract
           })
        "selected compiler contract differs from environment requirement"
  in
  let%bind () =
    match input.provenance with
    | Implicit -> Ok ()
    | Project { declaration; lock } ->
      let%bind _ = source declaration in
      option (fun id -> Result.map (source id) ~f:(fun _ -> ())) lock
  in
  let%bind () = require_scope input.entry in
  let%bind () =
    List.fold_result input.definitions ~init:() ~f:(fun () definition ->
      checkpoint ();
      let%bind () = location (Definition.declared_at definition) in
      let%bind () = require_scope (Definition.implementation definition) in
      option require_scope (Definition.interface definition))
  in
  List.fold_result input.scopes ~init:() ~f:(fun () scope ->
    checkpoint ();
    let%bind _ = source (Scope.source scope) in
    let%bind () = option location (Scope.selected_at scope) in
    List.fold_result
      (Scope.bindings scope)
      ~init:()
      ~f:(fun () (binding : Scope.binding) ->
        checkpoint ();
        let%bind () = location binding.declared_at in
        if
          List.exists input.definitions ~f:(fun definition ->
            checkpoint ();
            compare_id
              (Module_id.implementation binding.module_id)
              (Definition.implementation definition)
            = 0)
        then Ok ()
        else
          error
            ~locations:[ binding.declared_at ]
            (Missing_module binding.module_id)
            "configured binding has no module definition"))
;;

let fingerprint_input input ~checkpoint =
  let context = ref (Digestif.SHA256.init ()) in
  let feed text =
    let rec chunks offset =
      checkpoint ();
      if offset < String.length text
      then (
        let len = Int.min 4096 (String.length text - offset) in
        context := Digestif.SHA256.feed_string !context ~off:offset ~len text;
        chunks (offset + len))
    in
    chunks 0
  in
  let atom text =
    feed (Int.to_string (String.length text));
    feed ":";
    feed text;
    Ok ()
  in
  let open Result.Let_syntax in
  let%map () = encode input ~atom ~checkpoint in
  (* This hex was generated internally, not decoded from untrusted metadata. *)
  Digestif.SHA256.(to_hex (get !context)) |> Captured.Digest.of_hex |> Or_error.ok_exn
;;

let create
      ~max_input_bytes
      ~compiler_contract
      ~environment
      ~provenance
      ~entry
      ~sources
      ~definitions
      ~scopes
      ~checkpoint
      ()
  =
  checkpoint ();
  let input =
    { Input.entry
    ; compiler_contract
    ; environment
    ; provenance
    ; sources
    ; definitions
    ; scopes
    }
  in
  let open Result.Let_syntax in
  let%bind accounting = account_input input ~max_input_bytes ~checkpoint in
  let%bind input = canonicalize input ~checkpoint in
  let%bind () = validate_portable_paths input ~checkpoint in
  let%bind () = validate_associations input ~checkpoint in
  let%map fingerprint = fingerprint_input input ~checkpoint in
  checkpoint ();
  { entry = input.entry
  ; compiler_contract = input.compiler_contract
  ; environment = input.environment
  ; provenance = input.provenance
  ; sources = input.sources
  ; definitions = input.definitions
  ; scopes = input.scopes
  ; accounting
  ; fingerprint
  }
;;

let single_file ~compiler_contract ~source ~checkpoint () =
  (* Constructors cannot reject these internal constants and unconstrained exact
     bytes; checkpoint exceptions retain their original cancellation semantics. *)
  let id =
    Captured.Id.create
      ~package_namespace:"ochat.single-file.v1"
      ~relative_path:"main.chatml"
    |> Or_error.ok_exn
  in
  let captured =
    Captured.create id ~contents:source ~expected_digest:None ~max_bytes:None ~checkpoint
    |> Or_error.ok_exn
  in
  let scope =
    Scope.create id ~bindings:[] ~selected_at:None ~checkpoint |> Or_error.ok_exn
  in
  create
    ~max_input_bytes:None
    ~compiler_contract
    ~environment:(Environment.implicit ~required_contract:compiler_contract)
    ~provenance:Implicit
    ~entry:id
    ~sources:[ captured ]
    ~definitions:[]
    ~scopes:[ scope ]
    ~checkpoint
    ()
;;
