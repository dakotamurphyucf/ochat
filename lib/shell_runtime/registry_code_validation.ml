open Core
module B = Registry_code_budget
module F = Registry_resource_footprint
module V = Registry_footprint_comparison
module M = Chatmd_shell_spec.Manifest
module S = Chatmd_shell_spec.Shell_spec
module T = Chatmd_shell_spec.Shell_tool_spec

type t =
  { runtimes : (Runtime.t * S.t) list
  ; tools : T.t String.Map.t
  }

let runtimes t = t.runtimes
let tools t = t.tools

let changed ?runtime_id field =
  F.Error.{ code = "shell.native_footprint_changed"; runtime_id; field }
;;

let create
      ~original_manifest
      ~original_runtimes
      ~original_tools
      ~original_policy
      ~candidate_manifest
      ~candidate_runtimes
      ~candidate_policy
      ~budget
  =
  let module Key = struct
    module T = struct
      type t = string [@@deriving sexp_of]

      let compare = B.compare_string budget
    end

    include T
    include Comparator.Make (T)
  end
  in
  let index ~key ~field values =
    List.fold_result
      values
      ~init:(Map.empty (module Key))
      ~f:(fun map value ->
        B.work budget;
        match Map.add map ~key:(key value) ~data:value with
        | `Ok map -> Ok map
        | `Duplicate -> Error (changed field))
  in
  let compare ~field equal original candidate =
    B.work budget;
    if equal budget original candidate then Ok () else Error (changed field)
  in
  let dependency budget (left : M.dependency) (right : M.dependency) =
    B.work budget;
    V.string budget left.from_id right.from_id
    && V.string budget left.to_id right.to_id
    && V.scalar M.equal_edge_kind budget left.kind right.kind
  in
  let open Result.Let_syntax in
  let%bind () =
    F.check_admin_policy original_policy ~candidate:candidate_policy ~budget
  in
  let original = original_manifest.M.payload in
  let candidate = candidate_manifest.M.payload in
  let%bind () =
    compare
      ~field:"encoding_version"
      V.string
      original.encoding_version
      candidate.encoding_version
  in
  let%bind () =
    compare
      ~field:"platform"
      (V.scalar S.equal_platform)
      original.platform
      candidate.platform
  in
  let%bind () =
    compare
      ~field:"moderator_runtime"
      (V.option V.string)
      original.moderator_runtime
      candidate.moderator_runtime
  in
  let%bind () =
    compare
      ~field:"dependencies"
      (V.list dependency)
      original.dependencies
      candidate.dependencies
  in
  let%bind () =
    compare
      ~field:"required_features"
      (V.list V.string)
      original.required_features
      candidate.required_features
  in
  let%bind candidates =
    index
      ~key:(fun specification -> S.Runtime_id.to_string specification.S.id)
      ~field:"duplicate_runtime"
      candidate_runtimes
  in
  let%bind reversed, remaining =
    List.fold_result
      original_runtimes
      ~init:([], candidates)
      ~f:(fun (reversed, remaining) runtime ->
        B.work budget;
        let id = Runtime.id runtime in
        match Map.find remaining id with
        | None -> Error (changed ~runtime_id:id "missing_runtime")
        | Some specification ->
          let%map () =
            F.check_runtime (Runtime.spec runtime) ~candidate:specification ~budget
          in
          let remaining = Map.remove remaining id in
          B.work budget;
          (runtime, specification) :: reversed, remaining)
  in
  let%bind () =
    if Map.is_empty remaining then Ok () else Error (changed "surplus_runtime")
  in
  let runtimes =
    List.fold reversed ~init:[] ~f:(fun ordered value ->
      B.work budget;
      value :: ordered)
  in
  let%bind tools =
    index ~key:(fun (tool : T.t) -> tool.name) ~field:"duplicate_tool" candidate.tools
  in
  let%bind () =
    Map.fold original_tools ~init:(Ok tools) ~f:(fun ~key ~data remaining ->
      let%bind remaining = remaining in
      B.work budget;
      match Map.find remaining key with
      | None -> Error (changed "missing_tool")
      | Some candidate ->
        let%map () = F.check_tool data ~candidate ~budget in
        Map.remove remaining key)
    |> Result.bind ~f:(fun remaining ->
      if Map.is_empty remaining then Ok () else Error (changed "surplus_tool"))
  in
  let tools =
    Map.mapi original_tools ~f:(fun ~key ~data:_ ->
      B.work budget;
      (* Every original key was proven present above. The resulting map retains
         its original comparator and keys, never the budget-scoped comparator. *)
      Map.find_exn tools key)
  in
  let%bind extensions =
    index
      ~key:(fun (extension : M.extension_script) -> extension.id)
      ~field:"duplicate_extension"
      candidate.extension_scripts
  in
  let%bind remaining =
    List.fold_result
      original.extension_scripts
      ~init:extensions
      ~f:(fun remaining original ->
        B.work budget;
        match Map.find remaining original.id with
        | None -> Error (changed "missing_extension")
        | Some candidate ->
          let%map () = F.check_extension original ~candidate ~budget in
          Map.remove remaining original.id)
  in
  let%map () =
    if Map.is_empty remaining then Ok () else Error (changed "surplus_extension")
  in
  { runtimes; tools }
;;
