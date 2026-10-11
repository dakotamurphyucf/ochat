open Core
open Git_catalog_model
module Host = Chatml_git_host
module Id = Host.Object_id

module Key = struct
  type t = Id.t [@@deriving sexp_of]

  let compare = Id.compare
end

module Key_comparator = struct
  include Key
  include Comparator.Make (Key)
end

type t =
  { repository : Host.Repository.t
  ; budget : Git_catalog_budget.t
  ; mutable objects : (Id.t, Git_object.t, Key_comparator.comparator_witness) Map.t
  ; mutable trees : (Id.t, Git_tree.t, Key_comparator.comparator_witness) Map.t
  }

let create repository ~budget =
  { repository
  ; budget
  ; objects = Map.empty (module Key_comparator)
  ; trees = Map.empty (module Key_comparator)
  }
;;

let checkpoint t = Git_catalog_budget.checkpoint t.budget
let error t = Git_catalog_budget.error t.budget

let object_error t id allowance = function
  | Git_object.Error.Missing -> Error.Missing_object id
  | Malformed_protocol -> Malformed_object_protocol
  | Hash_mismatch -> Object_hash_mismatch id
  | Wrong_kind -> Wrong_object_kind id
  | Body_limit ->
    Limit_exceeded
      (if allowance < (Git_catalog_budget.limits t.budget).max_object_bytes
       then Total_object_bytes
       else Object_bytes)
;;

let object_ t id ~kind =
  checkpoint t;
  match Map.find t.objects id with
  | Some object_ ->
    if Git_object.Kind.equal kind (Git_object.kind object_)
    then Ok object_
    else Error (error t (Wrong_object_kind id))
  | None ->
    let open Result.Let_syntax in
    let%bind () = Git_catalog_budget.admit_object t.budget in
    let allowance = Git_catalog_budget.object_allowance t.budget in
    if allowance > Int.max_value - 128
    then Error (error t Accounting_overflow)
    else (
      let%bind wire =
        Result.map_error
          (Host.Repository.raw_object
             t.repository
             id
             ~max_output_bytes:(allowance + 128)
             ~checkpoint:(fun () -> checkpoint t))
          ~f:(fun failure -> error t (Host failure))
      in
      let%bind object_ =
        Result.map_error
          (Git_object.verify
             id
             ~wire
             ~expected:kind
             ~max_bytes:allowance
             ~admit_bytes:(Git_catalog_budget.admit_body t.budget)
             ~checkpoint:(fun () -> checkpoint t))
          ~f:(fun failure -> error t (object_error t id allowance failure))
      in
      checkpoint t;
      t.objects <- Map.set t.objects ~key:id ~data:object_;
      Ok object_)
;;

let root t =
  let open Result.Let_syntax in
  let%bind commit =
    object_ t (Id.of_pin (Host.Repository.pin t.repository)) ~kind:Commit
  in
  Result.map_error
    (Git_commit.root_tree commit ~checkpoint:(fun () -> checkpoint t))
    ~f:(fun _ -> error t Invalid_commit)
;;

let tree t id =
  checkpoint t;
  match Map.find t.trees id with
  | Some tree -> Ok tree
  | None ->
    let open Result.Let_syntax in
    let%bind object_ = object_ t id ~kind:Tree in
    let%bind tree =
      Result.map_error
        (Git_tree.decode
           object_
           ~max_entries:(Git_catalog_budget.tree_allowance t.budget)
           ~checkpoint:(fun () -> checkpoint t))
        ~f:(fun failure ->
          error
            t
            (match failure with
             | Git_tree.Error.Wrong_kind | Malformed -> Invalid_tree
             | Entry_limit -> Limit_exceeded Tree_entries
             | Name_limit -> Limit_exceeded Tree_name_bytes))
    in
    let count =
      List.fold (Git_tree.entries tree) ~init:0 ~f:(fun count _ ->
        checkpoint t;
        count + 1)
    in
    let%bind () = Git_catalog_budget.admit_tree t.budget count in
    checkpoint t;
    t.trees <- Map.set t.trees ~key:id ~data:tree;
    Ok tree
;;

let subtree t ~root ~components ~path =
  let rec walk root depth = function
    | [] -> Ok root
    | component :: rest ->
      checkpoint t;
      if depth >= (Git_catalog_budget.limits t.budget).max_path_components
      then Error (error t (Limit_exceeded Path_components))
      else
        let open Result.Let_syntax in
        let%bind current = tree t root in
        (match
           Git_tree.find current ~name:component ~checkpoint:(fun () -> checkpoint t)
         with
         | None -> Error (error t (Missing_member path))
         | Some entry ->
           (match Git_tree.Entry.mode entry with
            | Tree -> walk (Git_tree.Entry.id entry) (depth + 1) rest
            | Regular | Executable | Symlink | Gitlink | Unsupported ->
              Error (error t (Unsupported_member_mode path))))
  in
  checkpoint t;
  if List.length components > (Git_catalog_budget.limits t.budget).max_path_components
  then Error (error t (Limit_exceeded Path_components))
  else walk root 0 components
;;

let directory t ~root ~components ~path =
  let open Result.Let_syntax in
  let%bind id = subtree t ~root ~components ~path in
  tree t id
;;

let blob t ~root ~path =
  let components = String.split (Path.to_string path) ~on:'/' in
  checkpoint t;
  if List.length components > (Git_catalog_budget.limits t.budget).max_path_components
  then Error (error t (Limit_exceeded Path_components))
  else (
    match List.rev components with
    | [] -> Error (error t (Missing_member path))
    | name :: parents ->
      let open Result.Let_syntax in
      let%bind parent = directory t ~root ~components:(List.rev parents) ~path in
      checkpoint t;
      (match Git_tree.find parent ~name ~checkpoint:(fun () -> checkpoint t) with
       | None -> Ok None
       | Some entry ->
         (match Git_tree.Entry.mode entry with
          | Regular | Executable ->
            let%map object_ = object_ t (Git_tree.Entry.id entry) ~kind:Blob in
            Some (Git_object.body object_)
          | Tree | Symlink | Gitlink | Unsupported ->
            Error (error t (Unsupported_member_mode path)))))
;;
