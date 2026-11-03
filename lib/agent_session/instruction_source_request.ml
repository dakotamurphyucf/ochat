open! Core
module Content = Instruction_content
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)
let max_members = 64

let validate_roots roots =
  if List.length roots > max_members
  then invalid "instruction root limit"
  else if Option.is_some (List.find_a_dup roots ~compare:Content.Root_id.compare)
  then invalid "duplicate instruction root"
  else Ok ()
;;

module Selection = struct
  type t =
    | Project of
        { root : Content.Root_id.t
        ; scope : Content.Scope.t
        }
    | Skills of Content.Root_id.t list

  let roots = function
    | Project { root; _ } -> [ root ]
    | Skills roots -> roots
  ;;

  let validate t =
    match t with
    | Project _ -> Ok ()
    | Skills [] -> invalid "skills discovery needs explicit roots"
    | Skills roots -> validate_roots roots
  ;;
end

module Member = struct
  type t =
    { root : Content.Root_id.t
    ; member : Source.Portable_path.t
    ; digest : Chatml_source.Digest.t
    }

  let create ~root ~member ~digest = { root; member; digest }

  let of_observed observed =
    let id = Content.Observed.id observed in
    { root = Content.Id.root id
    ; member = Content.Id.member id
    ; digest = Content.Observed.digest observed
    }
  ;;

  let same_member left right =
    Content.Root_id.equal left.root right.root
    && Source.Portable_path.equal left.member right.member
  ;;

  let equal left right =
    same_member left right && Chatml_source.Digest.equal left.digest right.digest
  ;;
end

type t =
  { selection : Selection.t
  ; members : Member.t list
  }

let selection t = t.selection
let members t = t.members

let create ~selection ~members =
  let open Result.Let_syntax in
  let%bind () = Selection.validate selection in
  if List.length members > max_members
  then invalid "instruction member limit"
  else if
    List.existsi members ~f:(fun index member ->
      List.exists (List.take members index) ~f:(Member.same_member member))
  then invalid "duplicate qualified instruction member"
  else if
    List.exists members ~f:(fun member ->
      not (List.mem (Selection.roots selection) member.root ~equal:Content.Root_id.equal))
  then invalid "instruction member outside selected roots"
  else (
    match selection, members with
    | Selection.Skills _, [] -> invalid "skills capture needs explicit members"
    | Project _, _ | Skills _, _ :: _ -> Ok { selection; members })
;;

let scope_equal left right =
  Option.equal
    Source.Portable_path.equal
    (Content.Scope.path left)
    (Content.Scope.path right)
;;

let matches t capture =
  let kind_matches =
    match t.selection with
    | Selection.Project { root; scope } ->
      Instruction_capture.Kind.equal (Instruction_capture.kind capture) Project
      && Option.equal Content.Root_id.equal (Some root) (Instruction_capture.root capture)
      && scope_equal scope (Instruction_capture.scope capture)
    | Skills _ -> Instruction_capture.Kind.equal (Instruction_capture.kind capture) Skills
  in
  kind_matches
  && List.equal
       Member.equal
       t.members
       (List.map (Instruction_capture.loaded capture) ~f:(fun loaded ->
          Member.of_observed (Content.Loaded.observed loaded)))
;;

module Discovery = struct
  type t =
    { roots : Content.Root_id.t list
    ; observations : Content.Observed.t list
    ; issues : (Content.Root_id.t * Source.Portable_path.t * string) list
    }

  let roots t = t.roots
  let observations t = t.observations
  let issues t = t.issues

  let create ~roots ~observations ~issues =
    let open Result.Let_syntax in
    let%bind () = validate_roots roots in
    if List.length observations + List.length issues > max_members
    then invalid "instruction discovery result limit"
    else (
      let admitted root = List.mem roots root ~equal:Content.Root_id.equal in
      let bytes = ref 0 in
      let valid_observation observed =
        let length = Content.Observed.byte_length observed in
        if length < 0 || length > 262144 - !bytes
        then false
        else (
          bytes := !bytes + length;
          admitted (Content.Id.root (Content.Observed.id observed)))
      in
      if
        (not (List.for_all observations ~f:valid_observation))
        || (not
              (List.for_all issues ~f:(fun (root, _, code) ->
                 admitted root && String.length code <= 128)))
        || Option.is_some
             (List.find_a_dup
                (List.map observations ~f:Content.Observed.id)
                ~compare:Content.Id.compare)
      then invalid "invalid bounded instruction discovery"
      else Ok { roots; observations; issues })
  ;;
end
