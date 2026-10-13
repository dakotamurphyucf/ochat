open! Core
open Authoring_common
module C = Authoring_codec

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let source_limit = 8 * 1024 * 1024

module Context = Authoring_context_core.Context

let bounded_name value =
  if
    String.is_empty value
    || String.length value > 256
    || String.exists value ~f:(fun char ->
      let code = Char.to_int char in
      code < 0x20 || code = 0x7f)
  then C.invalid "invalid authoring selection name"
  else C.utf8_text ~max_bytes:256 (`String value)
;;

let name_json json =
  let* value = C.utf8_text ~max_bytes:256 json in
  bounded_name value
;;

module Source_read = struct
  let verified_bytes ~source ~contents =
    if
      String.length contents <> Source.byte_length source
      || String.length contents > source_limit
      || not
           (Chatml_source.Digest.equal
              (Chatml_source.Digest.of_contents contents ~checkpoint:(fun () -> ()))
              (Source.digest source))
    then C.invalid "authoring read bytes do not match source revision"
    else Ok ()
  ;;

  module Root = struct
    type t =
      { id : Id.Authoring_root.t
      ; display_name : string
      }
    [@@deriving sexp_of]

    let create ~id ~display_name =
      let* display_name = bounded_name display_name in
      Ok { id; display_name }
    ;;

    let to_json t =
      obj [ "id", Id.Authoring_root.to_json t.id; "display_name", str t.display_name ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "id"; "display_name" ] json in
      let* id = C.required_as fields "id" Id.Authoring_root.of_json in
      let* display_name = C.required_as fields "display_name" name_json in
      create ~id ~display_name
    ;;
  end

  module Roots_result = struct
    type t = { roots : Root.t list } [@@deriving sexp_of]

    let create ~roots =
      let ids = List.map roots ~f:(fun root -> root.Root.id) in
      if
        List.length roots > 64
        || List.length ids
           <> List.length (List.dedup_and_sort ids ~compare:Id.Authoring_root.compare)
      then C.invalid "invalid authoring roots result"
      else Ok { roots }
    ;;

    let to_json t = obj [ "roots", C.array Root.to_json t.roots ]

    let of_json json =
      let* fields = C.fields ~allowed:[ "roots" ] json in
      let* roots = C.required_as fields "roots" (C.list ~max_items:64 Root.of_json) in
      create ~roots
    ;;
  end

  module Host_list_request = struct
    type t =
      { root : Id.Authoring_root.t
      ; directory : string option
      }
    [@@deriving sexp_of]

    let create ~root ~directory =
      let* directory =
        match directory with
        | None -> Ok None
        | Some member -> Result.map (C.portable_member_string member) ~f:Option.some
      in
      Ok { root; directory }
    ;;

    let to_json t =
      obj
        ([ "root", Id.Authoring_root.to_json t.root ]
         @ C.optional_field "directory" t.directory str)
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "root"; "directory" ] json in
      let* root = C.required_as fields "root" Id.Authoring_root.of_json in
      let* directory = C.optional_as fields "directory" C.portable_member in
      create ~root ~directory
    ;;
  end

  module Host_list_result = struct
    type t =
      { root : Id.Authoring_root.t
      ; members : string list
      }
    [@@deriving sexp_of]

    let create ~root ~members =
      let* members = Result.all (List.map members ~f:C.portable_member_string) in
      if
        List.length members > 4096
        || not (List.is_sorted_strictly members ~compare:String.compare)
      then C.invalid "invalid bounded host member list"
      else Ok { root; members }
    ;;

    let to_json t =
      obj [ "root", Id.Authoring_root.to_json t.root; "members", C.array str t.members ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "root"; "members" ] json in
      let* root = C.required_as fields "root" Id.Authoring_root.of_json in
      let* members =
        C.required_as fields "members" (C.list ~max_items:4096 C.portable_member)
      in
      create ~root ~members
    ;;
  end

  module Host_read_request = struct
    type t =
      { root : Id.Authoring_root.t
      ; member : string
      }
    [@@deriving sexp_of]

    let create ~root ~member =
      let* member = C.portable_member_string member in
      Ok { root; member }
    ;;

    let to_json t =
      obj [ "root", Id.Authoring_root.to_json t.root; "member", str t.member ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "root"; "member" ] json in
      let* root = C.required_as fields "root" Id.Authoring_root.of_json in
      let* member = C.required_as fields "member" C.portable_member in
      create ~root ~member
    ;;
  end

  module Host_read_result = struct
    type t =
      { root : Id.Authoring_root.t
      ; member : string
      ; source : Source.t
      ; contents : string
      }
    [@@deriving sexp_of]

    let create ~root ~member ~source ~contents =
      let* member = C.portable_member_string member in
      let* () = verified_bytes ~source ~contents in
      if
        (not (String.equal (Chatml_source.Id.relative_path (Source.id source)) member))
        || Option.is_some (Source.editor_revision source)
      then C.invalid "host read source owner differs from selected member"
      else Ok { root; member; source; contents }
    ;;

    let root t = t.root
    let member t = t.member
    let source t = t.source
    let contents t = t.contents

    let to_json t =
      obj
        [ "root", Id.Authoring_root.to_json t.root
        ; "member", str t.member
        ; "source", Source.to_json t.source
        ; "contents_base64", C.bytes_base64_json t.contents
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "root"; "member"; "source"; "contents_base64" ] json
      in
      let* root = C.required_as fields "root" Id.Authoring_root.of_json in
      let* member = C.required_as fields "member" C.portable_member in
      let* source = C.required_as fields "source" Source.of_json in
      let* contents =
        C.required_as fields "contents_base64" (C.bytes_base64 ~max_bytes:source_limit)
      in
      create ~root ~member ~source ~contents
    ;;
  end

  module List_request = struct
    type t =
      { context : Context.Expected.t
      ; package_namespace : string option
      ; directory : string option
      }
    [@@deriving sexp_of]

    let create ~context ~package_namespace ~directory =
      let* package_namespace =
        match package_namespace with
        | None -> Ok None
        | Some namespace when String.is_empty namespace || String.length namespace > 1024
          -> C.invalid "invalid authoring package namespace filter"
        | Some namespace -> Ok (Some namespace)
      in
      let* directory =
        match directory with
        | None -> Ok None
        | Some member -> Result.map (C.portable_member_string member) ~f:Option.some
      in
      Ok { context; package_namespace; directory }
    ;;

    let to_json t =
      obj
        ([ "context", Context.Expected.to_json t.context ]
         @ C.optional_field
             "package_namespace_base64"
             t.package_namespace
             C.bytes_base64_json
         @ C.optional_field "directory" t.directory str)
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "context"; "package_namespace_base64"; "directory" ] json
      in
      let* context = C.required_as fields "context" Context.Expected.of_json in
      let* package_namespace =
        C.optional_as fields "package_namespace_base64" (C.bytes_base64 ~max_bytes:1024)
      in
      let* directory = C.optional_as fields "directory" C.portable_member in
      create ~context ~package_namespace ~directory
    ;;
  end

  module Read_request = struct
    type t =
      { context : Context.Expected.t
      ; source_id : Chatml_source.Id.t
      }
    [@@deriving sexp_of]

    let create ~context ~source_id =
      Result.map (C.validate_source_id source_id) ~f:(fun () -> { context; source_id })
    ;;

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "source_id", C.source_id_json t.source_id
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "context"; "source_id" ] json in
      let* context = C.required_as fields "context" Context.Expected.of_json in
      let* source_id = C.required_as fields "source_id" C.source_id in
      create ~context ~source_id
    ;;
  end

  module List_result = struct
    type t =
      { context : Context.Expected.t
      ; sources : Source.t list
      ; omitted : int
      }
    [@@deriving sexp_of]

    let create ~context ~sources ~omitted =
      if
        omitted < 0
        || omitted > C.maximum_wire_integer
        || List.length sources > 4096
        || not
             (List.is_sorted_strictly sources ~compare:(fun a b ->
                Chatml_source.Id.compare (Source.id a) (Source.id b)))
      then C.invalid "invalid captured source inventory"
      else Ok { context; sources; omitted }
    ;;

    let context t = t.context
    let sources t = t.sources
    let omitted t = t.omitted
    let truncated t = t.omitted > 0

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "sources", C.array Source.to_json t.sources
        ; "omitted", C.integer t.omitted
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "context"; "sources"; "omitted" ] json in
      let* context = C.required_as fields "context" Context.Expected.of_json in
      let* sources =
        C.required_as fields "sources" (C.list ~max_items:4096 Source.of_json)
      in
      let* omitted = C.required_as fields "omitted" C.nonnegative in
      create ~context ~sources ~omitted
    ;;
  end

  module Read_result = struct
    type t =
      { context : Context.Expected.t
      ; source : Source.t
      ; contents : string
      }
    [@@deriving sexp_of]

    let create ~context ~source ~contents =
      let* () = verified_bytes ~source ~contents in
      Ok { context; source; contents }
    ;;

    let context t = t.context
    let source t = t.source
    let contents t = t.contents

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "source", Source.to_json t.source
        ; "contents_base64", C.bytes_base64_json t.contents
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "context"; "source"; "contents_base64" ] json in
      let* context = C.required_as fields "context" Context.Expected.of_json in
      let* source = C.required_as fields "source" Source.of_json in
      let* contents =
        C.required_as fields "contents_base64" (C.bytes_base64 ~max_bytes:source_limit)
      in
      create ~context ~source ~contents
    ;;
  end
end
