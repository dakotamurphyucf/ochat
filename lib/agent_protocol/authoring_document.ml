open! Core
open Authoring_common
module C = Authoring_codec

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let source_limit = 8 * 1024 * 1024

module Context = Authoring_context_core.Context

let equal_source_id a b = Chatml_source.Id.equal (Source.id a) (Source.id b)

module Document = struct
  module Version = struct
    type t =
      { context : Context.Expected.t
      ; source : Source.t
      ; last_editor_revision : int option
      ; overlay_open : bool
      }
    [@@deriving sexp_of]

    let create ~context ~source ~last_editor_revision ~overlay_open =
      if
        Option.exists last_editor_revision ~f:(fun revision ->
          revision <= 0 || revision > C.maximum_wire_integer)
        || (overlay_open && Option.is_none (Source.editor_revision source))
        || (overlay_open
            && not
                 (Option.equal
                    Int.equal
                    last_editor_revision
                    (Source.editor_revision source)))
        || ((not overlay_open) && Option.is_some (Source.editor_revision source))
      then C.invalid "inconsistent authoring document version"
      else Ok { context; source; last_editor_revision; overlay_open }
    ;;

    let context t = t.context
    let source t = t.source
    let last_editor_revision t = t.last_editor_revision
    let overlay_open t = t.overlay_open

    let to_json t =
      obj
        ([ "context", Context.Expected.to_json t.context
         ; "source", Source.to_json t.source
         ; "overlay_open", C.boolean t.overlay_open
         ]
         @ C.optional_field "last_editor_revision" t.last_editor_revision C.integer)
    ;;

    let of_json json =
      let* fields =
        C.fields
          ~allowed:[ "context"; "source"; "last_editor_revision"; "overlay_open" ]
          json
      in
      let* context = C.required_as fields "context" Context.Expected.of_json in
      let* source = C.required_as fields "source" Source.of_json in
      let* last_editor_revision =
        C.optional_as fields "last_editor_revision" C.positive
      in
      let* overlay_open = C.required_as fields "overlay_open" Json_codec.bool in
      create ~context ~source ~last_editor_revision ~overlay_open
    ;;
  end

  module Version_request = struct
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

  module Replace_request = struct
    type t =
      { expected : Version.t
      ; next_editor_revision : int
      ; contents : string
      }
    [@@deriving sexp_of]

    let create ~expected ~next_editor_revision ~contents =
      let* _ = C.utf8_text ~max_bytes:source_limit (str contents) in
      if
        next_editor_revision <= 0
        || next_editor_revision > C.maximum_wire_integer
        || Option.exists (Version.last_editor_revision expected) ~f:(fun previous ->
          next_editor_revision <= previous)
      then C.invalid "authoring editor revision must increase"
      else Ok { expected; next_editor_revision; contents }
    ;;

    let to_json t =
      obj
        [ "expected", Version.to_json t.expected
        ; "next_editor_revision", C.integer t.next_editor_revision
        ; "contents", str t.contents
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "expected"; "next_editor_revision"; "contents" ] json
      in
      let* expected = C.required_as fields "expected" Version.of_json in
      let* next_editor_revision =
        C.required_as fields "next_editor_revision" C.positive
      in
      let* contents =
        C.required_as fields "contents" (C.utf8_text ~max_bytes:source_limit)
      in
      create ~expected ~next_editor_revision ~contents
    ;;
  end

  module Close_request = struct
    type t = { expected : Version.t } [@@deriving sexp_of]

    let create ~expected = { expected }
    let to_json t = obj [ "expected", Version.to_json t.expected ]

    let of_json json =
      let* fields = C.fields ~allowed:[ "expected" ] json in
      let* expected = C.required_as fields "expected" Version.of_json in
      Ok { expected }
    ;;
  end

  module Result = struct
    type t =
      { context : Context.t
      ; version : Version.t
      }
    [@@deriving sexp_of]

    let create ~context ~version =
      if
        not
          (Context.Expected.equal
             (Version.context version)
             (Context.Expected.of_view context))
      then C.invalid "document result context/version disagree"
      else (
        let source = Version.source version in
        let matches_basis =
          match Context.basis context with
          | Context.Project _ -> true
          | Context.Document { source = current; _ } -> Source.equal source current
        in
        let matches_overlay =
          if Version.overlay_open version
          then
            List.exists (Context.editable_sources context) ~f:(fun editable ->
              Source.equal editable source)
          else
            not
              (List.exists (Context.editable_sources context) ~f:(fun editable ->
                 equal_source_id editable source))
        in
        if (not matches_basis) || not matches_overlay
        then C.invalid "document result source/basis disagree"
        else Ok { context; version })
    ;;

    let to_json t =
      obj [ "context", Context.to_json t.context; "version", Version.to_json t.version ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "context"; "version" ] json in
      let* context = C.required_as fields "context" Context.of_json in
      let* version = C.required_as fields "version" Version.of_json in
      create ~context ~version
    ;;
  end
end
