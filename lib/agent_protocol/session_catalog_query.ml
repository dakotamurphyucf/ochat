open! Core

module Sort = struct
  type field =
    | Created_at
    | Updated_at
    | Display_name
  [@@deriving compare, equal, sexp]

  type direction =
    | Ascending
    | Descending
  [@@deriving compare, equal, sexp]

  type t =
    { field : field
    ; direction : direction
    }
  [@@deriving compare, equal, sexp]

  let default = { field = Created_at; direction = Ascending }

  let to_json t =
    `Object
      [ ( "field"
        , `String
            (match t.field with
             | Created_at -> "created_at"
             | Updated_at -> "updated_at"
             | Display_name -> "display_name") )
      ; ( "direction"
        , `String
            (match t.direction with
             | Ascending -> "ascending"
             | Descending -> "descending") )
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind field =
      Json_codec.required_as
        fields
        "field"
        (Json_codec.enum
           ~name:"sort field"
           [ "created_at", Created_at
           ; "updated_at", Updated_at
           ; "display_name", Display_name
           ])
    in
    let%map direction =
      Json_codec.required_as
        fields
        "direction"
        (Json_codec.enum
           ~name:"sort direction"
           [ "ascending", Ascending; "descending", Descending ])
    in
    { field; direction }
  ;;
end

module Archive_filter = struct
  type t =
    | Active
    | Archived
    | All
  [@@deriving compare, equal, sexp]

  let to_json t =
    `String
      (match t with
       | Active -> "active"
       | Archived -> "archived"
       | All -> "all")
  ;;

  let of_json =
    Json_codec.enum
      ~name:"archive filter"
      [ "active", Active; "archived", Archived; "all", All ]
  ;;
end
