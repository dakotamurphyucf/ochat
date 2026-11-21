open! Core
module Image = Agent_protocol.Inline_image

let%test_unit
    "inline image admission checks the original bytes, declared type and decoded budget"
  =
  let png = "\x89PNG\r\n\x1a\n" in
  let png_uri = "data:image/png;base64,iVBORw0KGgo=" in
  let examples =
    [ png_uri, png, "image/png"
    ; "data:image/jpeg;base64,/9j/", "\xff\xd8\xff", "image/jpeg"
    ; "data:image/gif;base64,R0lGODlh", "GIF89a", "image/gif"
    ; "data:image/webp;base64,UklGRgAAAABXRUJQ", "RIFF\000\000\000\000WEBP", "image/webp"
    ]
  in
  List.iter examples ~f:(fun (uri, expected, media_type) ->
    let size = String.length expected in
    let image = Image.of_data_uri uri ~max_bytes:size |> Result.ok |> Option.value_exn in
    [%test_eq: string] expected (Image.contents image);
    [%test_eq: string] media_type (Image.media_type image);
    assert (
      Option.equal
        Image.equal_error
        (Result.error (Image.of_data_uri uri ~max_bytes:(size - 1)))
        (Some Size_limit)));
  List.iter
    [ "https://example.invalid/picture.png", Image.Unsupported
    ; "blob_private", Unsupported
    ; "data:image/gif;base64,iVBORw0KGgo=", Media_mismatch
    ; "data:image/png;base64,iVBORw0KGgp=", Invalid_encoding
    ; "data:image/png;base64,!!==", Invalid_encoding
    ; "data:image/png;base64,", Invalid_encoding
    ]
    ~f:(fun (uri, expected) ->
      assert (
        Option.equal
          Image.equal_error
          (Some expected)
          (Result.error (Image.of_data_uri uri ~max_bytes:128))))
;;
