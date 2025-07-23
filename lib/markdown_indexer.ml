open! Core

(* Markdown_indexer — stub implementation.
   Real embedding and vector persistence will be added later. *)

let index_directory
    ?(vector_db_root=".md_index")
    ~index_name
    ~description
    ~root =
  let _ = vector_db_root, index_name, description, root in
  failwith "Markdown_indexer: not yet implemented"

