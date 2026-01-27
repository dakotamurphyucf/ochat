(** Public renderer entry points.

    The concrete chat page implementation lives in {!Renderer_page_chat}
    and is invoked via {!Renderer_pages}. *)

let render_full ~size ~model = Renderer_pages.render ~size ~model
let lang_of_path = Renderer_lang.lang_of_path
