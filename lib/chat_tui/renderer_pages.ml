let page_of_model (model : Model.t) : Model.Page_id.t = Model.active_page model

let render ~size ~model =
  match page_of_model model with
  | Model.Page_id.Chat -> Renderer_page_chat.render ~size ~model
;;
