let page_of_model (model : Model.t) : Model.Page_id.t = Model.active_page model

let render ~size ~model =
  match page_of_model model with
  | Model.Page_id.Chat -> Renderer_page_chat.render ~size ~model
  | Model.Page_id.Agent -> Renderer_page_agent.render ~size ~model
  | Model.Page_id.Shell_security -> Renderer_page_shell_security.render ~size ~model
;;
