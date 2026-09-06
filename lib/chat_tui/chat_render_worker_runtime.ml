open Core
module Source = Highlight_grammar_discovery.Source
module Job = Chat_message_render_job

module Config = struct
  type t =
    { custom_grammars : Source.t list
    ; theme_generation : int
    ; grammar_generation : int
    }

  let create ~custom_grammars ~theme_generation ~grammar_generation =
    { custom_grammars; theme_generation; grammar_generation }
  ;;

  let theme_generation t = t.theme_generation
  let grammar_generation t = t.grammar_generation
end

type resources =
  { registry : Highlight_tm_loader.registry
  ; engine : Highlight_tm_engine.t
  ; code_cache : Job.Code_cache.t
  ; highlight_cache : Job.Highlight_cache.t
  ; prepared_cache : Job.Prepared_cache.t
  ; wrapped_cache : Job.Wrapped_cache.t
  ; config : Config.t
  }

type t =
  { code_cache_capacity : int
  ; mutable resources : resources
  }

let build ~config ~code_cache_capacity =
  let open Or_error.Let_syntax in
  let registry = Highlight_registry.create () in
  let%map () =
    List.fold_result config.Config.custom_grammars ~init:() ~f:(fun () source ->
      Highlight_tm_loader.add_grammar_jsonaf registry (Source.json source)
      |> Or_error.tag
           ~tag:(sprintf "Failed to load custom TextMate grammar %S" (Source.name source)))
  in
  let engine =
    Highlight_tm_engine.create ~theme:Highlight_theme.github_dark
    |> Highlight_tm_engine.with_registry ~registry
  in
  { registry
  ; engine
  ; code_cache = Job.Code_cache.create ~capacity:code_cache_capacity
  ; highlight_cache = Job.Highlight_cache.create ~capacity:code_cache_capacity
  ; prepared_cache = Job.Prepared_cache.create ~capacity:code_cache_capacity
  ; wrapped_cache = Job.Wrapped_cache.create ~capacity:code_cache_capacity
  ; config
  }
;;

let create ~config ~code_cache_capacity () =
  if code_cache_capacity < 0
  then invalid_arg "Chat_render_worker_runtime.create: negative cache capacity";
  build ~config ~code_cache_capacity
  |> Or_error.map ~f:(fun resources -> { code_cache_capacity; resources })
;;

let rebuild t ~config =
  build ~config ~code_cache_capacity:t.code_cache_capacity
  |> Or_error.map ~f:(fun resources -> t.resources <- resources)
;;

let theme_generation t = Config.theme_generation t.resources.config
let grammar_generation t = Config.grammar_generation t.resources.config

let render t ?is_cancelled job =
  let resources = t.resources in
  let runtime =
    Job.Runtime.create
      ~hi_engine:resources.engine
      ~theme_generation:(theme_generation t)
      ~grammar_generation:(grammar_generation t)
      ?is_cancelled
      ~code_cache:resources.code_cache
      ~highlight_cache:resources.highlight_cache
      ~prepared_cache:resources.prepared_cache
      ~wrapped_cache:resources.wrapped_cache
      ()
  in
  Renderer_component_message.render_detached ~runtime job
;;

module For_testing = struct
  let has_language t language =
    Highlight_tm_loader.find_grammar_by_lang_tag t.resources.registry language
    |> Option.is_some
  ;;

  let code_cache_length t = Job.Code_cache.length t.resources.code_cache
  let highlight_cache_length t = Job.Highlight_cache.length t.resources.highlight_cache
  let prepared_cache_length t = Job.Prepared_cache.length t.resources.prepared_cache
  let wrapped_cache_stats t = Job.Wrapped_cache.stats t.resources.wrapped_cache
end
