open! Core
module C = Authoring_codec
module A = Authoring_common
module X = Authoring_context

let map decode wrap json = Result.map (decode json) ~f:wrap

let missing method_ =
  Protocol_error.create
    Protocol_error.Method_not_found
    ~message:("unknown authoring method: " ^ method_)
    ~retryable:false
    ()
;;

let empty json = Result.map (C.fields ~allowed:[] json) ~f:(fun _ -> ())

module Request = struct
  type t =
    | Capabilities
    | Roots
    | Host_list of X.Source_read.Host_list_request.t
    | Host_read of X.Source_read.Host_read_request.t
    | Context_open of X.Context.Open_request.t
    | Context_refresh of X.Context.Refresh_request.t
    | Context_close of X.Context.Close_request.t
    | Source_list of X.Source_read.List_request.t
    | Source_read of X.Source_read.Read_request.t
    | Document_version of X.Document.Version_request.t
    | Document_replace of X.Document.Replace_request.t
    | Document_close of X.Document.Close_request.t
  [@@deriving sexp_of]

  let method_name = function
    | Capabilities -> A.Method.to_string A.Method.Capabilities
    | Roots -> A.Method.to_string A.Method.Roots
    | Host_list _ -> A.Method.to_string A.Method.Host_list
    | Host_read _ -> A.Method.to_string A.Method.Host_read
    | Context_open _ -> A.Method.to_string A.Method.Context_open
    | Context_refresh _ -> A.Method.to_string A.Method.Context_refresh
    | Context_close _ -> A.Method.to_string A.Method.Context_close
    | Source_list _ -> A.Method.to_string A.Method.Source_list
    | Source_read _ -> A.Method.to_string A.Method.Source_read
    | Document_version _ -> A.Method.to_string A.Method.Document_version
    | Document_replace _ -> A.Method.to_string A.Method.Document_replace
    | Document_close _ -> A.Method.to_string A.Method.Document_close
  ;;

  let params = function
    | Capabilities | Roots -> `Object []
    | Host_list request -> X.Source_read.Host_list_request.to_json request
    | Host_read request -> X.Source_read.Host_read_request.to_json request
    | Context_open request -> X.Context.Open_request.to_json request
    | Context_refresh request -> X.Context.Refresh_request.to_json request
    | Context_close request -> X.Context.Close_request.to_json request
    | Source_list request -> X.Source_read.List_request.to_json request
    | Source_read request -> X.Source_read.Read_request.to_json request
    | Document_version request -> X.Document.Version_request.to_json request
    | Document_replace request -> X.Document.Replace_request.to_json request
    | Document_close request -> X.Document.Close_request.to_json request
  ;;

  let decoders =
    [ "authoring.capabilities", map empty (fun () -> Capabilities)
    ; "authoring.roots", map empty (fun () -> Roots)
    ; ( "authoring.host.list"
      , map X.Source_read.Host_list_request.of_json (fun x -> Host_list x) )
    ; ( "authoring.host.read"
      , map X.Source_read.Host_read_request.of_json (fun x -> Host_read x) )
    ; ( "authoring.context.open"
      , map X.Context.Open_request.of_json (fun x -> Context_open x) )
    ; ( "authoring.context.refresh"
      , map X.Context.Refresh_request.of_json (fun x -> Context_refresh x) )
    ; ( "authoring.context.close"
      , map X.Context.Close_request.of_json (fun x -> Context_close x) )
    ; ( "authoring.source.list"
      , map X.Source_read.List_request.of_json (fun x -> Source_list x) )
    ; ( "authoring.source.read"
      , map X.Source_read.Read_request.of_json (fun x -> Source_read x) )
    ; ( "authoring.document.version"
      , map X.Document.Version_request.of_json (fun x -> Document_version x) )
    ; ( "authoring.document.replace"
      , map X.Document.Replace_request.of_json (fun x -> Document_replace x) )
    ; ( "authoring.document.close"
      , map X.Document.Close_request.of_json (fun x -> Document_close x) )
    ]
  ;;

  let of_method_and_params ~method_ ~params =
    match List.Assoc.find decoders method_ ~equal:String.equal with
    | Some decode -> decode params
    | None -> Error (missing method_)
  ;;

  let supported_methods = List.map decoders ~f:fst
end

module Response = struct
  type t =
    | Capabilities of A.Capabilities.t
    | Roots of X.Source_read.Roots_result.t
    | Host_list of X.Source_read.Host_list_result.t
    | Host_read of X.Source_read.Host_read_result.t
    | Context_open of X.Context.t
    | Context_refresh of X.Context.t
    | Context_close of X.Context.Close_result.t
    | Source_list of X.Source_read.List_result.t
    | Source_read of X.Source_read.Read_result.t
    | Document_version of X.Document.Version.t
    | Document_replace of X.Document.Result.t
    | Document_close of X.Document.Result.t
  [@@deriving sexp_of]

  let method_name = function
    | Capabilities _ -> A.Method.to_string A.Method.Capabilities
    | Roots _ -> A.Method.to_string A.Method.Roots
    | Host_list _ -> A.Method.to_string A.Method.Host_list
    | Host_read _ -> A.Method.to_string A.Method.Host_read
    | Context_open _ -> A.Method.to_string A.Method.Context_open
    | Context_refresh _ -> A.Method.to_string A.Method.Context_refresh
    | Context_close _ -> A.Method.to_string A.Method.Context_close
    | Source_list _ -> A.Method.to_string A.Method.Source_list
    | Source_read _ -> A.Method.to_string A.Method.Source_read
    | Document_version _ -> A.Method.to_string A.Method.Document_version
    | Document_replace _ -> A.Method.to_string A.Method.Document_replace
    | Document_close _ -> A.Method.to_string A.Method.Document_close
  ;;

  let to_json = function
    | Capabilities value -> A.Capabilities.to_json value
    | Roots value -> X.Source_read.Roots_result.to_json value
    | Host_list value -> X.Source_read.Host_list_result.to_json value
    | Host_read value -> X.Source_read.Host_read_result.to_json value
    | Context_open value | Context_refresh value -> X.Context.to_json value
    | Context_close value -> X.Context.Close_result.to_json value
    | Source_list value -> X.Source_read.List_result.to_json value
    | Source_read value -> X.Source_read.Read_result.to_json value
    | Document_version value -> X.Document.Version.to_json value
    | Document_replace value | Document_close value -> X.Document.Result.to_json value
  ;;

  let decoders =
    [ "authoring.capabilities", map A.Capabilities.of_json (fun x -> Capabilities x)
    ; "authoring.roots", map X.Source_read.Roots_result.of_json (fun x -> Roots x)
    ; ( "authoring.host.list"
      , map X.Source_read.Host_list_result.of_json (fun x -> Host_list x) )
    ; ( "authoring.host.read"
      , map X.Source_read.Host_read_result.of_json (fun x -> Host_read x) )
    ; "authoring.context.open", map X.Context.of_json (fun x -> Context_open x)
    ; "authoring.context.refresh", map X.Context.of_json (fun x -> Context_refresh x)
    ; ( "authoring.context.close"
      , map X.Context.Close_result.of_json (fun x -> Context_close x) )
    ; ( "authoring.source.list"
      , map X.Source_read.List_result.of_json (fun x -> Source_list x) )
    ; ( "authoring.source.read"
      , map X.Source_read.Read_result.of_json (fun x -> Source_read x) )
    ; ( "authoring.document.version"
      , map X.Document.Version.of_json (fun x -> Document_version x) )
    ; ( "authoring.document.replace"
      , map X.Document.Result.of_json (fun x -> Document_replace x) )
    ; ( "authoring.document.close"
      , map X.Document.Result.of_json (fun x -> Document_close x) )
    ]
  ;;

  let of_method_and_json ~method_ json =
    match List.Assoc.find decoders method_ ~equal:String.equal with
    | Some decode -> decode json
    | None -> Error (missing method_)
  ;;

  let supported_methods = List.map decoders ~f:fst
end
