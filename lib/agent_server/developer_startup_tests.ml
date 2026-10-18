let%test_module "declarative Developer startup admission" =
  (module struct
    open! Core
    module P = Agent_protocol
    module Config = Developer_startup_configuration

    let valid_selector =
      { Config.Principal_selector.principal_id =
          (match P.Id.Principal.of_string "pri_valid" with
           | Ok id -> id
           | Error _ -> failwith "invalid fixture principal")
      ; authentication_kind = "http.static_bearer"
      }
    ;;

    let config principal =
      { Config.selections =
          [ { Config.Selection.source =
                Config.Source.Document_source
                  { origin = Config.Source.Supplied_document; script_id = "main" }
            ; target = Config.Target.One_off_v1
            ; principals = [ principal ]
            }
          ]
      ; capacity = Config.Capacity.standard_v1
      ; model = Config.Disabled
      ; approvals = Config.Disabled
      ; repl = Config.Disabled
      ; portable = true
      }
    ;;

    let%test_unit "invalid derived principal and duplicate source selection deny" =
      let invalid_principal =
        Config.Principal_selector.t_of_sexp
          (Sexp.of_string
             "((principal_id malformed) (authentication_kind http.static_bearer))")
      in
      assert (Result.is_error (Config.validate (config invalid_principal)));
      let valid = config valid_selector in
      assert (Result.is_ok (Config.validate valid));
      assert (
        Result.is_error
          (Config.validate
             { valid with selections = valid.selections @ valid.selections }))
    ;;

    let%test_unit "durable run capacity is independent of concurrent commands" =
      let capacity =
        { Config.Capacity.standard_v1 with max_runs = 2; max_active_commands = 4 }
      in
      assert (Result.is_ok (Config.Capacity.validate capacity));
      assert (
        Result.is_ok
          (Developer_startup_limits.create
             ~capacity
             ~approvals:Config.Disabled
             ~repl:Config.Disabled
             ~portable:true
             ~initialize_limits:Daemon.default_options.protocol_limits));
      assert (Result.is_error (Config.Capacity.validate { capacity with max_runs = 4097 }))
    ;;
  end)
;;
