let%test_module "declarative Developer startup admission" =
  (module struct
    open! Core
    module P = Agent_protocol
    module Config = Developer_startup_configuration

    let must = function
      | Ok value -> value
      | Error error -> raise_s [%sexp "invalid startup fixture", (error : P.Error.t)]
    ;;

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

    let%test_unit "portable REPL preset follows the installed owner and Initialize" =
      let admitted ~capacity ~repl ~portable =
        Developer_startup_limits.create
          ~capacity
          ~approvals:Config.Disabled
          ~repl
          ~portable
          ~initialize_limits:Daemon.default_options.protocol_limits
      in
      let standard = Config.Capacity.standard_v1 in
      let preset =
        admitted ~capacity:standard ~repl:Config.Standard_v1 ~portable:true |> must
      in
      let owner = Developer_startup_limits.repl preset |> Option.value_exn in
      let policy = Developer_startup_limits.portable preset |> Option.value_exn in
      let wire = Developer_endpoint.Policy.repl_wire_limits policy |> Option.value_exn in
      let limits = Developer_repl_epoch.Configuration.limits owner in
      assert (Int.equal (Developer_repl_epoch.Limits.max_receipt_bytes limits) 4096);
      assert (
        Int.equal
          (P.Developer_repl_wire.Limits.max_source_bytes wire)
          standard.max_repl_source_bytes);
      assert (
        Int.equal
          (P.Developer_repl_wire.Limits.max_registry_entries wire)
          (standard.max_repl_instances * ((2 * standard.max_repl_keys_per_instance) + 1)));
      assert (
        Result.is_ok
          (Developer_portable_admission.check
             ~policy
             ~initialize:Daemon.default_options.protocol_limits
             ~budgets:(Developer_startup_limits.host preset).budgets
             ~approval:None
             ~repl:(Some owner)));
      let custom =
        { standard with
          max_repl_instances = 3
        ; max_repl_keys_per_instance = 17
        ; max_retained_tasks = 11
        ; max_repl_source_bytes = 100_000
        }
      in
      let customized =
        admitted ~capacity:custom ~repl:Config.Standard_v1 ~portable:true |> must
      in
      let customized_wire =
        Developer_startup_limits.portable customized
        |> Option.value_exn
        |> Developer_endpoint.Policy.repl_wire_limits
        |> Option.value_exn
      in
      assert (
        Int.equal
          (P.Developer_repl_wire.Limits.max_registry_entries customized_wire)
          (3 * ((2 * 17) + 1)));
      assert (Int.equal (P.Developer_repl_wire.Limits.max_page_items customized_wire) 11);
      assert (
        Int.equal (P.Developer_repl_wire.Limits.max_source_bytes customized_wire) 100_000);
      let only_repl =
        admitted ~capacity:standard ~repl:Config.Standard_v1 ~portable:false |> must
      in
      assert (Option.is_none (Developer_startup_limits.portable only_repl));
      let only_portable =
        admitted ~capacity:standard ~repl:Config.Disabled ~portable:true |> must
      in
      assert (
        Option.is_none
          (Developer_endpoint.Policy.repl_wire_limits
             (Developer_startup_limits.portable only_portable |> Option.value_exn)));
      let narrow_initialize =
        { Daemon.default_options.protocol_limits with max_request_bytes = 1024 }
      in
      assert (
        Result.is_error
          (Developer_startup_limits.create
             ~capacity:standard
             ~approvals:Config.Disabled
             ~repl:Config.Standard_v1
             ~portable:true
             ~initialize_limits:narrow_initialize));
      assert (
        Result.is_error
          (admitted
             ~capacity:{ standard with max_repl_source_bytes = Int.max_value }
             ~repl:Config.Standard_v1
             ~portable:true));
      assert (
        Result.is_error
          (admitted
             ~capacity:{ standard with max_repl_instances = Int.max_value }
             ~repl:Config.Standard_v1
             ~portable:true))
    ;;
  end)
;;
