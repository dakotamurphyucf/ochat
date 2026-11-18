module X = Persistence_codec
include Agent_protocol.Goal_command_receipt

let shape =
  X.shape_exn
    [ "principal_id", Document_schema.Shape.value
    ; "key", Document_schema.Shape.value
    ; "request_sha256", Document_schema.Shape.value
    ; "goal_id", Document_schema.Shape.value
    ; "goal_revision", Document_schema.Shape.value
    ; "catalog_revision", Document_schema.Shape.value
    ; "source", X.nullable_shape Run_record_shapes.source
    ; "accepted_at", Document_schema.Shape.value
    ]
;;
