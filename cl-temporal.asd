(defsystem "cl-temporal"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:cl-temporal/proto :grpc :serapeum :alexandria :log4cl :log4cl-extras :bordeaux-threads :eager-future2)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "executable")
                 (:file "connection")
                 (:module "workflow"
                  :serial t
                  :components ((:file "package")
                               (:file "main")
                               (:file "poller")))
                 (:module "activity"
                  :serial t
                  :components ((:file "package")
                               (:file "main")
                               (:file "poller")))
                 (:file "worker")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-temporal/tests"))))

(defsystem "cl-temporal/tests"
  :author ""
  :license ""
  :depends-on ("cl-temporal"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-temporal"
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "cl-temporal/proto"
  :version "1.34.0"
  :depends-on (:cl-protobufs)
  :defsystem-depends-on (:cl-protobufs.asdf)
  :components ((:module "proto"
                :serial t
                :components
                ((:module "enums"
                  :pathname ""
                  :components ((:protobuf-source-file "schedule"
                                :proto-pathname "temporal/api/enums/v1/schedule.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "common"
                                :proto-pathname "temporal/api/enums/v1/common.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "task_queue"
                                :proto-pathname "temporal/api/enums/v1/task_queue.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "command_type"
                                :proto-pathname "temporal/api/enums/v1/command_type.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "batch_operation"
                                :proto-pathname "temporal/api/enums/v1/batch_operation.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "reset"
                                :proto-pathname "temporal/api/enums/v1/reset.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "failed_cause"
                                :proto-pathname "temporal/api/enums/v1/failed_cause.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "workflow"
                                :proto-pathname "temporal/api/enums/v1/workflow.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "update"
                                :proto-pathname "temporal/api/enums/v1/update.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "event_type"
                                :proto-pathname "temporal/api/enums/v1/event_type.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "query"
                                :proto-pathname "temporal/api/enums/v1/query.proto"
                                :proto-search-path ("temporal/api/enums/v1"))
                               (:protobuf-source-file "namespace"
                                :proto-pathname "temporal/api/enums/v1/namespace.proto"
                                :proto-search-path ("temporal/api/enums/v1"))))
                 (:module "sdk"
                  :pathname ""
                  :components ((:protobuf-source-file "workflow_metadata"
                                :proto-pathname "temporal/api/sdk/v1/workflow_metadata.proto"
                                :proto-search-path ("temporal/api/sdk/v1"))
                               (:protobuf-source-file "task_complete_metadata"
                                :proto-pathname "temporal/api/sdk/v1/task_complete_metadata.proto"
                                :proto-search-path ("temporal/api/sdk/v1"))))
                 (:module "messages"
                  :pathname ""
                  :serial t
                  :depends-on ("enums" "sdk")
                  :components ((:module "common"
                                :pathname "temporal/api/common/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "batch"
                                :pathname "temporal/api/batch/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "errordetails"
                                :pathname "temporal/api/errordetails/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))

                               (:module "failure"
                                :pathname "temporal/api/failure/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "filter"
                                :pathname "temporal/api/filter/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "taskqueue"
                                :pathname "temporal/api/taskqueue/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "workflow"
                                :pathname "temporal/api/workflow/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "update"
                                :pathname "temporal/api/update/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "history"
                                :pathname "temporal/api/history/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "export"
                                :pathname "temporal/api/export/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "namespace"
                                :pathname "temporal/api/namespace/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "nexus"
                                :pathname "temporal/api/nexus/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "protocol"
                                :pathname "temporal/api/protocol/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "query"
                                :pathname "temporal/api/query/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "replication"
                                :pathname "temporal/api/replication/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "schedule"
                                :pathname "temporal/api/schedule/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "version"
                                :pathname "temporal/api/version/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "command"
                                :pathname "temporal/api/command/v1"
                                :components ((:protobuf-source-file "message"
                                              :proto-search-path ("./" "../../../../"))))))
                 (:module "services"
                  :pathname ""
                  :components ((:module "operator"
                                :pathname "temporal/api/operatorservice/v1"
                                :components ((:protobuf-source-file "request_response"
                                              :proto-search-path ("./" "../../../../"))
                                             (:protobuf-source-file "service"
                                              :proto-search-path ("./" "../../../../"))))
                               (:module "workflow"
                                :pathname "temporal/api/workflowservice/v1"
                                :components ((:protobuf-source-file "request_response"
                                              :proto-search-path ("./" "../../../../"))
                                             (:protobuf-source-file "service"
                                              :proto-search-path ("./" "../../../../"))))))))))
