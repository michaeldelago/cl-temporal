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
  :serial t
  :components ((:module "proto"
                :components
                ((:module "google"
                  :pathname ""
                  :components ((:protobuf-source-file "google/api/http")
                               (:protobuf-source-file "google/api/annotations"
                                :depends-on ("google/api/http"))))
                 (:module "enums"
                  :pathname ""
                  :depends-on ("google")
                  :components ((:protobuf-source-file "temporal/api/enums/v1/batch_operation")
                               (:protobuf-source-file "temporal/api/enums/v1/command_type")
                               (:protobuf-source-file "temporal/api/enums/v1/common")
                               (:protobuf-source-file "temporal/api/enums/v1/event_type")
                               (:protobuf-source-file "temporal/api/enums/v1/namespace")
                               (:protobuf-source-file "temporal/api/enums/v1/query")
                               (:protobuf-source-file "temporal/api/enums/v1/reset")
                               (:protobuf-source-file "temporal/api/enums/v1/schedule")
                               (:protobuf-source-file "temporal/api/enums/v1/update")
                               (:protobuf-source-file "temporal/api/enums/v1/workflow")
                               (:protobuf-source-file "temporal/api/enums/v1/failed_cause")
                               (:protobuf-source-file "temporal/api/enums/v1/task_queue")))
                 (:protobuf-source-file "temporal/api/common/v1/message")
                 (:module "sdk"
                  :pathname ""
                  :depends-on ("temporal/api/common/v1/message")
                  :components ((:protobuf-source-file "temporal/api/sdk/v1/workflow_metadata")
                               (:protobuf-source-file "temporal/api/sdk/v1/user_metadata")
                               (:protobuf-source-file "temporal/api/sdk/v1/task_complete_metadata")))
                 (:module "messages"
                  :pathname ""
                  :depends-on ("enums" "sdk" "temporal/api/common/v1/message")
                  :components ((:protobuf-source-file "temporal/api/batch/v1/message")
                               (:protobuf-source-file "temporal/api/command/v1/message"
                                :depends-on ("temporal/api/failure/v1/message"
                                             "temporal/api/taskqueue/v1/message"))
                               (:protobuf-source-file "temporal/api/errordetails/v1/message")
                               (:protobuf-source-file "temporal/api/export/v1/message"
                                :depends-on ("temporal/api/history/v1/message"))
                               (:protobuf-source-file "temporal/api/failure/v1/message")
                               (:protobuf-source-file "temporal/api/filter/v1/message")
                               (:protobuf-source-file "temporal/api/history/v1/message"
                                :depends-on ("temporal/api/failure/v1/message"
                                             "temporal/api/taskqueue/v1/message"
                                             "temporal/api/update/v1/message"
                                             "temporal/api/workflow/v1/message"))
                               (:protobuf-source-file "temporal/api/namespace/v1/message")
                               (:protobuf-source-file "temporal/api/nexus/v1/message")
                               (:protobuf-source-file "temporal/api/protocol/v1/message")
                               (:protobuf-source-file "temporal/api/query/v1/message")
                               (:protobuf-source-file "temporal/api/replication/v1/message")
                               (:protobuf-source-file "temporal/api/schedule/v1/message"
                                :depends-on ("temporal/api/workflow/v1/message" ))
                               (:protobuf-source-file "temporal/api/taskqueue/v1/message")
                               (:protobuf-source-file "temporal/api/update/v1/message"
                                :depends-on ("temporal/api/failure/v1/message"))
                               (:protobuf-source-file "temporal/api/version/v1/message")
                               (:protobuf-source-file "temporal/api/workflow/v1/message"
                                :depends-on ("temporal/api/failure/v1/message"
                                             "temporal/api/taskqueue/v1/message"))))
                 (:module "service"
                  :pathname ""
                  :depends-on ("messages")
                  :components ((:module "operatorservice"
                                :pathname ""
                                :components ((:protobuf-source-file "temporal/api/operatorservice/v1/request_response")
                                             (:protobuf-source-file "temporal/api/operatorservice/v1/service"
                                              :depends-on ("temporal/api/operatorservice/v1/request_response"))))
                               (:module "workflowservice"
                                :pathname ""
                                :depends-on ("operatorservice")
                                :components ((:protobuf-source-file "temporal/api/workflowservice/v1/request_response")
                                             (:protobuf-source-file "temporal/api/workflowservice/v1/service"
                                              :depends-on ("temporal/api/workflowservice/v1/request_response"))))))))))
