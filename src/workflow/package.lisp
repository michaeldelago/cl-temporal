(defpackage cl-temporal.workflow
  (:use :cl)
  (:local-nicknames
   (:temporal.workflowservice :cl-protobufs.temporal.api.workflowservice.v1)
   (:temporal.common :cl-protobufs.temporal.api.common.v1)
   (:temporal.enums :cl-protobufs.temporal.api.enums.v1)))
