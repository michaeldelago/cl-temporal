(in-package :cl-temporal)

(defvar *channel*)
(defvar *identity* "foo-bar-worker")
(defparameter *poll-mutex* nil)

(defun call-with-connection (thunk &key host port task-queue (namespace "default") (insecure t))
  (declare (ignore namespace insecure task-queue))
  (grpc:init-grpc)
  (grpc:with-insecure-channel (*channel* (concatenate 'string host ":" (write-to-string port)))
    (funcall thunk)))


(defmacro with-connection ((&rest keys &key &allow-other-keys) &body body)
  (serapeum:with-thunk (body)
    `(call-with-connection ,body ,@keys)))

(defmacro assert-connection ()
  `(unless *channel* (error 'null-channel :channel *channel*)))

(defun register-namespace (name &key (description "") (is-global nil) (retention-period 10) (data nil))
  (assert-connection)
  (let ((req (temporal.workflowservice:make-register-namespace-request
              :namespace name
              :description description
              :is-global-namespace is-global
              :workflow-execution-retention-period (cl-protobufs.google.protobuf:make-duration :seconds (* 24 60 60 retention-period))
              :data data)))
    (temporal.workflowservice-rpc:call-register-namespace *channel* req)))

(serapeum.exporting:defun poll-workflow-task-queue (namespace task-queue &optional (binary-checksum nil))
  ;; (assert-connection)
  (let ((poll-workflow-request (temporal.workflowservice:make-poll-workflow-task-queue-request
                                :namespace namespace
                                :task-queue (temporal.taskqueue:make-task-queue :name task-queue)
                                :identity *identity*
                                :worker-version-capabilities (when binary-checksum
                                                               (temporal.common:make-worker-version-capabilities :build-id binary-checksum)))))
    (serapeum:synchronized (*poll-mutex*)
      (log:info "polling workflow")
      (let ((grpc::*call-deadline* 30))
        (temporal.workflowservice-rpc:call-poll-workflow-task-queue *channel* poll-workflow-request)))))

(serapeum.exporting:defun poll-activity-task-queue (namespace task-queue &optional (binary-checksum nil))
  (assert-connection)
  (let ((poll-activity-request (temporal.workflowservice:make-poll-activity-task-queue-request
                                :namespace namespace
                                :task-queue (temporal.taskqueue:make-task-queue :name task-queue)
                                :identity *identity*
                                :worker-version-capabilities (when binary-checksum
                                                               (temporal.common:make-worker-version-capabilities :build-id binary-checksum)))))
    (serapeum:synchronized (*poll-mutex*)
      (log:info "polling activity")
      (temporal.workflowservice-rpc:call-poll-activity-task-queue *channel* poll-activity-request))))
