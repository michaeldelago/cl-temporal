(in-package :cl-temporal)

;; Used for management of the connection to Temporal.
;; These are used where needed inside these functions
(defvar *channel* nil "gRPC communication channel to temporal")
(defvar *identity* "unknown-worker" "identity of temporal worker/connection")
(defvar *namespace* "default" "temporal namespace with which to work")
(defvar *call-deadline* grpc:*call-deadline* "")
(defparameter *poll-mutex* nil)

(serapeum.exporting:defun with-env (fn)
  (serapeum:dynamic-closure '(cl-temporal:*channel* cl-temporal:*namespace* cl-temporal:*identity* cl-temporal:*call-deadline*)
                            fn))

(defun call-with-connection (thunk &key host port (namespace *namespace*) (identity *identity*) (insecure t) (default-call-deadline *call-deadline*))
  (declare (ignore insecure))
  (when (not insecure)
    (warn 'insecure-not-implemented :message "insecure grpc connections to temporal have not yet been implemented"))
  (grpc:init-grpc)
  (let ((*namespace* namespace)
        (*identity* identity)
        (*call-deadline* default-call-deadline))
    (grpc:with-insecure-channel (*channel* (concatenate 'string host ":" (write-to-string port)))
      (funcall thunk))))



(defmacro with-connection ((&rest keys &key &allow-other-keys) &body body)
  (serapeum:with-thunk (body)
    `(call-with-connection ,body ,@keys)))

(defmacro assert-connection ()
  `(unless *channel* (error 'null-channel :channel *channel*)))

(defmacro assert-deadline ()
  `(unless grpc:*call-deadline* (error 'no-deadline-set :deadline grpc:*call-deadline*)))

(defun register-namespace (&key (namespace *namespace*) (description "") (is-global nil) (retention-period 10) (data nil))
  (assert-connection)
  (let ((req (temporal.workflowservice:make-register-namespace-request
              :namespace namespace
              :description description
              :is-global-namespace is-global
              :workflow-execution-retention-period (cl-protobufs.google.protobuf:make-duration :seconds (* 24 60 60 retention-period))
              :data data)))
    (temporal.workflowservice-rpc:call-register-namespace *channel* req)))

(serapeum.exporting:defun poll-workflow-task-queue (task-queue &key (namespace *namespace*) (binary-checksum nil))
  (assert-connection)
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

(serapeum.exporting:defun poll-activity-task-queue (task-queue &key (namespace *namespace*) (binary-checksum nil))
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

;; TODO implement encodeFailureAttributes/decodeFailureAttributes
(serapeum.exporting:defun fail-workflow (task-token &key (namespace *namespace*) (condition nil) (stack-trace nil))
  (assert-connection)
  (let* ((cause (cl-protobufs.temporal.api.enums.v1:workflow-task-failed-cause-keyword-to-int :workflow-task-failed-cause-unspecified))
         (failure (temporal.failure:make-failure :message cond :source "CommonLispSDK" :stack-trace stack-trace))
         (workflow-task-failed-req (temporal.workflowservice:make-respond-workflow-task-failed-request
                                    :task-token task-token
                                    :cause cause
                                    :failure failure
                                    :identity *identity*
                                    :namespace namespace
                                    :messages nil
                                    :worker-version-stamp nil)))
    (temporal.workflowservice-rpc:call-respond-workflow-task-failed *channel* workflow-task-failed-req)))

(serapeum.exporting:defun finish-workflow (task-token &key (namespace *namespace*) (sticky-attributes nil) (commands nil) (return-new-workflow-task nil) (query-results nil) (messages nil) (worker-version-stamp nil) (sdk-metadata nil) (metering-metadata nil))
  (let ((workflow-task-complete (temporal.workflowservice:make-respond-workflow-task-completed-request
                                 :task-token task-token
                                 :commands commands
                                 :identity *identity*
                                 :sticky-attributes sticky-attributes
                                 :worker-version-stamp worker-version-stamp
                                 :query-results query-results
                                 :messages messages
                                 :sdk-metadata sdk-metadata
                                 :metering-metadata metering-metadata)))
    (temporal.workflowservice-rpc:call-respond-workflow-task-completed *channel* workflow-task-complete)))
