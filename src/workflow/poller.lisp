(in-package :cl-temporal.workflow)

(defparameter *default-options*
  (list))

(defstruct (workflow-poller (:constructor make-workflow-poller (namespace task-queue workflow-lookup)))
  namespace
  task-queue
  workflow-lookup
  (shutting-down nil)
  (polling nil)
  (config (list))
  (middleware (list))
  (options *default-options*)
  (thread-handle nil))

(defun start-polling (poller)
  (with-slots (shutting-down polling thread-handle namespace task-queue)
      poller
    (setf shutting-down nil)
    (setf polling t)
    (setf thread-handle
          ;; (bt2:make-thread
          ;;  (let ((grpc-channel cl-temporal::*channel*))
          ;;    (lambda ()
          ;;      (let ((cl-temporal::*channel* grpc-channel)
          ;;            (cl-temporal::*namespace* namespace))
          ;;        (poll-loop poller))))
          ;;  :name (format nil "poller/workflow/~a/~a" namespace task-queue))
          (bt2:make-thread
           (cl-temporal:with-env
               (serapeum:op (poll-loop poller)))))))

(defun poll-loop (poller)
  (with-slots (namespace task-queue)
      poller
    (log:info "workflow polling start" namespace task-queue)
    (loop named task-poller
          while (null (workflow-poller-shutting-down poller))
          for task = (cl-temporal:poll-workflow-task-queue task-queue)
          do (progn (when (temporal.workflowservice:workflow-type task)
                      (log:info "found task" namespace task-queue)
                      (let ((wf (make-workflow task)))
                        (execute wf)))
                    (sleep 0.5)))))
