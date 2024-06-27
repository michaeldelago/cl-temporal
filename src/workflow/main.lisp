(in-package :cl-temporal.workflow)

(serapeum.exporting:defparameter *created-workflows* (serapeum:dict))

(serapeum.exporting:defclass workflow (cl-temporal:executable)
  ((task-token
    :initarg :task-token
    :accessor task-token)
   (run-id
    :initarg :run-id
    :accessor run-id)
   ;; (namespace
   ;;  :initarg :namespace
   ;;  :accessor namespace)
   (history
    :initarg :history
    :accessor history)
   (next-page-token
    :initarg :next-page-token
    :accessor next-page-token)
   (task-queue
    :initarg :task-queue
    :accessor task-queue)
   (scheduled-time
    :initarg :scheduled-time
    :accessor scheduled-time)
   (started-time
    :initarg :started-time
    :accessor started-time)
   (queries
    :initarg :queries
    :accessor queries)
   (messages
    :initarg :messages
    :accessor messages)))

(defmethod make-workflow ((workflow temporal.workflowservice:poll-workflow-task-queue-response))
  (with-slots (workflow-type execution-info task-token history next-page-token task-queue scheduled-time started-time queries messages)
      workflow
    (let* ((workflow-name (string-upcase (format nil "~a-WORKFLOW" (slot-value workflow-type 'name))))
           (wf-class (find-symbol workflow-name :cl-temporal)))
      (make-instance class :task-queue task-queue
                           :run-id (slot-value execution-info 'run-id)
                           :history history
                           :next-page-token next-page-token
                           :scheduled-time scheduled-time
                           :started-time started-time
                           :queries queries
                           :messages messages))))


(serapeum.exporting:defmethod execute ((wf workflow))
  (error "method not implemented"))

(defmethod execute :around ((workflow workflow))
  "Handles the communications with temporal around workflow start and end"
  (when (next-method-p)
    (handler-case
        (call-next-method)
      (error (c)
        (log:warn "error" workflow)
        (cl-temporal:fail-workflow)))
    (cl-temporal:finish-workflow (temporal.workflowservice:task-token (task workflow)))))

(serapeum.exporting:defmacro defworkflow (name (&rest args) &body body)
  "Create a workflow with name NAME and create an execute method with ARGS and BODY.
Note that this only defines a workflow and does not register it to a worker.

(defworkflow hello (name)
  (format nil \"Hello, ~A~&\" name))"
  (let ((wf-name (alexandria:symbolicate name '- 'workflow))
        (closure (gensym)))
    (when (gethash wf-name *created-workflows*)
      (warn 'workflow-already-defined :message "workflow is already defined" :name name))
    (setf (gethash name *created-workflows*) wf-name)
    `(progn
       (serapeum.exporting:defclass ,wf-name (cl-temporal.workflow:workflow)
         (workflow-type
          :initform ,name
          :reader workflow-type
          :allocation :class))
       (defmethod execute ((wf ,wf-name))
         (labels ((,closure ,args
                    ,@body))
           (,closure))))))

(defun now ()
  (local-time:timestamp-to-unix (local-time:universal-to-timestamp (get-universal-time))))

(defmethod add-event ((workflow workflow) event-type &optional attrs)
  (let ((temporal.history:make-history-event
          :event-time (cl-protobufs.google.protobuf.timestamp:make-timestamp (now))
          :attributes attrs
          :event-id (current-event-id workflow)

          )))

  )


(defmethod add-workflow-execution-completed ((workflow workflow) event)
  (add-event workflow (temporal.history:make-workflow-execution-completed-event-attributes
                       :workflow-task-completed-event-id (id event))))
