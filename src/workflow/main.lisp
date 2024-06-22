(in-package :cl-temporal.workflow)

(serapeum.exporting:defclass workflow (cl-temporal:executable)
  ((workflow-task-queue-response
    :initarg :task
    :accessor task)))

(serapeum.exporting:defmethod execute ((wf workflow))
  (error "method not implemented"))

(serapeum:eval-always
  (serapeum.exporting:defparameter created-workflows (serapeum:dict)))

(serapeum.exporting:defmacro defworkflow (name (&rest args) &body body)
  "Create a workflow with name NAME and create an execute method with ARGS and BODY.
Note that this only defines a workflow and does not register it to a worker.

(defworkflow hello (name)
  (format nil \"Hello, ~A~&\" name))"
  (let ((wf-name (alexandria:symbolicate name '- 'workflow))
        (closure (gensym)))
    (when (gethash wf-name created-workflows)
      (warn 'workflow-already-defined :message "workflow is already defined" :name name))
    (setf (gethash name created-workflows) wf-name)
    `(progn
       (serapeum.exporting:defclass ,wf-name (cl-temporal.workflow:workflow) nil)
       (defmethod execute ((wf ,wf-name))
         (labels ((,closure ,args
                    ,@body))
           (,closure))))))

(trace make-workflow-from-raw)

(defun make-workflow-from-raw (task)
  (let* ((workflow-name (string-upcase (format nil "~a-WORKFLOW" (temporal.common:name (temporal.workflowservice:workflow-type task)))))
         (class (find-symbol workflow-name :cl-temporal)))
    (make-instance class :task task)))
