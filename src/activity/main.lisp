(in-package :cl-temporal.activity)

(serapeum.exporting:defparameter *created-activities* (serapeum:dict))

(serapeum.exporting:defclass activity (cl-temporal:executable)
  ((name
    :initarg :name
    :accessor name)
   (task-queue
    :initarg :task-queue
    :accessor task-queue)
   (internal
    :initform nil
    :accessor internal)))

(serapeum:eval-always
  (serapeum.exporting:defparameter created-activities (serapeum:dict)))

(serapeum.exporting:defmacro defactivity (name (&rest args) &body body)
  "Create a activity with name NAME and create an execute method with ARGS and BODY.
Note that this only defines a activity and does not register it to a worker.

(defactivity hello (name)
  (format nil \"Hello, ~A~&\" name))"
  (let ((wf-name (alexandria:symbolicate name '- 'activity)))
    (when (gethash wf-name created-activities)
      (warn 'activity-already-defined :message "workflow is already defined" :name name))
    (setf (gethash name created-activities) wf-name)
    `(progn
       (defclass ,wf-name (activity) nil)
       (defmethod execute ((wf ,wf-name) ,@args)
         ,@body))))
