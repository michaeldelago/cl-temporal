(in-package :cl-temporal.activity)

(defparameter *default-options*
  (list))

(defstruct (activity-poller (:constructor make-activity-poller (namespace task-queue activity-lookup)))
  namespace
  task-queue
  activity-lookup
  (shutting-down nil)
  (polling nil)
  (config (list))
  (middleware (list))
  (options *default-options*)
  (thread-handle nil))

(defun start-polling (poller)
  (with-slots (shutting-down polling thread-handle)
      poller
    (setf shutting-down nil)
    (setf polling t)
    (setf thread-handle (bt2:make-thread (lambda () (poll-loop poller))))))

(defun poll-loop (poller)
  (with-slots (namespace task-queue)
      poller
    (log:debug "activity polling start" namespace task-queue)
    (loop named task-poller
          while (null (activity-poller-shutting-down poller))
          with task = (poll-activity-task-queue namespace task-queue)
          do (progn (when (activity-type task)
                      (log:info "found task" namespace task-queue task))
                    (sleep 0.5)))))
