(in-package :cl-temporal)

(defvar *hostname* "localhost")
(defvar *port* 7233)
(defvar *channel*)

(defun main ()
  (grpc:init-grpc)
  (unwind-protect
       (with-connection (:host *hostname* :port *port* :insecure t :namespace "default" :task-queue "hello-world")
         (let* ((message (temporal.workflowservice:make-list-namespaces-request))
                (response (temporal.workflowservice-rpc:call-list-namespaces *channel* message)))
           response))))

;; (main)

;; (cl-temporal.activity:defactivity hello ()
;;   (format nil "Hello ~A" name))

(cl-temporal.workflow:defworkflow hello-world ()
  (log:info "hello workflow"))

(defun test ()
  (sb-thread:release-foreground)
  (with-connection (:host *hostname*
                    :port *port*
                    :insecure t
                    :namespace "cl-dev"
                    :task-queue "foo-task")
    (let ((worker (make-instance 'worker :task-queue "foo-task" :namespace "cl-dev")))
      (register-workflow worker 'hello-world)
      ;; (register-activity worker 'Hello)
      (start worker))))
