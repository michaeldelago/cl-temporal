(defpackage helloworld.worker
  (:use :cl))
(in-package :helloworld.worker)

(defvar *hostname* "localhost")
(defvar *port* 7233)
(defvar *channel* nil)

(defun main ()
  (grpc:with-channel (channel
                      :hostname *hostname*
                      :port *port)
    (cl-temporal:with-worker (worker channel)
      (cl-temporal:add-workflow worker "hello-world"
                                    (lambda (name)
                                      (log:info "HelloWorld started" :name name)
                                      (let (result)
                                        (handler-case
                                            (setq result (cl-temporal:call-activity worker "hello-world" (list name)))
                                          (error (c)
                                            (log:error "caught error!"))))))
      (cl-temporal:add-activity worker "hello-world"
                                    (lambda (name)
                                      (log:info "Activity" :name name)
                                      (format nil "Hello ~S!" name)))

      (cl-temporal:run worker))))
