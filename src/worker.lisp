(in-package :cl-temporal)

(defclass worker ()
  ((namespace
    :initarg :namespace
    :initform "default"
    :type string
    :accessor namespace)
   (task-queue
    :initarg :task-queue
    :type string
    :accessor task-queue)
   (worker-id
    :initarg :worker-id
    :initform "foobar"
    :type string
    :accessor worker-id)
   (workflows
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor workflows)
   (activities
    :initform (make-hash-table :test #'equal :synchronized t)
    :accessor activities)
   (app-data
    :initform (list)
    :accessor app-data)
   (pollers
    :initform (list)
    :accessor pollers)
   (shutting-down
    :initform nil
    :accessor shutting-down)))

(defmacro with-worker ((name &optional (namespace "default") (worker-id "abcd") (task-queue "foobar")) &body body)
  `(let ((,name (make-instance 'worker :namespace ,namespace :worker-id ,worker-id :task-queue ,task-queue)))
     ,@body))

(defmethod register-activity ((worker worker) activity-name)
  (let ((activity-class (gethash activity-name cl-temporal.activity:*created-activities*)))
    (setf (gethash activity-name (activities worker)) activity-class)))

(defmethod register-workflow ((worker worker) workflow-name)
  (let ((wf-class (gethash workflow-name cl-temporal.workflow:*created-workflows*)))
    (setf (gethash workflow-name (workflows worker)) wf-class)))

(defmethod complete-workflow-activation ((worker worker) completion)
  "TODO"
  (with-slots (workflows)
      worker
    (let* ((run-id (alexandria:assoc-value completion :run-id))
           (oneshot (serapeum:box nil))))))


(defmethod workflow-activation-handler ((worker worker) activation completions-channel)
  "TODO"
  nil)

(defmethod activity-task-handler ((worker worker) activity)
  "TODO"
  nil)

(defmethod poll-workflow-activation ((worker worker))
  "TODO"
  nil)

(defmethod poll-activity-task ((worker worker))
  "TODO"
  nil)

;; Mostly translated incomplete implementation from the Rust sdk. May be useful for later code
;;
;; (defmethod run ((worker worker))
;;   ;; 2 channels: one for sending workflow futures, and one for sending their results (completions)
;;   ;; The type for workflow-future channel should generally be a pair, (cons run-id workflow-fut)
;;   ;; The type for completions is undefined at the moment
;;   (let ((workflow-future (make-instance 'calispel:channel))
;;         (completions (make-instance 'calispel:channel)))
;;     (labels
;;         ;; The workflow future joiner fn pops futures off of the workflow-future channel and executes them
;;         ((workflow-future-joiner ()
;;            (destructuring-bind (run-id fut)
;;                (calispel:? workflow-future)
;;              (eager-future2:yield fut)
;;              (log:info "removing workflow from cache" :run-id run-id)
;;              (remhash run-id (workflows worker)))
;;            (workflow-future-joiner))
;;          ;; The workflow completion processor handles behavior when workflows complete
;;          (workflow-completion-processor ()
;;            (let ((completion (calispel:? completions)))
;;              (alexandria:if-let ((interceptor (interceptor worker)))
;;                (eager-future2:yield
;;                 (on-workflow-activation-completion interceptor completion)))
;;              (eager-future2:yield
;;               (complete-workflow-activation worker completion)))
;;            (workflow-completion-processor))
;;          ;; The workflow poller loop polls temporal for running workflows as required and sends them to the workflow future joiner
;;          (workflow-poller-loop ()
;;            (let ((activation (poll-workflow-activation worker)))
;;              (alexandria:if-let ((interceptor (interceptor worker)))
;;                (eager-future2:yield (on-workflow-activation interceptor worker)))
;;              (alexandria:if-let ((workflow-fut (workflow-activation-handler worker activation completions)))
;;                (calispel:! workflow-future workflow-fut))))
;;          ;; The activity poller loop polls temporal for running activities as required
;;          (activity-poller-loop ()
;;            (unless (alexandria:emptyp (activities worker))
;;              (loop
;;                do (let ((activity (eager-future2:yield (poll-activity-task worker))))
;;                     (activity-task-handler worker activity))))))
;;       (eager-future2:pand
;;        (eager-future2:pcall #'workflow-future-joiner :future-type :eager)
;;        (eager-future2:pcall #'workflow-completion-processor :future-type :eager)
;;        (eager-future2:pcall #'worker-poller-loop :future-type :eager)
;;        (eager-future2:pcall #'activity-poller-loop :future-type :eager)))))


;; (defmethod register-activity ((worker worker) activity)
;;   nil)

;; (defmethod register-workflow ((worker worker) workflow)
;;   nil)

(defmethod start ((worker worker))
  (serapeum:do-hash-table (name workflow (workflows worker))
    (push (cl-temporal.workflow::make-workflow-poller (namespace worker) (task-queue worker) nil) (pollers worker)))
  (serapeum:do-hash-table (name activity (activities worker))
    (with-slots (task-queue lookup)
        activity
      (push (cl-temporal.activity::make-activity-poller (namespace worker) (task-queue worker) nil) (pollers worker))))
  (dolist (poller (pollers worker))
    (ecase (type-of poller)
      ('cl-temporal.workflow::workflow-poller (cl-temporal.workflow::start-polling poller))
      ('cl-temporal.activity::activity-poller (cl-temporal.activity::start-polling poller))))
  (log:info "starting worker" (pollers worker))
  ;; TODO (michaeldelago) add on worker start hook
  (loop while (null (shutting-down worker))))
