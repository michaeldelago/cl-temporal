;; Copyright 2010, 2011 Vladimir Sedach <vas@oneofus.la>

;; This file is part of Eager Future2.

;; SPDX-License-Identifier: LGPL-3.0-or-later

;; Eager Future2 is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Eager Future2 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Eager Future2. If not, see
;; <https://www.gnu.org/licenses/>.

(in-package #:eager-future2)

(defvar *default-future-type* :speculative
  "One of :eager, :speculative (default) or :lazy.
If eager, any newly created futures start their computation immediately.
If speculative, newly created futures are computed when thread pool threads are available, in FIFO future creation order.
If lazy, newly created futures are not computed until asked to yield their values.")

(defvar *computing-future* nil
  "Part of scheduling protocol for thread-pooled futures.")

(defun abort-scheduled-future-task (thread future-id)
  (when (thread-alive-p thread)
    (ignore-errors ;; should probably log them or something
      (interrupt-thread thread (lambda ()
                                 (when (eql *computing-future* future-id)
                                   (throw 'task-done nil)))))))

(defun make-scheduler-task (future-ptr)
  (lambda ()
    (catch 'task-done
      (flet ((get-future () (or (weak-pointer-value future-ptr) (throw 'task-done nil))))
        (let ((*computing-future* (future-id (get-future))))
          (with-lock-held ((lock (get-future)))
            (if (or (%ready-to-yield? (get-future)) (computing-thread (get-future)))
                (throw 'task-done nil)
                (setf (computing-thread (get-future)) (current-thread))))
          (finalize (get-future) (let ((thread (current-thread))
                                       (future-id *computing-future*))
                                   (lambda () (abort-scheduled-future-task thread future-id))))
          (let ((values
                 (let ((*debugger-hook*
                        (lambda (c old-hook)
                          (declare (ignore old-hook))
                          (with-lock-held ((lock (get-future)))
                            (setf (restart-notifier (get-future)) (make-condition-variable :name "Eager Future2 restart proxy CV")
                                  (error-descriptor (get-future)) (cons c (compute-restarts c)))
                            (loop (let ((wait-list (wait-list (get-future))))
                                    (when wait-list
                                      (let ((random-waiter (elt wait-list (random (length wait-list)))))
                                        (with-lock-held ((car random-waiter))
                                          (condition-notify (cdr random-waiter))))))
                               (condition-wait (restart-notifier (get-future)) (lock (get-future)))
                               (let ((proxy-restart (proxy-restart (get-future))))
                                 (when proxy-restart
                                   (apply #'invoke-restart (car proxy-restart) (cdr proxy-restart)))))))))
                   (restart-case (multiple-value-list (funcall (task (get-future))))
                     (force-values (&rest values) values)))))
            (apply #'force (get-future) values)))))))

(defun schedule-future (future future-type)
  (let ((scheduler-task (make-scheduler-task (make-weak-pointer future))))
    (ccase future-type
      (:eager (schedule-immediate scheduler-task))
      (:speculative (schedule-last scheduler-task)))))

(defun pcall (thunk &optional (future-type *default-future-type*))
  "Given a function of no arguments, returns an object (called a
future) that can later be used to retrieve the values computed by the
function.

future-type (by default the value of *default-future-type*) can either
be :eager, :speculative, or :lazy. See the documentation of
*default-future-type* for an explanation of the different future
types.

The function is called in an unspecified dynamic environment."
  (let ((future (make-future thunk (gensym "FUTURE-ID"))))
    (unless (eq future-type :lazy)
      (schedule-future future future-type))
    future))
