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

(defvar *task-queue-lock* (make-lock "Eager Future2 thread pool lock"))
(defvar *leader-notifier* (make-condition-variable :name "Eager Future2 leader notifier"))
(defvar *task-queue* ())
(defvar *free-threads* 0)

(defvar *thread-counter-lock* (make-recursive-lock "Eager Future2 thread pool total thread counter lock"))
(defvar *total-threads* 0)

(defun make-pool-thread ()
  (make-thread
   (lambda ()
     (unwind-protect
          (catch 'die
            (let ((*debugger-hook* (lambda (c old-hook)
                                     (declare (ignore c old-hook))
                                     (throw 'continue nil))))
              (loop (catch 'continue
                      (funcall (with-lock-held (*task-queue-lock*)
                                 (incf *free-threads*)
                                 (unwind-protect
                                      (loop (if *task-queue*
                                                (return (pop *task-queue*))
                                                (condition-wait *leader-notifier* *task-queue-lock*)))
                                   (decf *free-threads*))))))))
       (with-recursive-lock-held (*thread-counter-lock*) (decf *total-threads*))))
   :name "Eager Future2 Worker")
  (with-recursive-lock-held (*thread-counter-lock*) (incf *total-threads*)))

(defun thread-pool-size ()
  "Returns the current number of threads in the thread pool. This
number determines the maximum amount of speculative futures that can
be computed at the same time."
  (with-recursive-lock-held (*thread-counter-lock*)
    *total-threads*))

(defun advise-thread-pool-size (new-size)
  "Attempts to set the amount of threads in the thread pool to given value."
  (with-recursive-lock-held (*thread-counter-lock*)
    (if (< *total-threads* new-size)
        (loop repeat (- new-size *total-threads*) do (make-pool-thread))
        (with-lock-held (*task-queue-lock*)
          (loop repeat (- *total-threads* new-size) do
               (push (lambda () (throw 'die nil)) *task-queue*)
               (condition-notify *leader-notifier*))))))

(eval-when (:load-toplevel)
  (advise-thread-pool-size 10))

(defun schedule-last (task)
  (with-lock-held (*task-queue-lock*)
    (setf *task-queue* (append *task-queue* (list task)))
    (when (< 0 *free-threads*)
      (condition-notify *leader-notifier*)))
  (values))

(defun schedule-immediate (task)
  (unless (with-lock-held (*task-queue-lock*)
            (when (< 0 *free-threads*)
              (setf *task-queue* (push task *task-queue*))
              (condition-notify *leader-notifier*)
              t))
    (make-thread task :name "Eager Future2 Temporary Worker"))
  (values))
