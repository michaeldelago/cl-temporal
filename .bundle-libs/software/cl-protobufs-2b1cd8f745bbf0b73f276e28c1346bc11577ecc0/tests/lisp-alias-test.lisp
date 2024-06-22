;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.alias
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.alias-test)
                    (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.alias)


(defsuite alias-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'alias-suite :use-debugger use-debugger
                                 :signal-condition-on-fail t))

(defstruct aliased-struct i)

(defconstant +TAG-I+ (pi::make-tag 'proto:int32 1)
  "The tag that should be used for Message.I and AliasedMessage.I")

;; Serialization of cl-protobufs-generated class (as opposed to using a lisp_alias FieldOption)

(defun expect-bytes (list array)
  (assert-equal (coerce list 'list) (coerce array 'list)))

(deftest serialize-regular (alias-suite)
  (let ((obj (pb:make-message :i 99)))
    (expect-bytes (list +TAG-I+ 99)
                  (proto:serialize-to-bytes obj))))

;; Serialization of the aliased (explicit) message
(defun internal-serialize-message (msg buf)
  "Serialization function for message.
   MSG: The message being serialized.
   BUF: The buffer to serialize to."
  (let ((i (aliased-struct-i msg))
        (size 0))
    (incf size (pi::serialize-scalar i 'proto:int32  +TAG-I+ buf))))

#+sbcl
(defun (:protobuf :serialize pb:aliased-message)
    (val buf)
  (internal-serialize-message val buf))

#-sbcl
(setf (get 'pb:aliased-message :serialize)
      (lambda (val buf)
        (internal-serialize-message val buf)))

(deftest serialize-aliased (alias-suite)
  (let ((struct (make-aliased-struct :i 99)))
    (expect-bytes (list +TAG-I+ 99)
                  (proto:serialize-to-bytes struct 'pb:aliased-message))))

;;  Serialization of OuterMessage

(deftest serialize-empty-outer (alias-suite)
  (let ((outer (pb:make-outer-message)))
    (expect-bytes nil (proto:serialize-to-bytes outer))))

(defconstant +TAG-MESSAGE+ (pi::make-tag 'string 1)
  "The tag that should be used for field OuterMessage.Message")

(defconstant +TAG-ALIASED+ (pi::make-tag 'string 2)
  "The tag that should be used for field OuterMessage.Aliased")

(deftest serialize-outer-containing-regular (alias-suite)
  (let ((outer (pb:make-outer-message
                :message (pb:make-message :i 99))))
    (expect-bytes (list +TAG-MESSAGE+ 2 +TAG-I+ 99)
                  (proto:serialize-to-bytes outer))))

(deftest serialize-outer-containing-aliased (alias-suite)
  (let ((outer (pb:make-outer-message
                :aliased (make-aliased-struct :i 99))))
    (expect-bytes (list +TAG-ALIASED+ 2 +TAG-I+ 99)
                  (proto:serialize-to-bytes outer))))

;; cl-protobufs message metadata

(deftest find-message-for-alias (alias-suite)
  (assert-true (proto:find-message-descriptor 'my.dog.has.fleas::aliased-struct))
  ;; Known bug with ABCL
  ;; https://github.com/armedbear/abcl/issues/287
  #-abcl
  (assert-eq (proto:find-message-descriptor 'pb:aliased-message)
             (proto:find-message-descriptor 'my.dog.has.fleas::aliased-struct)))
