;;; Copyright 2013 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:cl-protobufs.test.case-preservation
  (:use #:cl
        #:clunit)
  (:local-nicknames (#:pb #:cl-protobufs.protobuf-case-preservation-unittest)
                    (#:pi #:cl-protobufs.implementation)
                    (#:proto #:cl-protobufs))
  (:export :run))

(in-package #:cl-protobufs.test.case-preservation)


(defsuite case-preservation-suite (cl-protobufs.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'case-preservation-suite :use-debugger use-debugger
                                             :signal-condition-on-fail t))


(deftest case-preservation-test (case-preservation-suite)
  (let ((service (proto:find-service-descriptor 'pb:quux-service)))
    (assert-true service)
    ;; We're reaching into the implementation to verify the objects have
    ;; been properly constructed.
    (let ((method (proto:find-method-descriptor service "QUUXMethod")))
      (assert-true method)
      (assert-equal
          (pi::proto-input-name method)
          "protobuf_case_preservation_unittest.QUUXRequest")
      (assert-equal
          (pi::proto-output-name method)
          "protobuf_case_preservation_unittest.QUUXResponse"))))
