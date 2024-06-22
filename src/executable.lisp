(in-package :cl-temporal)

(serapeum.exporting:defclass executable nil nil)

(serapeum.exporting:defgeneric execute (executable)
  (:documentation "Execute this thing")
  (:method (executable)
    (error "method not implemented")))
