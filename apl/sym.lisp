;;; sym.lisp --- APL Symbols

;; 

;;; Code:
(in-package :apl)

(defmacro ⍝ (&rest args)
  `(make-instance 'comment :comment ,(format nil "~{~a~^ ~}" args) :chars "⍝"))
