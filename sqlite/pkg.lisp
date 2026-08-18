(defpkg :sqlite
  (:use std-lisp sb-alien)
  (:export :load-sqlite :+sqlite-version-number+))
(in-package :sqlite)
(define-alien-loader :sqlite "/usr/lib/" "sqlite3")
