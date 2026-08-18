;;; FORMAT
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun directive-handler-name (char suffix)
    (sb-int:package-symbolicate "SB-FORMAT"
                                (if (char= char #\Newline) "NL" (string char))
                                suffix)))

(defmacro def-complex-format-directive (char lambda-list &body body)
  ;; Assert that it isn't lowercase
  (let ((code (char-code char)))
    (when (<= (char-code #\a) code (char-code #\z))
      (error "Come on, use uppercase why don't you?")))
  (let ((defun-name (directive-handler-name char "-COMPILER"))
        (directive (gensym "DIRECTIVE"))
        (directives (if lambda-list (car (last lambda-list)) (gensym "DIRECTIVES"))))
    `(progn
       (defun ,defun-name (,directive ,directives)
         ,@(if lambda-list
               `((let ,(mapcar (lambda (var)
                                 `(,var
                                   (,(std/sym:symbolicate "DIRECTIVE-" var) ,directive)))
                               (butlast lambda-list))
                   ,@body))
               `((declare (ignore ,directive ,directives))
                 ,@body)))
       (setf (aref *format-directive-expanders* ,(char-code char))
             #',defun-name)
       ',defun-name)))

(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (gensym "DIRECTIVES"))
        (declarations nil)
        (body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
        (unless (and (consp form) (eq (car form) 'declare))
          (return))
        (push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
               ,directives))))

(defmacro expand-bind-defaults (specs params &body body)
  (std/macs:once-only ((params params))
    (if specs
        (multiple-value-bind (exp runt)
            (std/macs:with-collectors (expander-bindings runtime-bindings)
              (dolist (spec specs)
                (destructuring-bind (var default) spec
                  (expander-bindings `(,var ',var))
                  (runtime-bindings
                   `(list ',var
                          (let* ((param-and-offset (pop ,params))
                                 (offset (car param-and-offset))
                                 (param (cdr param-and-offset)))
                            (case param
                              (:arg `(or ,(expand-next-arg offset) ,,default))
                              (:remaining
                               (setf *only-simple-args* nil)
                               '(length args))
                              ((nil) ,default)
                              (t param))))))))
          `(let ,exp
            `(let ,(list ,@runt)
               #+nil
              ,@(if ,params
                    (sb-format::format-error-at
                     nil (caar ,params)
                     "Too many parameters, expected no more than ~W"
                     ,(length specs)))
              ,,@body)))
        `(progn
           #+nil
           (when ,params
             (sb-format::format-error-at nil (caar ,params)
                              "Too many parameters, expected none"))
           ,@body))))

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (expand-bind-defaults
          ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
          params
        `(sb-format::format-print-integer stream ,(sb-format::expand-next-arg) ,colonp ,atsignp
                                          ,base ,mincol ,padchar ,commachar
                                          ,commainterval))
      `(sb-format::format-integer ,(sb-format::expand-next-arg) ,base stream)))
