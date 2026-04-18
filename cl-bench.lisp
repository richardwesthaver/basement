;;; cl-bench.lisp --- Common Lisp Implementation Benchmarks

;; These functions can be used on any conforming Common Lisp
;; implementation to measure and compare performance characteristics.

;;; Commentary:

;; - http://www.ulisp.com/show?1EO1

;;; Code:
(defpackage :cl-bench
  (:use :cl)
  (:export :tak :fib :q :q2 :factor :factorize :crc-32))

(in-package :cl-bench)
;;; Takeuchi
;; (time (tak 18 12 6))
(defun tak (x y z)
  (if (not (< y x))
      z
    (tak
     (tak (1- x) y z)
     (tak (1- y) z x)
     (tak (1- z) x y))))

;;; Fibonacci
;; (time (fib 23))
(defun fib (n)
  (if (< n 3) 1
    (+ (fib (- n 1)) (fib (- n 2)))))

;;; Hofstadter Q
;; (time (q 21))
(defun q (n)
  (if (<= n 2) 1
    (+
     (q (- n (q (- n 1))))
     (q (- n (q (- n 2)))))))

;;; 2D recursive Q
;; http://www.lispology.com/show?314T
;; (time (q2 7 8))
(defun q2 (x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (1- x) y)) y)
       (q2 x (- y (q2 x (1- y)))))))

;;; Factor
;; (time (factor 2142142141))
(defun factor (n)
  (let ((d 2) (i 1))
    (loop
     (when (> (* d d) n) (return n))
     (when (zerop (mod n d)) (return d))
     (incf d i) (setq i 2))))

;;; Factorize
;; (time (factorize 731731731))
(defun factorize (n)
  (let ((f (factor n)))
    (if (= n f) (list n) (cons f (factorize (/ n f))))))

;;; Ackermann
;; The Ackermann function is the simplest example of a well-defined total
;; function which is computable but not primitive recursive, providing a
;; counterexample to the belief in the early 1900s that every computable
;; function was also primitive recursive (Dtzel 1991). It grows faster
;; than an exponential function, or even a multiple exponential function. 
(defun ackermann (m n)
  (declare (type integer m n))
  (cond
    ((zerop m) (1+ n))
    ((zerop n) (ackermann (1- m) 1))
    (t (ackermann (1- m) (ackermann m (1- n))))))

;;; CRC-32
;; (time (crc32 "The quick brown fox jumps over the lazy dog"))
(defun crc32 (str)
  (let ((crc #xFFFFFFFF))
    (dotimes (k (length str))
      (let* ((c (char str k))
             (n (char-code c)))
        (dotimes (i 8)
          (setq crc 
                (if (oddp (logxor n crc))
                    (logxor (logand (ash crc -1) #x7FFFFFFF) #xEDB88320)
                  (logand (ash crc -1) #x7FFFFFFF)))
          (setq n (ash n -1)))))
    (logxor crc #xFFFFFFFF)))

;;; RNG
(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +m1+   4294967087d0)
  (defconstant +m2+   4294944443d0)
  (defconstant +a12+     1403580d0)
  (defconstant +a13n+     810728d0)
  (defconstant +a21+      527612d0)
  (defconstant +a23n+    1370589d0)
  (defconstant +norm+ (/ (1+ +m1+))))

(declaim (inline mrg32k3a-comp-1 mrg32k3a-comp-2))

(defun mrg32k3a-comp-1 (state)
  (declare (type (simple-array double-float (6)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((s10 (aref state 0))
	(s11 (aref state 1)))
    (declare (type (double-float 0d0 4294967086d0) s10 s11))
    (let* ((p1 (- (* s11 +a12+) (* s10 +a13n+)))
	   (k (ftruncate (/ p1 +m1+)))
	   (p1b (- p1 (* k +m1+)))
	   (p1c (if (< p1b 0)
		    (+ p1b +m1+)
		    p1b)))
      (shiftf (aref state 0)
	      (aref state 1)
	      (aref state 2)
	      p1c)
      p1c)))

(defun mrg32k3a-comp-2 (state)
  (declare (type (simple-array double-float (6)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((s20 (aref state 3))
	(s22 (aref state 5)))
    (declare (type (double-float 0d0 4294944442d0) s20 s22))
    (let* ((p2 (- (* s22 +a21+) (* s20 +a23n+)))
	   (k (ftruncate (/ p2 +m2+)))
	   (p2b (- p2 (* k +m2+)))
	   (p2c (if (< p2b 0)
		    (+ p2b +m2+)
		    p2b)))
      (shiftf (aref state 3)
	      (aref state 4)
	      (aref state 5)
	      p2c)
      p2c)))

(declaim (inline mrg32k3a))
(defun mrg32k3a (state)
  (declare (type (simple-array double-float (6)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((p1 (mrg32k3a-comp-1 state))
	(p2 (mrg32k3a-comp-2 state)))
    (if (<= p1 p2)
	(* (+ (- p1 p2) +m1+) +norm+)
	(* (- p1 p2) +norm+))))

;;; Mandelbrot
;; calculate the "level" of a point in the Mandebrot Set, which is the
;; number of iterations taken to escape to "infinity" (points that
;; don't escape are included in the Mandelbrot Set). This version is
;; intended to test performance when programming in naïve math-style. 
(defun mset-level/complex (c)
  (declare (type complex c))
  (loop :for z = #c(0 0) :then (+ (* z z) c)
        :for iter :from 1 :to 300
        :until (> (abs z) 4.0)
        :finally (return iter)))

;; this version is intended to test lower-level performance-oriented
;; coding of the same function; hence the extra declarations and the
;; decoding of the operations on complex numbers.
(defun mset-level/dfloat (c1 c2)
  (declare (type double-float c1 c2))
  (let ((z1 0.0d0)
        (z2 0.0d0)
        (aux 0.0d0))
    (declare (double-float z1 z2 aux))
    (do ((iter 0 (1+ iter)))
        ((or (> (abs (+ (* z1 z1) (* z2 z2))) 4.0)
             (> iter 300))
         iter)
      (setq aux z1
            z1 (+ (* z1 z1) (- (* z2 z2)) c1)
            z2 (+ (* 2.0d0 z2 aux) c2)))))

;;; Arrays
(defun bench-1d-arrays (&optional (size 100000) (runs 10))
  (declare (fixnum size))
  (let ((ones (make-array size :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array size :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array size :element-type '(integer 0 2000))))
    (dotimes (runs runs)
      (dotimes (pos size)
        (setf (aref threes pos) (+ (aref ones pos) (aref twos pos))))
      (assert (null (search (list 4 5 6) threes)))))
  (values))

(defun bench-2d-arrays (&optional (size 2000) (runs 10))
  (declare (fixnum size))
  (let ((ones (make-array (list size size) :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array (list size size) :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array (list size size) :element-type '(integer 0 2000))))
    (dotimes (runs runs)
      (dotimes (i size)
        (dotimes (j size)
          (setf (aref threes i j)
                (+ (aref ones i j) (aref twos i j)))))
      (assert (eql 3 (aref threes 3 3)))))
  (values))

(defun bench-3d-arrays (&optional (size 200) (runs 10))
  (declare (fixnum size))
  (let ((ones (make-array (list size size size) :element-type '(integer 0 1000) :initial-element 1))
        (twos (make-array (list size size size) :element-type '(integer 0 1000) :initial-element 2))
        (threes (make-array (list size size size) :element-type '(integer 0 2000))))
    (dotimes (runs runs)
      (dotimes (i size)
        (dotimes (j size)
          (dotimes (k size)
            (setf (aref threes i j k)
                  (+ (aref ones i j k) (aref twos i j k))))))
      (assert (eql 3 (aref threes 3 3 3)))))
  (values))

(defun bench-bitvectors (&optional (size 1000000) (runs 700))
  (declare (fixnum size))
  (let ((zeros (make-array size :element-type 'bit :initial-element 0))
        (ones  (make-array size :element-type 'bit :initial-element 1))
        (xors  (make-array size :element-type 'bit)))
    (dotimes (runs runs)
      (bit-xor zeros ones xors)
      (bit-nand zeros ones xors)
      (bit-and zeros xors)))
  (values))

(defun bench-strings (&optional (size 1000000) (runs 50))
  (declare (fixnum size))
  (let ((zzz (make-string size :initial-element #\z))
        (xxx (make-string size)))
    (dotimes (runs runs)
      (and (fill xxx #\x)
           (replace xxx zzz)
           (search "xxxd" xxx)
           (nstring-upcase xxx))))
  (values))

(defun bench-strings/adjustable (&optional (size 1000000) (runs 100))
  (declare (fixnum size))
  (dotimes (runs runs)
    (let ((sink (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
      (dotimes (i size)
        (vector-push-extend (code-char (mod i 128)) sink))))
  (values))

;; certain implementations such as OpenMCL have an array (and thus
;; string) length limit of (expt 2 24), so don't try this on humungous
;; sizes
(defun bench-string-concat (&optional (size 1000000) (runs 100))
  (declare (fixnum size))
  (dotimes (runs runs)
    (let ((len (length
                (with-output-to-string (string)
                  (dotimes (i size)
                    (write-sequence "hi there!" string))))))
      (assert (eql len (* size (length "hi there!")))))
    (values)))

(defun bench-search-sequence (&optional (size 1000000) (runs 10))
  (declare (fixnum size))
  (let ((haystack (make-array size :element-type '(integer 0 1000))))
    (dotimes (runs runs)
      (dotimes (i size)
        (setf (aref haystack i) (mod i 1000)))
      (assert (null (find -1 haystack :test #'=)))
      (assert (null (find-if #'minusp haystack)))
      (assert (null (position -1 haystack :test #'= :from-end t)))
      (loop :for i :from 20 :to 900 :by 20
            :do (assert (eql i (position i haystack :test #'=))))
      (assert (eql 0 (search #(0 1 2 3 4) haystack :end2 1000 :from-end t)))))
  (values))

;;; Bignum
(defvar *x1*)
(defvar *x2*)
(defvar *x3*)
(defvar *y*)
(defvar *z*)


;; this can be 1e-6 on most compilers, but for COMPUTE-PI-DECIMAL on
;; OpenMCL we lose lotsa precision
(defun fuzzy-eql (a b)
  (< (abs (/ (- a b) b)) 1e-4))


(defun elementary-benchmark (N repeat)
  (setq *x1* (floor (+ (isqrt (* 5 (expt 10 (* 4 N)))) (expt 10 (* 2 N))) 2))
  (setq *x2* (isqrt (* 3 (expt 10 (* 2 N)))))
  (setq *x3* (+ (expt 10 N) 1))
  ;; (format t "~&~%N = ~D, Multiplication *x1* * *x2*, divide times by ~D~%" N repeat)
  (dotimes (count 3)
    (dotimes (_ repeat)
      (setq *y* (* *x1* *x2*))))
  ;; (format t "~&~%N = ~D, Division (with remainder) *x1* / *x2*, divide times by ~D~%" N repeat)
  (dotimes (count 3)
    (dotimes (_ repeat)
      (multiple-value-setq (*y* *z*) (floor *x1* *x2*))))
  ;; (format t "~&~%N = ~D, integer_sqrt(*x3*), divide times by ~D~%" N repeat)
  (dotimes (count 3)
    (dotimes (_ repeat)
      (setq *y* (isqrt *x3*))))
  ;; (format t "~&~%N = ~D, gcd(*x1*,*x2*), divide times by ~D~%" N repeat)
  (dotimes (count 3)
    (dotimes (_ repeat)
      (setq *y* (gcd *x1* *x2*)))))

(defun run-elem-100-1000 ()
  (elementary-benchmark 100 1000))

(defun run-elem-1000-100 ()
  (elementary-benchmark 1000 100))

(defun run-elem-10000-1 ()
  (elementary-benchmark 10000 1))


(defun pari-benchmark (N repeat)
  (dotimes (count 3)
    (dotimes (_ repeat)
      (let ((u 1) (v 1) (p 1) (q 1))
        (do ((k 1 (1+ k)))
            ((> k N) (setq *y* p *z* q))
          (let ((w (+ u v)))
            (shiftf u v w)
            (setq p (* p w))
            (setq q (lcm q w))))))))

(defun run-pari-100-10 ()
  (pari-benchmark 100 10))

(defun run-pari-200-5 ()
  (pari-benchmark 200 5))

(defun run-pari-1000-1 ()
  (pari-benchmark 1000 1))

;; calculating pi using ratios
(defun compute-pi-decimal (n)
  (let ((p 0)
        (r nil)
        (dpi 0))
    (dotimes (i n)
      (incf p (/ (- (/ 4 (+ 1 (* 8 i)))
                    (/ 2 (+ 4 (* 8 i)))
                    (/ 1 (+ 5 (* 8 i)))
                    (/ 1 (+ 6 (* 8 i))))
                 (expt 16 i))))
    (dotimes (i n)
      (multiple-value-setq (r p) (truncate p 10))
      (setf dpi (+ (* 10 dpi) r))
      (setf p (* p 10)))
    dpi))

(defun run-pi-decimal/small ()
  (assert (fuzzy-eql pi (/ (compute-pi-decimal 200) (expt 10 198)))))

(defun run-pi-decimal/big ()
  (assert (fuzzy-eql pi (/ (compute-pi-decimal 1000) (expt 10 998)))))


(defun pi-atan (k n)
  (do* ((a 0) (w (* n k)) (k2 (* k k)) (i -1))
       ((= w 0) a)
    (setq w (truncate w k2))
    (incf i 2)
    (incf a (truncate w i))
    (setq w (truncate w k2))
    (incf i 2)
    (decf a (truncate w i))))

(defun calc-pi-atan (digits)
  (let* ((n digits)
         (m (+ n 3))
         (tenpower (expt 10 m)))
    (values (truncate (- (+ (pi-atan 18 (* tenpower 48))
                            (pi-atan 57 (* tenpower 32)))
                         (pi-atan 239 (* tenpower 20)))
                      1000))))

(defun run-pi-atan ()
  (let ((api (calc-pi-atan 1000)))
    (assert (fuzzy-eql pi (/ api (expt 10 1000))))))

;;; CLOS
;;;; Janderson
;;                         class-0-0
;;                      /     |        \
;;                    /       |          \
;;                  /         |            \
;;          class-0-1      class-1-1     . class-2-1
;;             |         /    |     .  .   /    |
;;             |     /      . |  .       /      |
;;             |  /     .     |        /        |
;;          class-0-2      class-1-2       class-2-2

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +hierarchy-depth+ 10)
  (defconstant +hierarchy-width+ 5)
  ;; the level-0 hierarchy
  (defclass class-0-0 () ()))

(defvar *instances* (make-array +hierarchy-width+ :element-type 'class-0-0))

(when (fboundp 'simple-method) (fmakunbound 'simple-method))
(when (fboundp 'complex-method) (fmakunbound 'complex-method))
 
(defgeneric simple-method (a b))

(defmethod simple-method ((self class-0-0) other) other)

#-(or poplog)
(defgeneric complex-method (a b &rest rest)
  (:method-combination and))

#-(or poplog)
(defmethod complex-method and ((self class-0-0) other &rest rest)
   (declare (ignore rest))
   other)

(defmacro make-class-name (depth width)
  (with-standard-io-syntax
    `(intern (format nil "CLASS-~d-~d" ,depth ,width))))

(defmacro make-attribute-name (depth width)
  (with-standard-io-syntax
    `(intern (format nil "ATTRIBUTE-~d-~d" ,depth ,width))))

(defmacro make-initarg-name (depth width)
  (with-standard-io-syntax
    `(intern (format nil "INITARG-~d-~d" ,depth ,width) :keyword)))

(defmacro make-accessor-name (depth width)
  (with-standard-io-syntax
    `(intern (format nil "GET-ATTRIBUTE-~d-~d" ,depth ,width))))

(defmacro class-definition (depth width)
  `(defclass ,(make-class-name depth width)
    ,(loop :for w :from width :below +hierarchy-width+
           :collect (make-class-name (1- depth) w))
    (( ,(make-attribute-name depth width)
      :initarg ,(make-initarg-name depth width)
      :initform (* ,depth ,width)
      :accessor ,(make-accessor-name depth width)))))

(defmacro init-instance-definition (depth width)
  `(defmethod initialize-instance :after ((self ,(make-class-name depth width)) &rest initargs)
    (declare (ignore initargs))
    (incf (,(make-accessor-name depth width) self))))

(defmacro simple-method-definition (depth width)
  `(defmethod simple-method ((self ,(make-class-name depth width))
                      (n number))
    (* n (call-next-method) (,(make-accessor-name depth width) self))))

(defmacro complex-method-definition (depth width)
  `(defmethod complex-method and ((self ,(make-class-name depth width))
                                  (n number) &rest rest)
    (declare (ignore rest))
    (,(make-accessor-name depth width) self)))

(defmacro after-method-definition (depth width)
  `(defmethod simple-method :after ((self ,(make-class-name depth width))
                             (n number))
    (setf (,(make-accessor-name depth width) self) ,(* depth width width))))

(defun defclass-forms ()
  (let (forms)
    (loop :for width :to +hierarchy-width+ :do
         (push `(defclass ,(make-class-name 1 width) (class-0-0) ()) forms))
    (loop :for dpth :from 2 :to +hierarchy-depth+ :do
          (loop :for wdth :to +hierarchy-width+ :do
                (push `(class-definition ,dpth ,wdth) forms)
                (push `(init-instance-definition ,dpth ,wdth) forms)))
    (nreverse forms)))

(defun defmethod-forms ()
  (let (forms)
    (loop :for dpth :from 2 to +hierarchy-depth+ :do
          (loop :for wdth :to +hierarchy-width+ :do
                (push `(simple-method-definition ,dpth ,wdth) forms)
                #-(or poplog)
                (push `(complex-method-definition ,dpth ,wdth) forms)))
    (nreverse forms)))

(defun after-method-forms ()
  (let (forms)
    (loop :for depth :from 2 :to +hierarchy-depth+ :do
          (loop :for width :to +hierarchy-width+ :do
                (push `(after-method-definition ,depth ,width) forms)))
    (nreverse forms)))

(defparameter *defclass-operator* nil)

(defun run-defclass ()
  (setq *defclass-operator* (compile nil `(lambda () ,@(defclass-forms))))
  (funcall *defclass-operator*))

(defun run-defclass-precompiled ()
  (funcall *defclass-operator*))

(defparameter *defmethod-operator* nil)

(defun run-defmethod ()
  (setq *defmethod-operator* (compile nil `(lambda () ,@(defmethod-forms))))
  (funcall *defmethod-operator*))

(defun run-defmethod-precompiled ()
  (funcall *defmethod-operator*))

(defun add-after-methods ()
  (funcall (compile nil `(lambda () ,@(after-method-forms)))))

;; ???
;; (defun make-instances ()
;;   (dotimes (i 5000)
;;     (dotimes (w +hierarchy-width+)
;;       (setf (aref *instances* w)
;;             (make-instance (make-class-name +hierarchy-depth+ w)
;;                            (make-initarg-name +hierarchy-depth+ w) 42))
;;       `(incf (slot-value (aref *instances* w) ',(make-attribute-name +hierarchy-depth+ w))))))

(defparameter *make-instances-operator* nil)

(defun make-instances ()
  (setq *make-instances-operator*
        (compile nil `(lambda ()
                        (dotimes (i 5000)
                          ,@(let ((forms nil))
                              (dotimes (w +hierarchy-width+)
                                (push `(progn (setf (aref *instances* ,w)
                                                    (make-instance ',(make-class-name +hierarchy-depth+ w)
                                                      ,(make-initarg-name +hierarchy-depth+ w) 42))
                                              (incf (slot-value (aref *instances* ,w)
                                                                ',(make-attribute-name +hierarchy-depth+ w))))
                                      forms))
                              (reverse forms))))))
  (funcall *make-instances-operator*))

(defun make-instances-precompiled ()
  (funcall *make-instances-operator*))

;; the code in the function MAKE-INSTANCES is very difficult to
;; optimize, because the arguments to MAKE-INSTANCE are not constant.
;; This test attempts to simulate the common case where some of the
;; parameters to MAKE-INSTANCE are constants.
(defclass a-simple-base-class ()
  ((attribute-one :accessor attribute-one
                  :initarg :attribute-one
                  :type string)))

(defclass a-derived-class (a-simple-base-class)
  ((attribute-two :accessor attribute-two
                  :initform 42
                  :type integer)))

(defun make-instances/simple ()
  (dotimes (i 5000)
    (make-instance 'a-derived-class
                   :attribute-one "The first attribute"))
  (dotimes (i 5000)
    (make-instance 'a-derived-class
                   :attribute-one "The non-defaulting attribute")))

(defun methodcall/simple (num)
  (dotimes (i 5000)
    (simple-method (aref *instances* num) i)))

(defun methodcalls/simple ()
  (dotimes (w +hierarchy-width+)
    (methodcall/simple w)))

(defun methodcalls/simple+after ()
  (add-after-methods)
  (dotimes (w +hierarchy-width+)
    (methodcall/simple w)))

(defun methodcall/complex (num)
  (dotimes (i 5000)
    (complex-method (aref *instances* num) i)))

(defun methodcalls/complex ()
  (dotimes (w +hierarchy-width+)
    (methodcall/complex w)))



;;; CLOS implementation of the Fibonnaci function, with EQL specialization

(defmethod eql-fib ((x (eql 0)))
   1)

(defmethod eql-fib ((x (eql 1)))
   1)

; a method for all other cases
(defmethod eql-fib (x)
   (+ (eql-fib (- x 1))
      (eql-fib (- x 2))))

;;; Misc
(defun permutations (x)
  (let* ((x x)
         (perms (list x)))
    (labels ((P (n)
               (if (> n 1)
                   (do ((j (- n 1) (- j 1)))
                       ((zerop j)
                        (P (- n 1)))
                     (P (- n 1))
                     (F n))))
             (F (n)
               (setf x (revloop x n (list-tail x n)))
               (push x perms))
             (revloop (x n y)
               (if (zerop n) y
                   (revloop (cdr x)
                            (- n 1)
                            (cons (car x) y))))
             (list-tail (x n)
               (if (zerop n) x
                   (list-tail (cdr x) (- n 1)))))
      (P (length x))
      perms)))

(defun iota (n)
  (do ((n n (- n 1))
       (p '() (cons n p)))
      ((zerop n) p)))

(defun integer-hash (key)
  (declare (type (unsigned-byte 32) key))
  (flet ((u32* (a b) (ldb (byte 32 0) (* a b)))
         (u32-right-shift (integer count)
           (ldb (byte 32 0) (ash integer count))))
    (u32* (u32-right-shift key 3) 2654435761)))

(defun make-big-list (n)
  (let ((list (list)))
    (dotimes (i n)
      (push (integer-hash n) list))
    list))
