(in-package :sqlite)

(defar sqlite3-libversion c-string)
(defar sqlite3-libversion-number int)
(defar sqlite3-sourceid c-string)
(defar sqlite3-compileoption-used int (zopt-name c-string))
(defar sqlite3-threadsafe boolean)

(define-opaque sqlite3)

(defar sqlite3-close int (db (* sqlite3)))
(defar sqlite3-close-v2 int (db (* sqlite3)))

(define-alien-type sqlite3-callback
  (function int
      (* t)
      int
    (* c-string)
    (* c-string)))

(defar sqlite3-exec int
  (db (* sqlite3))
  (sql c-string)
  (callback (* sqlite3-callback))
  (ret (* t))
  (errmsg (* c-string)))

(define-alien-enum (sqlite-result)
  :ok 0
  :error 1
  :internal 2
  :perm 3
  :abort 4
  :busy 5
  :locked 6
  :nomem 7
  :readonly 8
  :interrupt 9
  :ioerr 10
  :corrupt 11
  :notfound 12
  :full 13
  :cantopen 14
  :protocol 15
  :empty 16
  :schema 17
  :toobig 18
  :constraint 19
  :mismatch 20
  :misuse 21
  :nolfs 22
  :auth 23
  :format 24
  :range 25
  :notadb 26
  :notice 27
  :warning 28
  :row 100
  :done 101)

(defar sqlite3-open int
  (filename c-string)
  (ppdb (* (* sqlite3))))

(defar sqlite3-open16 int
  (filename c-string)
  (ppdb (* (* sqlite3))))

(defar sqlite3-open-v2 int
  (filename c-string)
  (ppdb (* (* sqlite3)))
  (flags int)
  (zvfs c-string))
