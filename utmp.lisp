;;; utmp.lisp --- UTMP Aliens

;; /var/run/utmp

;; /var/log/wtmp

#| method
           struct utmp ut;
           struct timeval tv;

           gettimeofday(&tv, NULL);
           ut.ut_tv.tv_sec = tv.tv_sec;
           ut.ut_tv.tv_usec = tv.tv_usec;
|#

;;; Code:
(defpkg :utmp
  (:use :cl :std :sb-alien))
(in-package :utmp)

(defvar *utmp-path* "/var/run/utmp")
(defvar *wtmp-path* "/var/log/wtmp")

(define-alien-enum (ut :type int)
  :linesize 32
  :namesize 32
  :hostsize 256)

(define-alien-enum (ut-type :type int)
  :empty 0
  :run-lvl 1
  :boot-time 2
  :new-time 3
  :old-time 4
  :init-process 5
  :login-process 6
  :user-process 7
  :dead-process 8
  :accounting 9)

(define-alien-type exit-status 
  (struct exit-status
    (e-termination short)
    (e-exit short)))

(define-alien-type utmp
  (struct utmp
    (ut-type short)
    (ut-pid pid-t)
    (ut-line (array char #.(ut :linesize)))
    (ut-id (array char 4))
    (ut-user (array char #.(ut :namesize)))
    (ut-host (array char #.(ut :hostsize)))
    (ut-exit exit-status)
    (ut-session long)
    (ut-tv timeval)
    (ut-addr-v6 (array int 4))
    (--unused (array char 20))))

(define-alien-routine getutent (* utmp))
(define-alien-routine setutent (* utmp))
(define-alien-routine endutent (* utmp))
(define-alien-routine getutid (* utmp) (id (* utmp)))
(define-alien-routine getutline (* utmp) (line (* utmp)))
(define-alien-routine pututline (* utmp) (ptr (* utmp)))
(define-alien-routine login-tty int (fd int))
(define-alien-routine login void (entry (* tmp)))
(define-alien-routine logout int (ut-line (* char)))
(define-alien-routine logwtmp void
  (ut-line (* char))
  (ut-name (* char))
  (ut-host (* char)))
(define-alien-routine udpwtmp void
  (wtmp-file (* char))
  (utmp (* utmp)))
(define-alien-routine utmpname int
  (file (* char)))
