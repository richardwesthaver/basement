;;; app.lisp --- App scraps from CORE

;; 

;;; Code:
(defconfig app-config (service-config)
  ((logger :initarg :logger :type logger-config)
   (db :initarg :db :type db-config)
   (thread-pool :initarg :thread-pool :type thread-pool)
   (hook :initarg :hook :type hook)))
