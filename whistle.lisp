;;; Copyright (c) 2011, 2012 Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

(defparameter *default-port* 9876)

(defvar *whistle-server* nil)

(defclass server ()
  ((config-file    :initarg :config-file :accessor config-file)
   (check-config   :initform t :accessor check-config)
   (root-directory :initarg :root-directory :accessor root-directory)
   (passwords      :initarg :passwords :initform () :accessor passwords)
   (realm          :initarg :realm :initform "Whistle" :accessor realm)
   (groups         :initarg :groups :initform () :accessor groups)
   (protections    :initarg :protections :initform () :accessor protections)
   (redirects      :initarg :redirects :initform () :accessor redirects)
   (urls           :initarg :urls :initform () :accessor urls)
   (log-directory  :initarg :log-directory :accessor log-directory)
   (access-log     :initarg :access-log :accessor access-log)
   (message-log    :initarg :message-log :accessor message-log)
   (ports          :initarg :ports :initform () :accessor ports)
   (acceptors      :initarg :acceptors :initform () :accessor acceptors)
   (handlers       :initarg :handlers :initform (make-hash-table) :accessor handlers)

   (config-last-checked :initform 0 :accessor config-last-checked)
   (static-handler :accessor static-handler)))

(defun start-whistle (&optional (config "./www/config.sexp"))
  "Start a whistle server, configured by the given config file."
  (setf *whistle-server* (server-setup config))
  (start-acceptors *whistle-server*))

(defun stop-whistle (&optional (server *whistle-server*))
  "Stop a whistle server, defaulting to *whistle-server*."
  (stop-acceptors server))

(defun restart-whistle (&optional (server *whistle-server*))
  "Restart a stopped a whistle server, defaulting to *whistle-server*."
  (start-acceptors server))

(defun config-file-updated (server)
  (> (file-write-date (config-file server)) (config-last-checked server)))

(defun add-handler (server name handler)
  (when (find-handler server name)
    (error "Duplicate handler name: ~a" name))
  (setf (gethash name (handlers server)) handler))

(defun find-handler (server name)
  (gethash name (handlers server)))

;; TODO: perhaps should provide a declarative way to automatically set
;; cookies on certain URLs so we don't have to write a special handler
;; just to set a cookie and then serve a static page or something.

(defmethod handle-request ((server server) request)
  "Whistle's implementation of Toot's handle-request method. After
  checking for redirects and authorization, we loop through the url
  patterns defined in the config file and hand the request off to the
  handler associated with the first matching pattern by calling the
  generate-response method."

  (when (and (check-config server) (config-file-updated server))
    (configure server))

  (with-redirects (request server)
    (with-authorization (request server)
      (loop with path = (request-path request)
         for (pattern handler . args) in (urls server)
         until (multiple-value-bind (match parts) (scan-to-strings pattern path)
                 (when match
                   (apply
                    #'generate-response
                    (find-handler server handler)
                    request
                    (fill-args args parts))
                   t))))))

(defun fill-args (args parts)
  (loop for arg in args
       for match-arg = (match-arg-p arg)
       when match-arg collect (aref parts (1- match-arg))
       else collect arg))

(defun match-arg-p (x)
  (and (symbolp x)
       (char= #\$ (char (symbol-name x) 0))
       (values (parse-integer (symbol-name x) :start 1 :junk-allowed t))))


(defun server-dir (server relative)
  (merge-pathnames relative (root-directory server)))

(defun content-file (server file)
  (merge-pathnames file (server-dir server "content/")))

(defun data-file (server file)
  (merge-pathnames file (server-dir server "data/")))

(defun data-directory (server dir)
  (data-file server (pathname-as-directory dir)))

(defun log-file (server file)
  (merge-pathnames file (server-dir server (log-directory server))))

(defun server-setup (config-file)
  (let ((server (make-instance 'server :config-file config-file)))
    (configure server)
    (open-logs server)
    server))

(defun clear-configuration (server)
  (setf (handlers server) (make-hash-table)))

(defun open-logs (server)
  (with-slots (log-directory access-log message-log) server
    (flet ((make-logger (file)
             (make-instance 'stream-logger :destination (open-log-file server file))))
      (setf access-log (make-logger "access.log"))
      (setf message-log (make-logger "messages.log")))))

(defun open-log-file (server file)
   (open (ensure-directories-exist (log-file server file))
         :direction :output
         :if-exists :append
         :if-does-not-exist :create))

(defun start-acceptors (server)
  (with-slots (acceptors) server
    (setf acceptors (loop for (protocol port) in (ports server) collect (make-acceptor server port)))
    (loop for acceptor in (acceptors server) do (start-acceptor acceptor))))

(defun stop-acceptors (server)
  (with-slots (acceptors) server
    (loop for acceptor in (acceptors server) do (stop-acceptor acceptor))))

(defun make-acceptor (server port)
  (with-slots (access-log message-log) server
    (make-instance 'acceptor
      :port port
      :handler server
      :access-logger access-log
      :message-logger message-log)))
