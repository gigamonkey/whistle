;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

(defparameter *default-port* 9876)

(defvar *whistle-server* nil)

(defclass server ()
  ((config-file    :initarg :config-file :accessor config-file)
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

   (static-handler :accessor static-handler)))

(defun start-whistle (&optional (config "./www/config.sexp"))
  (setf *whistle-server* (server-setup config))
  (start-acceptors *whistle-server*))

(defun handled-p (result)
  (not (eql result 'not-handled)))

(defmethod handle-request ((server server) request)
  (with-redirects (request server)
    (with-authorization (request server)
      (let ((*default-pathname-defaults* (merge-pathnames "content/" (root-directory server))))
        (loop with path = (uri-path (request-uri request))
           for (pattern fn) in (urls server)
           for result = (multiple-value-bind (match parts)
                            (scan-to-strings pattern path)
                          (and match
                               (cond
                                 ((eql fn 'static)
                                  (handle-request (static-handler server) request))
                                 (t (apply fn request (coerce parts 'list))))))
           when (and result (handled-p result)) return result
           finally (return 'not-handled))))))

(defun server-dir (server relative)
  (merge-pathnames relative (root-directory server)))

(defun content-file (server file)
  (merge-pathnames file (server-dir server "content/")))

(defun log-file (server file)
  (merge-pathnames file (server-dir server (log-directory server))))

(defun server-setup (config-file)
  (let ((server (make-instance 'server :config-file config-file)))
    (configure server)
    (open-logs server)
    server))

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

(defun make-acceptor (server port)
  (with-slots (access-log message-log) server
    (make-instance 'acceptor
      :port port
      :handler server
      :access-logger access-log
      :message-logger message-log)))
