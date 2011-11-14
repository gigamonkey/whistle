;;; Copyright (c) 2011, Peter Seibel.  All rights reserved.
;;;
;;; See LICENSE.txt for licensing information.

(in-package :whistle)

(defparameter *default-port* 9876)

(defvar *whistle-server* nil)

(defun test-server (&optional (config "./www/config.sexp"))
  (setf *whistle-server* (server-setup config))
  (start-acceptors *whistle-server*))

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
   (acceptors      :initarg :acceptors :initform () :accessor acceptors)))


(defun default-handler (request)
  (let ((path (uri-path (request-uri request))))
    (unless (safe-filename-p path)
      (abort-request-handler request +http-forbidden+))
    (serve-file request (merge-pathnames (subseq (add-index path) 1)))))

(defun add-index (filename &key (extension "html"))
  (format nil "~a~@[index~*~@[.~a~]~]" filename (ends-with #\/ filename) extension))

(defun handled-p (result)
  (not (eql result 'not-handled)))

(defmethod toot::handle-request ((server server) request)
  (with-redirects (request server)
    (with-authorization (request server)
      (let ((*default-pathname-defaults* (merge-pathnames "content/" (root-directory server))))
        (loop with path = (uri-path (request-uri request))
           for (pattern fn) in (urls server)
           thereis (multiple-value-bind (match parts)
                       (scan-to-strings pattern path)
                     (and match (handled-p (apply fn request (coerce parts 'list)))))
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

