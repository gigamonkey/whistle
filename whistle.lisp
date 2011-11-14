;;; Copyright (c) 2011, Peter Seibel.  All rights reserved.
;;;
;;; See LICENSE.txt for licensing information.

(in-package :whistle)

(defparameter *default-port* 9876)

(defvar *whistle-server* nil)

(defun test-server ()
  (setf *whistle-server* (server-setup "./www/"))
  (start-acceptors *whistle-server*))

(defun reread-config (&optional (server *whistle-server*))
  (read-configuration-files server))

(defclass server ()
  ((root-directory :initarg :root-directory :accessor root-directory)
   (passwords      :initarg :passwords :initform () :accessor passwords)
   (realm          :initarg :realm :initform "Whistle" :accessor realm)
   (groups         :initarg :groups :initform () :accessor groups)
   (protections    :initarg :protections :initform () :accessor protections)
   (redirects      :initarg :redirects :initform () :accessor redirects)
   (urls           :initarg :urls :initform () :accessor urls)
   (log-directory  :initarg :log-directory :initform "logs/" :accessor log-directory)
   (access-log     :initarg :access-log :accessor access-log)
   (message-log    :initarg :message-log :accessor message-log)
   (ports          :initarg :ports :initform () :accessor ports)
   (acceptors      :initarg :acceptors :initform () :accessor acceptors)))


(defun default-handler (request)
  (let ((path (uri-path (request-uri request))))
    (unless (safe-filename-p path)
      (abort-request-handler request +http-forbidden+))
    (serve-file request (resolve-file path))))

(defun resolve-file (path)
  (merge-pathnames (subseq (add-index path) 1)))

(defun add-index (filename &key (extension "html"))
  (format nil "~a~@[index~*~@[.~a~]~]" filename (ends-with #\/ filename) extension))

(defun handled-p (result)
  (not (eql result 'not-handled)))

(defmethod toot::handle-request ((server server) request)
  (with-redirects (request server)
    (with-authorization (request server)
      (let ((*default-pathname-defaults* (merge-pathnames "content/" (root-directory server))))
        (loop with path = (uri-path (request-uri request))
           for (pattern . fn) in (urls server)
           thereis (multiple-value-bind (match parts)
                       (scan-to-strings pattern path)
                     (and match (handled-p (apply fn request (coerce parts 'list)))))
           finally (return 'not-handled))))))

(defun server-dir (server relative)
  (merge-pathnames relative (root-directory server)))

(defun content-file (server file)
  (merge-pathnames file (server-dir server "content/")))

(defun config-file (server file)
  (merge-pathnames file (server-dir server "config/")))

(defun log-file (server file)
  (merge-pathnames file (server-dir server (log-directory server))))


(defun make-server (dir)
  (let ((actual-dir (file-exists-p (pathname-as-directory dir))))
    (cond
      (actual-dir (make-instance 'server :root-directory actual-dir))
      (t (error "~a does not exist." dir)))))

(defun server-setup (dir)
  (let ((server (make-server dir)))
    (read-configuration-files server)
    (open-logs server)
    server))

(defun read-configuration-files (server)
  (let ((*package* #.*package*))
    (load-ports "ports.sexp" server)
    (load-redirects "redirects.sexp" server)
    (load-passwords "passwords.sexp" server)
    (load-protections "protections.sexp" server)
    (load-urls "urls.sexp" server)))

(defun open-logs (server)
  (with-slots (log-directory access-log message-log) server
    (flet ((make-logger (file)
             (make-instance 'stream-logger :destination (open-log-file server file))))
      (setf access-log (make-logger "accept.log"))
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

(defun load-ports (file server)
  (setf (ports server) (file->list (config-file server file))))

(defun load-redirects (file server)
  (setf (redirects server) (file->list (config-file server file))))

(defun load-passwords (file server)
  (let ((passwords (file->list (config-file server file))))
    (setf (passwords server) (mapcar (lambda (x) (cons (first x) (second x))) passwords))
    (setf (groups server)
          (let ((groups-map (make-hash-table)))
            (loop for (user password . groups) in passwords do
                 (loop for group in groups do
                      (push user (gethash group groups-map nil))))
            groups-map))))

(defun load-protections (file server)
  (setf (protections server) (file->list (config-file server file))))

(defun load-urls (file server)
  (setf (urls server) (file->list (config-file server file))))
