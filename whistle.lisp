;;; Copyright (c) 2011, 2012 Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

(defvar *whistle-servers* (make-hash-table :test #'equal))

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
   (data-directory :initarg :data-directory :accessor data-directory)
   (access-log     :initarg :access-log :accessor access-log)
   (message-log    :initarg :message-log :accessor message-log)
   (ports          :initarg :ports :initform () :accessor ports)
   (acceptors      :initarg :acceptors :initform () :accessor acceptors)
   (handlers       :initarg :handlers :initform (make-hash-table) :accessor handlers)

   (config-last-checked :initform 0 :accessor config-last-checked)))

(defun start-whistle (config)
  "Start a whistle server, configured by the given config file."
  (let ((already-running (find-server config)))
    (when already-running
      (restart-case
          (error "Already a server running for ~a" config)
        (kill-it ()
          :report "Kill it."
          (stop-acceptors already-running))))
    (let ((server (server-setup config)))
      (start-acceptors server)
      (setf (gethash (truename config) *whistle-servers*) server))))

(defun stop-whistle (config)
  "Stop the whistle server, if any, running for the given config file."
  (when-let ((server (find-server config)))
    (stop-acceptors server)
    (remhash (truename config) *whistle-servers*)))

(defun stop-all-servers ()
  "Stop all the known running servers and clear *whistle-servers*."
  (loop for k being the hash-keys using (hash-value v) of *whistle-servers* do
       (format t "~&Stopping server for ~a" k)
       (stop-acceptors v))
  (clrhash *whistle-servers*))

(defun find-server (config)
  "Find the running server, if any, running for the given config file."
  (gethash (truename config) *whistle-servers*))

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

(defun log-file (server file)
  (merge-pathnames file (log-directory server)))

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
