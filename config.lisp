;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

;;; Code to read the Whistle configuration file.

(defvar *current-file* nil)
(defvar *server* nil)

(defun read-dollar (stream char)
  (declare (ignore char))
  (let ((n (read stream t)))
    `(quote ,(intern (format nil "$~d" n)))))

(defparameter *config-readtable*
  (let ((*readtable*  (copy-readtable nil)))
    (set-macro-character #\$ #'read-dollar t)
    *readtable*))

(defun configure (server)
  (clear-configuration server)
  (let ((truename (truename (config-file server))))
    (setf (root-directory server) (parent-directory truename))
    (read-config-file server truename)
    (setf (urls server) (nreverse (urls server)))
    (setf (config-last-checked server) (file-write-date truename))))

(defun read-config-file (server file)
  (let ((*current-file* (truename file)))
    (with-open-file (in *current-file*)
      (with-standard-io-syntax
        (let ((*package* (find-package :whistle-config))
              (*readtable* *config-readtable*)
              (*default-pathname-defaults* (parent-directory *current-file*))
              (*server* server))
          (loop for clause = (read in nil nil)
             while clause do (handler-case (eval clause)
                               (error (c)
                                 (cerror "Skip clause."
                                         "Problem with clause ~s in ~a: ~a"
                                         clause *current-file* c)))))))))

(defun config-read (in)
  (let ((*package* (find-package :whistle-config))
        (*readtable* *config-readtable*))
    (read in nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operators to be used in configuration files.

(defun whistle-config:port (protocol port)
  "Set a port for the server to listen on, serving the given protocol. E.g. (port :http 8080)"
  (assert (eql protocol :http)) ;; for the moment this is all we support.
  (push (list protocol port) (ports *server*)))

(defun whistle-config:logs (name)
  "Set the directory for the server's log files. If a relative
  pathname is specified will be resolved relative to the directory
  containing the config file."
  (setf (log-directory *server*) (merge-pathnames (pathname-as-directory name))))

(defun whistle-config:data (name)
  (setf (data-directory *server*) (merge-pathnames (pathname-as-directory name))))

(defun whistle-config:include (name)
  (read-config-file *server* (merge-pathnames name)))

(defmacro whistle-config:passwords (&body passwords)
  `(progn
     (setf (passwords *server*) (mapcar (lambda (x) (cons (first x) (second x))) ',passwords))
     (setf (groups *server*)
           (let ((groups-map (make-hash-table)))
             (loop for (user password . groups) in ',passwords do
                  (loop for group in groups do
                       (push user (gethash group groups-map nil))))
             groups-map))))

(defmacro whistle-config:protections (&body clauses)
  `(setf (protections *server*) ',clauses))

(defmacro whistle-config:urls (&body clauses)
  `(progn
     ,@(loop for clause in clauses collect `(whistle-config:url ,@clause))))

(defun whistle-config:url (pattern name &rest args)
  (apply #'add-url *server* pattern name args))

(defmacro whistle-config:redirects (&body clauses)
  `(setf (redirects *server*) ',clauses))

(defmacro whistle-config:handlers (&body clauses)
  `(loop for (name class . args) in ',clauses do
        (add-handler *server* name (apply #'make-instance class args))))

(defmacro whistle-config::plugin (class &rest args)
  `(register-plugin (make-instance ',class ,@args) *server*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Plugins

(defgeneric register-plugin (plugin server)
  (:documentation "Install the plugin in the server."))
