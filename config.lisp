;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

;;; Code to read the Whistle configuration file.

(defvar *current-file* nil)

(defun configure (server)
  (clear-configuration server)
  (read-config-file server (config-file server) (find-package :keyword)))

(defun read-config-file (server file package)
  (with-open-file (in file)
    (with-standard-io-syntax
      (let ((*package* package)
            (*current-file* in)
            (*default-pathname-defaults* (make-pathname :name nil :type nil :defaults (pathname in))))
        (setf (root-directory server) *default-pathname-defaults*)
        (loop for clause = (read in nil nil)
           while clause
           do (destructuring-bind (what . data) clause
                (parse-clause server what data))))))
  (setf (config-last-checked server) (file-write-date file)))

(defgeneric parse-clause (server what data)
  (:documentation "Parse one top-level clause of the config file."))

(defmethod parse-clause (server what data)
  (format *error-output* "~&Don't know how to parse ~s => ~s" what data))

(defmethod parse-clause (server (what (eql :ports)) ports)
  (setf (ports server) ports))

(defmethod parse-clause (server (what (eql :log-directory)) data)
  (destructuring-bind (dir) data
    (setf (log-directory server) (merge-pathnames dir))))

(defmethod parse-clause (server (what (eql :passwords)) passwords)
  (setf (passwords server) (mapcar (lambda (x) (cons (first x) (second x))) passwords))
  (setf (groups server)
        (let ((groups-map (make-hash-table)))
          (loop for (user password . groups) in passwords do
               (loop for group in groups do
                    (push user (gethash group groups-map nil))))
          groups-map)))

(defmethod parse-clause (server (what (eql :protections)) protections)
  (setf (protections server) protections))

(defmethod parse-clause (server (what (eql :urls)) urls)
  (setf (urls server) urls))

(defmethod parse-clause (server (what (eql :redirects)) redirects)
  (setf (redirects server) redirects))

;;; Config-file control

(defmethod parse-clause (server (what (eql :in-package )) data)
  (destructuring-bind (package) data
    (let ((pkg (find-package package)))
      (unless pkg
        (error "No package named: ~a" package))
      (setf *package* pkg))))

(defmethod parse-clause (server (what (eql :include)) data)
  (destructuring-bind (file) data
    (read-config-file server (merge-pathnames file) *package*)))

(defmethod parse-clause (server (what (eql :handlers)) data)
  (loop for (name class . args) in data do
       (add-handler server name (apply #'make-instance class args))))
