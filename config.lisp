;;; Copyright (c) 2011, Peter Seibel.  All rights reserved.
;;;
;;; See LICENSE.txt for licensing information.

(in-package :whistle)

;;; Code to read the Whistle configuration file.

(defun configure (server)
  (with-open-file (in (config-file server))
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (loop for clause = (read in nil nil)
           while clause
           do (destructuring-bind (what . data) clause
                (parse-clause server what data))))))
  server)


(defgeneric parse-clause (server what data))

(defmethod parse-clause (server what data)
  (format *error-output* "~&Don't know how to parse ~s => ~s" what data))

(defmethod parse-clause (server (what (eql :ports)) ports)
  (setf (ports server) ports))

(defmethod parse-clause (server (what (eql :root-directory)) data)
  (destructuring-bind (dir) data
    (setf (root-directory server) (file-exists-p (pathname-as-directory dir)))))

(defmethod parse-clause (server (what (eql :log-directory)) data)
  (destructuring-bind (dir) data
    (setf (log-directory server) dir)))

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

(defun flatten (list) (mapcan #'copy-list list))
