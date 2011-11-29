;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

(defgeneric write-passwords-file (output source &key cost)
  (:documentation "Write a passwords file containing user names and
  bcrypt'ed hashes for a source containing user names and plaintext
  passwords."))

(defmethod write-passwords-file (file (source string) &key cost)
  (write-passwords-file file (pathname source) :cost cost))

(defmethod write-passwords-file (file (source pathname) &key cost)
  (write-passwords-file file (file->list source) :cost cost))

(defmethod write-passwords-file (file (source cons) &key cost)
  (with-output-to-file (out file)
    (with-data-io-syntax
      (print `(:passwords
               ,@(loop for (user password . rest) in source collect
                      (list* user (bcrypt:hash password cost) rest)))
             out))))

(defmacro with-authorization ((request server &key (realm "Whistle")) &body body)
  "Execute body if the request has appropriate authorization.
  Otherwise REQUIRE-AUTHORIZATION with REALM."
  (once-only (request server)
    `(multiple-value-bind (protected users)
         (protection (groups server) (request-uri ,request) (protections ,server))
       (cond
         ((and protected (not (authorizedp ,request users (passwords ,server))))
          (require-authorization ,request ,realm))
         (t ,@body)))))

(defun authorizedp (request users passwords)
  (multiple-value-bind (user password) (authorization request)
    (and (valid-user-p user users) (check-password user password passwords))))

(defun valid-user-p (user users)
  (member user users :test #'string=))

(defun check-password (user password passwords)
  (bcrypt:password= password (cdr (assoc user passwords :test #'string=))))

(defun protection (groups uri table)
  (loop for (pattern . users-and-groups) in table
     when (scan pattern (uri-path uri))
     return (values t (expand-groups groups (cons :wheel users-and-groups)))))

(defun expand-groups (groups users-and-groups)
  (remove-duplicates
   (loop for x in users-and-groups append
        (etypecase x
          (keyword (gethash x groups))
          (string (list x))))
   :test #'equal))
