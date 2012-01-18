;;; Copyright (c) 2011, 2012 Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

(defgeneric generate-response (handler request &rest rest))

(defmethod generate-response ((handler static-file-handler) request &key &allow-other-keys)
  "Bridge between Whistle handler API and Toot."
  (handle-request handler request))

;; Our slightly more flexible static file handler

(defclass whistle-static-file-handler ()
  ((root :initarg :root :accessor root)
   (path-checker :initarg :path-checker :initform #'safe-pathname-p :accessor path-checker))
  (:documentation "A handler that serves files found under a given root directory."))

(defmethod initialize-instance :after ((h whistle-static-file-handler) &key &allow-other-keys)
  (with-slots (root) h
    (setf root (truename (merge-pathnames root)))))

(defmethod generate-response ((handler whistle-static-file-handler) request &key path &allow-other-keys)
  (with-slots (root path-checker) handler
    (let ((*default-pathname-defaults* root))
      (unless (funcall path-checker path)
        (abort-request-handler +http-forbidden+))
      (let ((file (merge-pathnames (add-index path))))
        (serve-file request file)))))

(defmethod generate-response ((handler function) request &rest args)
  (apply handler request args))

;; A sample handler class for demonstration purposes.

(defclass numeral-handler () ())

(defmethod generate-response ((handler numeral-handler) request &key number)
  (setf (content-type request) "text/plain")
  (with-response-body (out request)
    (format out "~r" (parse-integer number))))


(defun demo-function (request n)
  "A function that can be used directly as a responder."
  (setf (content-type request) "text/plain")
  (with-response-body (out request)
    (format out "~@r" (parse-integer n))))
