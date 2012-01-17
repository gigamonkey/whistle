;;; Copyright (c) 2011, 2012 Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

(defgeneric coerce-parameter (type value)
  (:documentation "Coerce a request parameter into a named type."))

(defmethod coerce-parameter ((type (eql 'string)) value) value)

(defmethod coerce-parameter ((type (eql 'keyword)) value)
  (keywordize value))

(defmethod coerce-parameter ((type (eql 'integer)) value)
  (parse-integer value))

(defmacro with-parameters ((&rest clauses) request &body body)
  (once-only (request)
    `(let (,@(loop for clause in clauses collect
                  (destructuring-bind (name &optional (type 'string)) clause
                    `(,name (coerce-parameter ',type (parameter ,(string-downcase name) ,request))))))
       ,@body)))
