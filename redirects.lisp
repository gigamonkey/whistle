;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :whistle)

;; A table of redirects. First item is the pattern to match against
;; the incoming URL. If the URL matches it is rewritten with the
;; replacement pattern in the second item. The third item is the HTTP
;; response code (should be one of the 3xx codes). N.B. It is possible
;; to create a set of redirects that causes a loop. Don't do that.

(defmacro with-redirects ((request server) &body body)
  (with-unique-names (code new-uri)
    (once-only (request server)
      `(multiple-value-bind (,code ,new-uri)
           (find-redirect ,request (redirects ,server))
         (cond
           (,code (redirect ,request ,new-uri :code ,code))
           (t ,@body))))))

(defun find-redirect (request table)
  (loop for (pattern replacement code) in table do
       (multiple-value-bind (new-uri matched-p)
           (regex-replace pattern (request-path request) replacement)
         (when matched-p
           (return
             (values
              code
              (princ-to-string (puri:merge-uris (puri:parse-uri new-uri) (request-uri request)))))))))
