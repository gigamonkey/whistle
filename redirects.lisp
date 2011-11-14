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
           (find-redirect (request-uri ,request) (redirects ,server))
         (cond
           (,code (redirect ,request ,new-uri :code ,code))
           (t ,@body))))))

(defun find-redirect (uri table)
  (loop for (pattern replacement code) in table do
       (multiple-value-bind (new-uri matched-p)
           (regex-replace pattern (uri-path uri) replacement)
         (when matched-p
           (return (values
                    code
                    (merge-uris (parse-uri new-uri) uri)))))))
