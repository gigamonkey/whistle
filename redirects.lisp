(in-package :whistle)

;; A table of redirects. First item is the pattern to match against
;; the incoming URL. If the URL matches it is rewritten with the
;; replacement pattern in the second item. The third item is the HTTP
;; response code (should be one of the 3xx codes). N.B. It is possible
;; to create a set of redirects that causes a loop. Don't do that.

(defmacro with-redirects ((request server) &body body)
  (with-unique-names (code new-script-name)
    (once-only (request server)
      `(multiple-value-bind (,code ,new-script-name)
           (find-redirect (script-name ,request) (query-string ,request) (redirects ,server))
       (cond
         (,code (redirect ,request ,new-script-name :code ,code))
         (t ,@body))))))
    
(defun find-redirect (script-name query-string table)
  (loop for (pattern replacement code) in table
     do (multiple-value-bind (new-script-name matched-p)
            (regex-replace pattern script-name replacement)
          (when matched-p 
            (return (values code
                            (format nil "~a~@[?~a~]" new-script-name query-string)))))
     finally (return (values nil nil))))
