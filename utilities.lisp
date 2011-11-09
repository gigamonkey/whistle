(in-package :whistle)

;;; Utility code that is not particularly tied to this project.

(defun keywordize (s)
  (intern (string-upcase s) :keyword))

(defun md5-file (file)
  (format nil "~(~{~2,'0x~}~)" (coerce (md5:md5sum-file file) 'list)))

(defun md5-string (string)
  (format nil "~(~{~2,'0x~}~)" (coerce (md5:md5sum-sequence string) 'list)))

(defun sanitize-type (filename)
  (case (keywordize (pathname-type filename))
    ((:tar.gz :tgz :tar_gz) "tgz")
    (:zip "zip")
    (t "unknown")))

(defun unlist (x)
  (if (not (rest x)) (first x) x))

(defun string-ends-with-p (string end)
  (let ((string-length (length string))
	(end-length (length end)))
    (and (<= end-length string-length)
	 (string= end string :start2 (- string-length end-length)))))

(defmacro with-assoc ((&rest names) alist &body body)
  (once-only (alist) 
    `(let (,@(loop for name in names collect `(,name (cdr (assoc ',name ,alist)))))
       ,@body)))

(defmacro let-alist ((&rest bindings) alist &body body)
  (with-unique-names (getit)
    (once-only (alist)
      `(flet ((,getit (k)
                (let ((cons (assoc (string k) ,alist :test #'string-equal)))
                  (unless cons
                    (warn "No entry for ~a" k))
                  (cdr cons))))
         (let (,@(loop for b in bindings
                    collect `(,b (,getit ',b))))
           ,@body)))))

(defmacro parser-case ((s c) &body clauses)
  (once-only (s c)
    `(case ,s
       ,@(loop for clause in clauses collect
	      (destructuring-bind (state . character-clauses) clause
		`(,state
		  (case ,c
		    ,@character-clauses)))))))

(defmacro with-data-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*print-case* :downcase)
	   (*read-eval* nil)
	   (*package* (find-package :keyword)))
       ,@body)))

(defmacro with-data-to-file ((stream file &key mode) &body body)
  (declare (ignore mode))
  `(with-open-file (,stream (ensure-directories-exist (merge-pathnames ,file))
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
     (with-data-io-syntax ,@body)))


(defmacro with-lock-file ((filename) &body body)
  (once-only (filename)
    `(progn
       ;; On SBCL anyway, the following OPEN should result in a call
       ;; to open() with O_EXCL which does what we need.
       (loop for x = (open (ensure-directories-exist ,filename) :direction :output :if-exists nil :if-does-not-exist :create)
          when x return (close x))
       (unwind-protect (progn ,@body)
         (delete-file ,filename)))))
