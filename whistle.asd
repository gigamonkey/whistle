;;; Copyright (c) 2011, Peter Seibel.  All rights reserved.
;;;
;;; See LICENSE.txt for licensing information.

(in-package :cl-user)

(defpackage :whistle-asd 
  (:use :cl :asdf)
  (:export :*whistle-version*))

(in-package :whistle-asd)

(defvar *whistle-version* "0.0.1"
  "A string denoting the current version of Whistle. Used for
diagnostic output.")

(defsystem :whistle
  :version #.*whistle-version*
  :depends-on (:alexandria
               :toot
               :monkeylib-bcrypt
               :com.gigamonkeys.utilities
               :com.gigamonkeys.pathnames
               :cl-ppcre)
  :components ((:file "packages")
               (:file "whistle" :depends-on ("packages" "passwords" "redirects"))
               (:file "passwords" :depends-on ("packages" "utilities"))
               (:file "redirects" :depends-on ("packages" "utilities"))
               (:file "utilities" :depends-on ("packages"))))
