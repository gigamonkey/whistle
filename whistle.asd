;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :whistle-asd
  (:use :cl :asdf)
  (:export :*whistle-version*))

(in-package :whistle-asd)

(defvar *whistle-version* "0.0.1"
  "A string denoting the current version of Whistle. Used for
diagnostic output.")

(defsystem :whistle
  :description "A more full-featured web server built on top of Toot."
  :version #.*whistle-version*
  :depends-on (:alexandria
               :toot
               :monkeylib-bcrypt
               :com.gigamonkeys.utilities
               :com.gigamonkeys.pathnames
               :cl-ppcre
               :puri)
  :components ((:file "packages")
               (:file "whistle" :depends-on ("packages" "passwords" "redirects"))
               (:file "api" :depends-on ("packages"))
               (:file "config" :depends-on ("packages"))
               (:file "passwords" :depends-on ("packages"))
               (:file "redirects" :depends-on ("packages"))
               (:file "handlers" :depends-on ("packages"))))
