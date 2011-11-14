;;; Copyright (c) 2011, Peter Seibel.  All rights reserved.
;;;
;;; See LICENSE.txt for licensing information.

(in-package :cl-user)

(defpackage :whistle
  (:use :cl
        :toot
        :com.gigamonkeys.utilities
        :com.gigamonkeys.pathnames
        :cl-ppcre
        :puri)
  (:import-from :alexandria :once-only :with-unique-names :ends-with))

