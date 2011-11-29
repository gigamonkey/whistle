;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :whistle
  (:use :cl
        :toot
        :com.gigamonkeys.utilities
        :com.gigamonkeys.pathnames
        :cl-ppcre)
  (:import-from :alexandria :once-only :with-unique-names :ends-with)

  ;; FIXME: Need to re-export symbols from :toot as well as a number
  ;; of symbols from :whistle
  (:export))
