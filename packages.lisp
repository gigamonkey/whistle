;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :whistle
  (:use :cl
        :toot
        :com.gigamonkeys.utilities
        :com.gigamonkeys.pathnames
        :cl-ppcre
        :puri)
  (:import-from :alexandria :once-only :with-unique-names :ends-with)

  ;; FIXME: Need to re-export symbols from :toot and :puri as well as
  ;; a number of symbols from :whistle
  (:export
   :default-handler))
