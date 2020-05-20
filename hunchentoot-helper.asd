(in-package #:cl-user)

(defpackage #:hunchentoot-helper-system
  (:use #:cl #:asdf))

(in-package #:hunchentoot-helper-system)

(defsystem hunchentoot-helper
  :author "hwa<dreameration@gmail.com>"
  :license "GPL 3.0"
  :description "My own helpers for using Hunchentoot."
  :depends-on (:hunchentoot)
  :components ((:file "package")
               (:file "helper")))
