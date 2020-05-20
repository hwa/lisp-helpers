(in-package #:cl-user)

(defpackage #:cl-json-helper-system
  (:use #:cl #:asdf))

(in-package #:cl-json-helper-system)

(defsystem cl-json-helper
  :author "hwa<dreameration@gmail.com>"
  :license "GPL 3.0"
  :description "My own helpers for cl-json"
  :depends-on (:cl-json)
  :components ((:file "package")
               (:file "helper")))
