(in-package #:cl-user)

(defpackage #:cl-yaml-helper-system
  (:use #:cl #:asdf))

(in-package #:cl-yaml-helper-system)

(defsystem cl-yaml-helper
  :author "hwa<dreameration@gmail.com>"
  :license "GPL 3.0"
  :description "My own helpers for cl-yaml"
  :depends-on (:cl-yaml)
  :components ((:file "package")
               (:file "helper")))
