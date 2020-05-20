(in-package #:hunchentoot-helper)

;;
;;;; server starter
;;

(defparameter *server* nil)
(defparameter *hunchentoot-log-dir* #p"/tmp")

(defun start-webserver (&optional (port 5000))
  (flet ((prefix-log-dir (log-file)
           (format nil "~a/~a" *hunchentoot-log-dir* log-file)))
    (let ((server
           (make-instance 'hunchentoot:easy-acceptor
                          :port port
                          :access-log-destination (prefix-log-dir "hunchentoot.log")
                          :message-log-destination (prefix-log-dir "hunchentoot.log"))))
      (setf *server* server)
      (hunchentoot:start server))))

;;
;;;; handler definition helper
;;

;;
;; usage example
;;
;; (define-easy-json-handler (handler-delete-base-category
;;                            :uri "/dealer/delete-base-category"
;;                            :default-request-type :POST)
;;     ((base-category-id :real-name "base-category" :parameter-type 'integer))
;;   (action-delete-base-category base-category-id))
;;
;; handler-delete-base-category becomes name of handler, :uri and
;; :default-request-type are definition parameters of handler.
;; ((base-category-id)) are request params bindings for use in &body.
;; action-delete-base-category is defined elsewhere, and returns lisp object
;; that would be encoded as json response to be sent out.


(defmacro define-easy-json-handler (description lambda-list &body body)
  (let ((but-last-expr (butlast body))
        (last-expr (last body)))
    `(hunchentoot:define-easy-handler ,description ,lambda-list
       (setf (hunchentoot:content-type*) "application/json")
       ,@but-last-expr
       (cl-json:encode-json-to-string ,@last-expr))))
