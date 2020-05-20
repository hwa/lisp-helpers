(in-package #:cl-json-helper)

;;
;;;; cl-json reversible encoder and decoder.
;;

;; The default encoder doesn't distinguish {}, [], null, false.
;; So we shall use WITH-CODER-SIMPLE-CLOS-SEMANTICS to decode {} as empty Object;
;; set *JSON-ARRAY-TYPE* to 'VECTOR to decode [] as #();
;; set *BOOLEAN-HANDLER* to decode true as '(:TRUE), false as '(:FALSE),
;; null as '(:NULL) so as for EXPLICIT-ENCODER to encode them correctly back.

;;The explicit decoder: If S is a list, the first symbol defines the encoding:
;; If (car S) is :TRUE return a JSON true value. If (car S) is :FALSE return a
;; JSON false value. If (car S) is :NULL return a JSON null value. If (car S) is
;; :JSON princ the strings in (cdr s) to stream If (car S) is :LIST or :ARRAY
;; encode (cdr S) as a a :JSON Array. If (car S) is :OBJECT encode (cdr S) as
;; A JSON Object, interpreting (cdr S) either as an A-LIST or a P-LIST.

;; (test explicit-encoder
;;   (is (string= "true" (with-explicit-encoder
;;                         (encode-json-to-string '(:true))))
;;       "True")
;;   (is (string= "false"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:false))))
;;       "False")
;;   (is (string= "null"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:null))))
;;       "False")
;;   (is (string= "[1,\"a\"]"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:list 1 "a"))))
;;       "List")
;;   (is (string= "[1,\"a\"]"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:array 1 "a"))))
;;       "Array")
;;   (is (string= "{\"a\":1,\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:object "a" 1 "b" 2))))
;;       "Plist")
;;   (is (string= "{\"a\":1,\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:object ("a" . 1) ( "b" . 2)))))
;;       "Alist")
;;   (is (string= "{\"a\":1,\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:alist ("a" . 1) ( "b" . 2)))))
;;       "Explicit Alist")
;;   (is (string= "{\"a\":1,\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:plist "a" 1 "b" 2))))
;;       "Explicit Plist")
;;   (is (string= "{\"a\":{\"c\":1,\"d\",2},\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:object
;;                                           "a" (:json "{\"c\":1,\"d\",2}")
;;                                           "b" 2))))
;;       "Embedded json")
;;   (is (string= "{\"a\":{\"c\":1,\"d\":2},\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:object
;;                                           "a" (:object "c" 1 "d" 2)
;;                                           "b" 2))))
;;       "Object in object")
;;   (is (string= "{\"a\":{\"c\":1,\"d\":2},\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:object
;;                                           "a" (:object ( "c" . 1) ( "d" . 2))
;;                                           "b" 2))))
;;       "Mixed alist and plist")
;;   (is (string= "{\"a\":1,\"b\":2}"
;;                (with-explicit-encoder
;;                  (encode-json-to-string '(:object ("a" . 1) nil ( "b" . 2)))))
;;       "Alist with a nil value"))


(defmacro with-reversible-json-decoder (&body body)
  `(cl-json:with-decoder-simple-clos-semantics
     (let ((cl-json:*json-array-type* 'vector)
           (cl-json:*boolean-handler* #'(lambda (str)
                                          (let ((mapping '(("true" . (:true))
                                                           ("false" . (:false))
                                                           ("null" . (:null)))))
                                            (cdr (assoc str mapping :test #'string=))))))
       ,@body)))

(defmacro with-reversible-json-encoder (&body body)
  `(cl-json:with-explicit-encoder ,@body))



;;;;
