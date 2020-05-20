(in-package #:cl-yaml-helper)

(defun decode-yaml-from-file (yaml-file)
  (cl-yaml:parse yaml-file))

(defgeneric encode-yaml-to-emitter (emitter object))

(defmethod encode-yaml-to-emitter (emitter (object (eql 'nil)))
  (yaml.emitter:emit-object emitter object))

(defmethod encode-yaml-to-emitter (emitter (object list))
  (yaml.emitter:emit-sequence (emitter)
    (loop for el in object
          do
             (encode-yaml-to-emitter emitter el))))

(defmethod encode-yaml-to-emitter (emitter (object vector))
  (yaml.emitter:emit-sequence (emitter)
    (loop for el across object
          do
             (encode-yaml-to-emitter emitter el))))

(defmethod encode-yaml-to-emitter (emitter (object hash-table))
  (yaml.emitter:emit-mapping (emitter)
    (loop for k being the hash-key using (hash-value v) of object
          do
             (encode-yaml-to-emitter emitter k)
             (encode-yaml-to-emitter emitter v))))

(defmethod encode-yaml-to-emitter (emitter object)
  (yaml.emitter:emit-object emitter object))


(defun encode-yaml-to-file (file object)
  (with-open-file (stream file :direction :output :if-does-not-exist :create)
    (yaml.emitter:with-emitter-to-stream
        (emitter stream)
        (yaml.emitter:emit-stream (emitter)
          (yaml.emitter:emit-document (emitter)
            (encode-yaml-to-emitter emitter object))))))
