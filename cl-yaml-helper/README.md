# cl-yaml-helper
Some helpers for cl-yaml

* encode-yaml-to-emitter

* encode-yaml-to-file

Encoding with newline and indent style, using C API's. The default encoder in
cl-yaml is written in Lisp rather than using C API, and output YAML string in
one line style.

* decode-yaml-from-file

Just wrapper for the decoder in cl-yaml.
