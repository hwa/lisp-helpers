# cl-json-helper

Some helpers for cl-json

* with-reversible-json-encoder, with-reversible-json-decoder

The default encoder and decoder in cl-json are not inter-reversible, which means
JSON string decoded into Lisp object is encoded into string that is not the same
as the original JSON string.

These two macro provide the reversible way to encode and decode.
