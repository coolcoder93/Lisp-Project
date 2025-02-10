;;(cl:in-package "CL-USER")

(require "ASDF")

(defparameter *current-directory*
  (uiop:pathname-directory-pathname (uiop:current-lisp-file-pathname)))

(asdf:load-asd (merge-pathnames "alexandria-v1.4/alexandria.asd"
                                *current-directory*))

(asdf:load-asd (merge-pathnames "babel_0.5.0/babel.asd"
                                *current-directory*))

(asdf:load-asd (merge-pathnames "trivial-features_1.0/trivial-features.asd"
                                *current-directory*))

(asdf:load-asd (merge-pathnames "cffi_0.24.1/cffi.asd"
                                *current-directory*))

(asdf:load-system "cffi")
