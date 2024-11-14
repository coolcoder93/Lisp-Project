(load "main")

(defconstant +executable-name+ "foo.image")

(setf sb-ext:*block-compile-default* t)

(sb-ext:save-lisp-and-die +executable-name+
                          :toplevel #'main
                          :executable t)
