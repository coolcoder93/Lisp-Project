;; Profiling:
;; (profile foo:bar)
;; (foo:bar args...)
;; (report)
;;
;; (profile) ; => returns list of all registered functions for profiling
;; (unprofile foo:bar) ; stops profiling foo:bar
;;
;; Statistical profiling:
;; (require "SB-SPROF")
;; (sb-sprof:with-profiling (:report :flat)
;;   (foo:bar args..))
;;
;; Measure time (and memory usage):
;; (time (func args...))
;;
;; EVAL-WHEN:
;; :compile-toplevel, evaled when (compile-file "foo.lisp")
;; :load-toplevel, evaled when (load "foo.fasl")
;; :execute, evaled whenever else e.g (load "foo.lisp")

(cl:in-package "CL-USER")
(declaim (optimize (debug 3)))

(defun main ()
  (format t "Hello, World!~%"))

(defmacro define-yam-lisp ()
  (let ((number-of-cl-symbols
          (let ((counter 0))
            (do-external-symbols (symbol "CL" counter)
              (incf counter))))
        (shadowed-symbols #(atom
                            null
                            delete-duplicates
                            define-condition
                            defconstant)))
    (flet ((collect-cl-symbols ()
             (let ((cl-symbols (make-list (- number-of-cl-symbols
                                             (length shadowed-symbols))))
                   (counter 0))
               (do-external-symbols (symbol "CL" cl-symbols)
                 (unless (find symbol shadowed-symbols)
                   (setf (elt cl-symbols counter) (symbol-name symbol))
                   (incf counter))))))
      `(defpackage "YAM-LISP"
         (:use "CL")
         (:export "DEFSUBST"
                  "DEFCONDITION"
                  "DEFCONST"
                  "DEFGLOBAL"
                  "ATOMP"
                  "NULLP"
                  "NREMOVE-DUPLICATES"
                  "WITH-GENSYMS"
                  ,@(collect-cl-symbols))))))

(define-yam-lisp)

(in-package "YAM-LISP")

(defmacro defsubst (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,@body)))

(defmacro defcondition (name supers &body (slots &rest options))
  `(cl:define-condition ,name ,supers
     ,slots
     ,@options))

(defmacro defconst (name value &optional documentation)
  `(cl:defconstant ,name (if (boundp ',name)
                              (symbol-value ',name)
                              ,value)
     ,documentation))

(defmacro defglobal (name value &optional documentation)
  "TODO: Documentation
Naming conventions:
Variables defined using DEFGLOBAL should be surrounded by slashes

Examples:
(defglobal *x* 10) ;; Bad style, *X* looks like a special variable
(defglobal +x+ 10) ;; Bad style, +X+ looks like a constant
(defglobal x 10)   ;; Bad style, it is unclear what X is
(defglobal /x/ 10) ;; Good style, it is clear that /X/ is a global lexical variable"
  #+sbcl `(sb-ext:defglobal ,name ,value ,documentation)
  #-sbcl `(progn
            (define-symbol-macro ,name ,value)
            (setf (symbol-value ',name) ,value)
            ,(when documentation
               `(setf (documentation ',name 'variable) ,documentation))
            ',name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defsubst atomp (object)
    (cl:atom object)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defsubst nullp (object)
    (cl:null object)))

(defsubst nremove-duplicates (sequence
                              &key
                                (test #'eql)
                                test-not
                                (start 0)
                                end
                                from-end
                                key)
  (cl:delete-duplicates sequence
                        :test test
                        :test-not test-not
                        :start start
                        :end end
                        :from-end from-end
                        :key key))
;; Usage:
;; (with-gensyms (a b)
;;   `(let* ((,a 10)
;;           (,b (+ ,a 16)))
;;      ,b))
;; Expands to:
;; (let ((a (gensym)) (b (gensym)))
;;   `(let* ((,a 10) (,b (+ ,a 16)))
;;      ,b))
;; It is important to use "," before the names of the symbols
(defmacro with-gensyms (symbols &body body)
  (let ((gensyms (list)))
    (declare (dynamic-extent gensyms))
    (flet ((collect-gensyms ()
             (dolist (symbol symbols (nreverse gensyms))
               (push `(,symbol (gensym "WITH-GENSYM")) gensyms))))
      `(let (,@(collect-gensyms))
         ,@body))))
