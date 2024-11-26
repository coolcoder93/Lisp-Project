;; Debugging:
;; (disassemble) See assembly
;; (inspect) See the symbols internals
;; (describe) Similar to INSPECT but less interactive
;; (trace)/(untrace) Trace to flow of a function, e.g (trace foo) (foo)
;; (step) Step through function execution, similar to trace but interactive
;; (break) Insert breakpoints
;;
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

(cl:in-package "COMMON-LISP-USER")

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
         (:use "COMMON-LISP")
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

(defpackage "FOO"
  (:use "YAM-LISP")
  (:export "MAIN"))

(in-package "FOO")

(sb-alien:define-alien-type glfw-window (sb-alien:* t))
(sb-alien:define-alien-type glfw-monitor (sb-alien:* t))

(sb-alien:define-alien-routine "glfwInit"
  sb-alien:int)

(sb-alien:define-alien-routine "glfwTerminate"
  sb-alien:void)

(sb-alien:define-alien-routine "glfwWindowHint"
  sb-alien:void
  (hint sb-alien:int)
  (value sb-alien:int))

(sb-alien:define-alien-routine "glfwCreateWindow"
  glfw-window
  (width sb-alien:int)
  (height sb-alien:int)
  (title sb-alien:c-string)
  (monitor glfw-monitor)
  (share glfw-window))

(sb-alien:define-alien-routine "glfwDestroyWindow"
  sb-alien:void
  (window glfw-window))

;; (sb-alien:define-alien-routine "glfwMakeContextCurrent"
;;   sb-alien:void
;;   (window glfw-window))

(sb-alien:define-alien-routine "glfwSwapBuffers"
  sb-alien:void
  (window glfw-window))

(sb-alien:define-alien-routine "glfwPollEvents"
  sb-alien:void)

(sb-alien:define-alien-routine "glfwWindowShouldClose"
  sb-alien:int
  (window glfw-window))

(defconst +glfw-true+ 1)
(defconst +glfw-false+ 0)
(defconst +glfw-no-api+ 0)

(defconst +glfw-client-api+ #x00022001)
(defconst +glfw-resizable+ #x00020003)

(defsubst window-is-open-p (window)
  (= (glfwWindowShouldClose window) +glfw-false+))

(defsubst swap-window-buffers (window)
  (glfwSwapBuffers window))

(defsubst poll-events ()
  (glfwPollEvents))

(defmacro with-engine (&body body)
  `(progn
     (sb-alien:load-shared-object "libglfw.so")
     (when (= (glfwInit) +glfw-false+)
       (error "Failed to inititalize GLFW"))
     (glfwWindowHint +glfw-resizable+ +glfw-false+)
     (glfwWindowHint +glfw-client-api+ +glfw-no-api+)
     (unwind-protect (progn ,@body)
       (glfwTerminate))))

(defmacro with-window (window (title width height) &body body)
  (check-type window symbol)
  `(let ((,window (glfwCreateWindow ,width
                                    ,height
                                    ,title
                                    nil
                                    nil)))
     (if (sb-alien:null-alien window)
         (error "Failed to create window with GLFW")
         (unwind-protect (progn ,@body)
           (glfwDestroyWindow ,window)))))

(defmacro while (condition &body body)
  `(do ()
       ((not ,condition))
     ,@body))

(defun main ()
  (with-engine
    (with-window window ("Test" 1280 720)
      (while (window-is-open-p window)
        (swap-window-buffers window)
        (poll-events)))))
