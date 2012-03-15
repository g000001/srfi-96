;;;; srfi-96.lisp

(cl:in-package :srfi-96.internal)
;; (in-readtable :srfi-96)

(def-suite srfi-96)

(in-suite srfi-96)

(cl:defmacro defsynonymfun (name fcn)
  `(cl:setf (cl:fdefinition ',name) ,fcn) )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dynamic-wind (in body out)
    (declare (function in body out))
    (funcall in)
    (unwind-protect (funcall body)
      (funcall out) )))

;;; Configuration

;;; - software-type

(defun scheme-implementation-type ()
  (cl:lisp-implementation-type) )

(defun scheme-implementation-version ()
  (cl:lisp-implementation-version) )

(defun scheme-implementation-home-page ()
  #+sbcl "http://www.sbcl.org/"
  #-(or sbcl) nil
  )

(defun scheme-file-suffix ()
  #+sbcl ".lisp"
  #-(or sbcl) nil
  )

(defvar |SLIB:FEATURES| cl:*features*)

;;; - most-positive-fixnum

;;; - char-code-limit

;;; File-System

(flet ((exchange (new)
           (let ((old *load-pathname*))
             (setq *load-pathname* new)
             old)))
  (defun with-load-pathname (path thunk)
    (let ((old 'NIL))
	(dynamic-wind
            (lambda () (setq old (exchange path)))
	    thunk
	    (lambda () (exchange old))))))

(let ((cntr 100))
  (defun tmpnam ()
    (incf cntr)
    (format nil "slib_~A" cntr)))

(defsynonymfun file-exists? #'cl:probe-file)

;;;; Input/Output

(defun open-file (filename modes)
  (case modes
    ((:r)
     (open filename
           :direction :input))
    ((:rb)
     (open filename
           :direction :input
           :element-type '(unsigned-byte 8)))
    ((:w)
     (open filename
           :direction :output))
    ((:wb)
     (open filename
           :direction :output
           :element-type '(unsigned-byte 8)))
    (otherwise
     (|SLIB:ERROR| 'open-file 'mode? modes))))

(defsynonymfun port? #'cl:streamp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defsynonymfun close-port #'cl:close))

(defun call-with-open-ports (&rest ports)
  (let ((proc (car ports)))
    (cond ((functionp proc) (setq ports (cdr ports)))
          (:else (setq ports (reverse ports))
                 (setq proc (car ports))
                 (setq ports (reverse (cdr ports))) ))
    (let ((ans (apply proc ports)))
      (mapc #'close-port ports)
      ans )))

(defun current-error-port ()
  cl:*error-output*)

;;; - force-output

;;; - file-position

(defun output-port-width (&optional port)
  (declare (ignore port))
  79)

(defun output-port-height (&optional port)
  (declare (ignore port))
  24)

;;;; Defmacro

;;; - defmacro

;;; - gentemp

(defsynonymfun |DEFMACRO:EVAL| #'cl:eval)

(defsynonymfun |DEFMACRO:LOAD| #'cl:load)

;;; - macroexpand

(defun |DEFMACRO:EXPAND*| (e)
  #+sbcl (sb-cltl2:macroexpand-all e))

;;;; R5RS Macros

(defsynonymfun |MACRO:EXPAND| #'cl:macroexpand)

(defsynonymfun |MACRO:EVAL| #'cl:eval)

(defsynonymfun |MACRO:LOAD| #'cl:load)

;;;; System

(defsynonymfun |SLIB:LOAD-SOURCE| #'cl:load)

(defsynonymfun |SLIB:LOAD-COMPILED| #'cl:load)

(defsynonymfun |SLIB:LOAD| #'cl:load)

(defsynonymfun |SLIB:EVAL| #'cl:eval)

(defun |SLIB:EVAL-LOAD| (filename eval)
  (declare (ignore eval))
  (cl:load filename))

(defun |SLIB:WARN| (&rest args)
  (cl:warn "~{~A~^ ~}" args))

(defun |SLIB:ERROR| (&rest args)
  (cl:error "~{~A~^ ~}" args))

(defun |SLIB:EXIT| (&optional (status 0))
  #+sbcl (sb-ext:quit :unix-status status)
  #-sbcl nil
  )

(defun browse-url (url)
  #+sbcl
  (and (sb-ext:run-program "/usr/bin/firefox" (list url))
       T))

(defun getenv (name)
  (srfi-98:get-environment-variable name))

(defun system (command-string)
  (let ((args (ppcre:split "\\s" command-string)))
    #+sbcl
    (sb-ext:process-exit-code
     (sb-ext:run-program (car args) (cdr args)))))

(defun program-arguments ()
  #+sbcl sb-ext:*posix-argv*)

;;;; Miscellany

;;; - identity

(defconstant |SLIB:TAB| #\Tab)

(defconstant |SLIB:FORM-FEED| #\Page)

;;; Mutual Exclusion
(defun make-exchanger (obj)
  (lambda (rep) (let ((old obj)) (setq obj rep) old)))

;;; Legacy

;;; - t

;;; - nil

(defsynonymfun last-pair #'cl:last)
