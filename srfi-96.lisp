;;;; srfi-96.lisp

(cl:in-package "https://github.com/g000001/srfi-96#internals")


(cl:defmacro defsynonymfun (name fcn)
  `(cl:setf (cl:fdefinition ',name) ,fcn) )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dynamic-wind (in body out)
    (declare (function in body out))
    (funcall in)
    (unwind-protect (funcall body)
      (funcall out) )))


;;; configuration

;;; - software-type

(defun scheme-implementation-type ()
  (cl:lisp-implementation-type) )


(defun scheme-implementation-version ()
  (cl:lisp-implementation-version) )


(defun scheme-implementation-home-page ()
  #+sbcl "http://www.sbcl.org/"
  #+lispworks "http://www.lispworks.com/"
  #-(or sbcl lispworks) nil
  )


(defun scheme-file-suffix ()
  #+(or sbcl lispworks) ".lisp"
  #-(or sbcl) nil
  )


(define-symbol-macro slib$features cl:*features*)


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
     (slib$error 'open-file 'mode? modes))))


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

(defsynonymfun defmacro$eval #'cl:eval)


(defsynonymfun defmacro$load #'cl:load)


;;; - macroexpand

(defun defmacro$expand* (e)
  #+sbcl (sb-cltl2:macroexpand-all e)
  #+lispworks (walker:walk-form e))


;;;; R5RS Macros

(defsynonymfun macro$expand #'cl:macroexpand)


(defsynonymfun macro$eval #'cl:eval)


(defsynonymfun macro$load #'cl:load)


;;;; System

(defsynonymfun slib$load-source #'cl:load)


(defsynonymfun slib$load-compiled #'cl:load)


(defsynonymfun slib$load #'cl:load)


(defsynonymfun slib$eval #'cl:eval)


(defun slib$eval-load (filename eval)
  (declare (ignore eval))
  (cl:load filename))


(defun slib$warn (&rest args)
  (cl:warn "~{~A~^ ~}" args))


(defun slib$error (&rest args)
  (cl:error "~{~A~^ ~}" args))


(defun slib$exit (&optional (status 0))
  #+sbcl (sb-ext:quit :unix-status status)
  #+lispworks (lw:quit :status status)
  #-(or sbcl lispworks) nil
  )


(defun browse-url (url)
  #+sbcl
  (and (sb-ext:run-program "/usr/bin/firefox" (list url))
       T)
  #+lispworks
  (and (sys:run-shell-command (format nil "/usr/bin/firefox ~A" url))
       T))


(defun getenv (name)
  (get-environment-variable name))


(defun system (command-string)
  (let ((args (split "\\s" command-string)))
    #+sbcl
    (sb-ext:process-exit-code
     (sb-ext:run-program (car args) (cdr args)))
    #+lispworks (sys:run-shell-command command-string)))


(defun program-arguments ()
  #+sbcl sb-ext:*posix-argv*
  #+lispworks sys:*line-arguments-list*)


;;;; Miscellany

;;; - identity

(defconstant slib$tab #\Tab)


(defconstant slib$form-feed #\Page)


;;; Mutual Exclusion
(defun make-exchanger (obj)
  (lambda (rep) (let ((old obj)) (setq obj rep) old)))


;;; Legacy

;;; - t

;;; - nil

(defsynonymfun last-pair #'cl:last)


;;; *EOF*
