;;;; srfi-96.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-96
  :serial t
  :depends-on (:fiveam
               :srfi-59
               :srfi-98
               :cl-ppcre)
  :components ((:file "package")
               (:file "srfi-96")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-96))))
  (load-system :srfi-96)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-96.internal :srfi-96))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
