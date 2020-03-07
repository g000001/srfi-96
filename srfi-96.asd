;;;; srfi-96.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-96
  :version "20200308"
  :description "SRFI 96: SLIB Prerequisites"
  :long-description "SRFI 96: SLIB Prerequisites
https://srfi.schemers.org/srfi-96"
  :author "CHIBA Masaomi"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (#+sbcl :sb-cltl2
               :srfi-59
               :srfi-98
               :cl-ppcre)
  :components ((:file "package")
               (:file "srfi-96")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-96))))
  (let ((name "https://github.com/g000001/srfi-96")
        (nickname :srfi-96))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


;;; *EOF*
