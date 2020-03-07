;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-96"
  (:use)
  (:export
   software-type scheme-implementation-type
   scheme-implementation-version
   scheme-implementation-home-page scheme-file-suffix slib$features
   with-load-pathname tmpnam file-exists? delete-fileopen-file port
   close-port call-with-open-ports call-with-open-ports
   current-error-port
   force-output force-output file-position file-position
   output-port-width
   output-port-width output-port-height output-port-height defmacro
   most-positive-fixnum
   char-code-limit
   gentemp defmacro$eval defmacro$load macroexpand defmacro$expand*
   macro$expand macro$eval macro$loadslib$load-source
   slib$load-compiled slib$load slib$eval slib$eval-load
   slib$warn slib$error slib$exit getenv system
   program-arguments slib$tab slib$form-feedmake-exchangert
   identity
   t nil last-pair)
  (:import-from cl
                identity
                delete-file
                t
                nil
                most-positive-fixnum
                char-code-limit
                software-type
                macroexpand
                gentemp
                defmacro
                force-output
                file-position
                ))


(defpackage "https://github.com/g000001/srfi-96#internals"
  (:use
   "https://github.com/g000001/srfi-96"
   "https://github.com/g000001/srfi-98"
   cl
   ppcre))


;;; *EOF*
