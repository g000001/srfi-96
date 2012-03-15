;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-96
  (:use)
  (:export
   :software-type :scheme-implementation-type :scheme-implementation-version
   :scheme-implementation-home-page :scheme-file-suffix :|SLIB:FEATURES|
   :with-load-pathname :tmpnam :file-exists? :delete-fileopen-file :port
   :close-port :call-with-open-ports :call-with-open-ports :current-error-port
   :force-output :force-output :file-position :file-position :output-port-width
   :output-port-width :output-port-height :output-port-height :defmacro
   :most-positive-fixnum
   :char-code-limit
   :gentemp :|defmacro:eval| :|defmacro:load| :macroexpand :|DEFMACRO:EXPAND*|
   :|MACRO:EXPAND| :|MACRO:EVAL| :|MACRO:LOADSLIB:LOAD-SOURCE|
   :|SLIB:LOAD-COMPILED| :|SLIB:LOAD| :|SLIB:EVAL| :|SLIB:EVAL-LOAD|
   :|slib:warn| :|slib:error| :|slib:exit| :getenv :system
   :program-arguments :|SLIB:TAB| :|SLIB:FORM-FEEDMAKE-EXCHANGERT|
   :identity
   :t :nil :last-pair)
  (:import-from :cl
                :identity
                :delete-file
                :t
                :nil
                :most-positive-fixnum
                :char-code-limit
                :software-type
                :macroexpand
                :gentemp
                :defmacro
                :force-output
                :file-position
                ))

(defpackage :srfi-96.internal
  (:use :srfi-96 :cl :fiveam))

;;; eof
