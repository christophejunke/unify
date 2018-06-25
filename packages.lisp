(defpackage :unification
  (:use :cl)
  (:documentation
   "Logical variables and unification.")
  (:import-from :alexandria
                #:hash-table-keys
                #:hash-table-alist 
                #:ensure-list
                #:with-gensyms)
  (:export

   ;; Variables
   
   #:with-logical-variables
   #:var
   #:name

   ;; Terms
   
   #:value
   #:unify
   #:make-copy
   #:copy-term
   #:term-information
   #:term-variables

   ;; Errors
   
   #:unification-error
   #:occurrence-error))

(defpackage :unification.internals
  (:use :cl :unification)
  (:documentation "Internals exported for extension")
  (:export #:logical-variable
           #:collapsible-logical-variable

           #:*logical-variable-class*
           #:*occur-check*
           #:*print-details*

           #:%logical-variable-boundp
           #:%make-logical-variable-unbound
           #:%set-logical-variable-value

           #:%first-term
           #:%second-term
           #:%var
           #:%term))

(in-package :unification)
(declaim (inline unify-var-term
                 value
                 unify%
                 (setf value)
                 %logical-variable-boundp))
