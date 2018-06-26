(defpackage :unification.internals
  (:use :cl)
  (:documentation "Internals exported for extension")
  (:export #:logical-variable
           #:collapsible-logical-variable

           #:*logical-variable-class*
           #:*occur-check*
           #:*print-details*

           #:with-raw-variables
           #:%logical-variable-bound-p
           #:%make-logical-variable-unbound

           #:%get-logical-variable-value
           #:%set-logical-variable-value
           #:%%value

           #:%first-term
           #:%second-term
           #:%var
           #:%term))

(defpackage :unification
  (:use :cl :unification.internals)
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



(in-package :unification)
(declaim (inline unify-var-term
                 value
                 unify%
                 (setf value)
                 %logical-variable-boundp))
