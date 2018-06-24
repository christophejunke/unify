(defpackage :unify
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
   #:make-var
   #:value
   #:name

   ;; Terms
   
   #:unify

   #:var
   #:nonvar
   #:ground

   #:make-copy
   
   #:copy-term
   #:deep-copy-term
   #:shallow-copy

   #:term-information
   #:term-variables

   ;; Internals exported for extensions
   
   #:logical-variable
   #:collapsable-logical-variable

   #:*logical-variable-class*
   #:*occur-check*
   #:*print-details*

   #:%logical-variable-boundp
   #:%make-logical-variable-unbound
   #:%set-logical-variable-value
   
   ;; Errors
   
   #:unification-error
   #:unification-error/first-term
   #:unification-error/second-term

   #:occurrence-error 
   #:occurrence-error/var
   #:occurrence-error/term))

