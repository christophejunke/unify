(defpackage :nondet
  (:use
   :cl
   :alexandria
   :unification
   :unification.internals))
(in-package :nondet)

(define-condition fail (error) ())
(defun fail ()
  (error 'fail))

(define-condition done () ())
(defun next ()
  "End of evaluation"
  (signal 'done))

(defvar *next* #'next
  "Continuation.")

(defun get-undo-expansion (form)
  ;; TODO reword
  "Get form associated with HEAD to undo the effects of (HEAD . ARGS)."
  (typecase form
    (cons (let ((undo (get (car form) 'undo)))
            (and (functionp undo) (apply undo (cdr form)))))))

(defmacro define-undo-expansion (name (&rest args) &body body)
  "Define an UNDO expansion for (NAME . ARGS)"
  `(setf (get ',name 'undo) (lambda ,args ,@body)))

(defmacro with-backtracking ((&rest options) &body body)
  (declare (ignore options))
  (with-gensyms (block-name condition restart)
    `(block ,block-name
       (handler-bind
           ((fail (lambda (,condition)
                    (let ((,restart (find-restart 'redo ,condition)))
                      (return-from ,block-name
                        (and ,restart (invoke-restart ,restart))))))
            (done (lambda (,condition)
                    (return-from ,block-name
                      (values t (and (find-restart 'redo ,condition) t))))))
         (seq ,@body)))))

;;;; ALTERNATION / SEQUENCE

(define-condition cut () ())
(defun cut () (signal 'cut))

;; (defmacro alt1 (form1 form2)
;;   (if form2
;;       (with-gensyms (active-redo-p condition)
;;         `(let ((,active-redo-p t))
;;            (handler-bind ((cut (lambda (,condition)
;;                                  (declare (ignore ,condition))
;;                                  (setf ,active-redo-p nil))))
;;              (restart-case ,(chain-expression form1 active-redo-p)
;;                (redo ()
;;                  :report ,(format nil "Try alternative: ~a" form2)
;;                  :test (lambda (,condition)
;;                          (declare (ignore ,condition))
;;                          ,active-redo-p)
;;                  (chain-expression ,form2))))))
;;       form1))

(defun prognext (form)
  `(progn ,form (funcall *next*)))

(defun backtracking-form (form forward backward)
  (cond
    (forward (if backward
                 `(unwind-protect ,(prognext forward)
                    ,backward)
                 (prognext forward)))
    (t (prognext form))))

(defun chain-expression (form)
  (multiple-value-bind (forward backward bindings)
      (get-undo-expansion form)
    (if forward
        `(let ,bindings ,(backtracking-form form forward backward))
        (prognext form))))

(defmacro with-cut-handler (action &body body)
  (with-gensyms (condition)
    `(handler-bind ((cut (lambda (,condition)
                           (declare (ignore ,condition))
                           ,@action)))
       ,@body)))

(defun test-fn (symbol)
  (with-gensyms (condition)
    `(lambda (,condition)
       (declare (ignore ,condition))
       ,symbol)))

(defmacro alt1 (form alternative)
  (flet ((prognext (form) `(progn ,form (funcall *next*))))
    (multiple-value-bind (forward backward bindings redo-symbol on-cut)
        (get-undo-expansion form)
      (let ((redo-symbol (or redo-symbol (gensym))))
        (cond
          (alternative
           `(let* ((,redo-symbol t) ,@bindings)
              (with-cut-handler ((setf ,redo-symbol nil) ,on-cut)
                (restart-case ,(backtracking-form form forward backward)
                  (redo ()
                    :report ,(format nil "Retry with ~a" alternative)
                    :test ,(test-fn redo-symbol)
                    ,@(rest (prognext alternative)))))))
          (forward `(let ((,redo-symbol t)
                          ,@bindings)
                      (with-cut-handler ((setf ,redo-symbol nil) ,on-cut)
                        ,(backtracking-form form forward backward))))
          (t form))))))

(defmacro seq1 (form1 form2)
  (if form2
      (let ((current (gensym)))
        ;; capture current *next* value
        `(let* ((,current *next*)
                (*next* (lambda ()
                          ;; bind back to the enclosing next. Dynamic
                          ;; scope helps when crossing function calls
                          ;; and for implementing cut.
                          (let ((*next* ,current))
                            ,form2))))
           ,(chain-expression form1)))
      (chain-expression form1)))

(defun weave-macro (macro body)
  (labels ((recurse (term)
             (typecase term
               (null nil)
               (cons (destructuring-bind (expr . body) term
                       (if body
                           `(,macro ,expr ,(recurse body))
                           `(,macro ,expr nil)))))))
    (recurse body)))

(defmacro seq (&body body) (weave-macro 'seq1 body))
(defmacro alt (&body body) (weave-macro 'alt1 body))

;;;;
;;;; TESTS
;;;;

(defun $unify (x y)
  (handler-case (unify x y)
    (unification-error () (fail))))

(defvar *undo-stack* nil)
(defun unify-collect (a b)
  (let ((*undo-stack* nil))
    (values (unify a b) *undo-stack*)))

(defclass can-undo () ())

(defmethod %set-logical-variable-value
    :before (value (variable can-undo))
  (push (if (%logical-variable-bound-p variable)
            (cons variable (%get-logical-variable-value variable))
            variable)
        *undo-stack*))

(defclass backtrackable-collapsible-logical-variable
    (can-undo collapsible-logical-variable)
  ())

(defclass backtrackable-logical-variable
    (can-undo logical-variable)
  ())

(defun apply-undo-list (list)
  (map ()
       (lambda (item)
         (etypecase item
           (cons (destructuring-bind (var . value) item
                   (setf (slot-value var '%%value) value)))
           (logical-variable (%make-logical-variable-unbound item))))
       list))

(let ((*logical-variable-class*
        'backtrackable-collapsible-logical-variable))
  (with-logical-variables (?a ?b ?c)
    (multiple-value-bind (res1 undo1) (unify-collect ?a ?b)
      (multiple-value-bind (res2 undo2) (unify-collect ?b ?c)
        (multiple-value-bind (res3 undo3) (unify-collect ?a 1)
          (let ((tmp
                  (list res1 undo1
                        res2 undo2
                        res3 undo3)))
            (apply-undo-list undo3)
            (apply-undo-list undo2)
            (apply-undo-list undo1)
            tmp))))))

(define-undo-expansion $unify (a b)
  (let ((ga (gensym))
        (gb (gensym))
        (gundo (copy-symbol :undo)))
    (values `(let ((*undo-stack* nil))
               (prog1 ($unify ,ga ,gb)
                 (setf ,gundo *undo-stack*)))
            `(apply-undo-list ,gundo)
            `((,ga ,a)
              (,gb ,b)
              (,gundo))
            nil
            `(setf ,gundo nil))))

(defun $member (term list)
  (typecase list
    (null (fail))
    (cons (destructuring-bind (head . tail) list
            (alt
              ($unify term head)
              ($member term tail))))))

(defun $findall (var goal out)
  (let ((tmp (list)))
    (alt
      (seq
        (funcall goal)
        (push (value var) tmp)
        (fail))
      ($unify out (nreverse tmp)))))

(setf *logical-variable-class*
      (find-class 'backtrackable-collapsible-logical-variable))

(let ((result))
  (with-logical-variables (?a ?b)
    (with-backtracking ()
      ($member ?a '(1 2 3))
      ($member ?b '(a b))
      (push (cons ?a ?b) result)
      (fail)))
  result)

(let ((result))
  (with-logical-variables (?a)
    (with-backtracking ()
      (alt
        ($unify ?a 1)
        ($unify ?a 2))
      (push ?a result)
      (fail)))
  result)

(let ((result))
  (with-logical-variables (?a ?b)
    (with-backtracking ()
      ($member ?a '(1 2 3))
      (cut)
      ($member ?b '(a b))
      (push (cons ?a ?b) result)
      (fail)))
  result)

(let ((result))
  (with-logical-variables (?a ?b)
    (with-backtracking ()
      ($member ?a '(1 2 3))
      ($member ?b '(a b))
      (cut)
      (push (cons ?a ?b) result)
      (fail)))
  result)

(defmacro is (expr)
  `(or ,expr (fail)))

(let ((result))
  (with-logical-variables (?a ?res)
    (with-backtracking ()
      ($findall ?a
                (lambda ()
                  (seq
                    ($member ?a '(1 2 3))
                    (is (oddp ?a))))
                ?res)
      (setf result ?res))
    result))

(with-logical-variables (?a ?res)
  (with-backtracking ()
    ($findall ?a
              (lambda ()
                (seq
                  ($member ?a '(1 2 3))
                  (is (oddp ?a))))
              ?res)
    (cut))
  ?res)


;; BROKEN
;; (with-logical-variables (?a ?b)
;;   (with-backtracking ()
;;     (alt
;;       ($unify `(,?a (1 2 3)) `(0 (1 2 4)))
;;       ($unify `(,?a (1 2 3)) `(9 (1 2 3))))))

(defun test ()
  "Loop over all choices with fail, then succeed."
  (with-backtracking ()
    (alt (seq (alt (print 1) (print 2) (print 3))
              (alt (print :a) (print :b) (print :c))
              (fail))
         t)))

(defun unif-test ()
  (with-backtracking ()
    (with-logical-variables (?a)
      (seq
        ($member ?a '(1 3 2 3))
        ($unify ?a 2)
        (print ?a)))))

(let ((result))
  (with-logical-variables (?a ?b)
    (with-backtracking ()
      ($member ?a '(1 2 3))
      ($member ?b '(a b))
      (push (cons ?a ?b) result)
      (fail)))
  result)

