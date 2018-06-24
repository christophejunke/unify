(defpackage :nondet (:use :cl :alexandria :unify))
(in-package :nondet)

(define-condition fail (error) ())
(define-condition done () ())

(defun next ()
  "End of evaluation"
  (signal 'done))

(defun fail ()
  (error 'fail))

(defvar *next* #'next
  "Continuation.")

(defun get-undo-expansion (head args)
  "Get form associated with HEAD to undo the effects of (HEAD . ARGS)."
  (let ((undo (get head 'undo)))
    (and (functionp undo) (apply undo args))))

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

(defmacro chain-expression (form)
  (flet ((chain (form) `(progn ,form (funcall *next*))))
    (typecase form
      (atom (chain form))
      (cons (destructuring-bind (head . rest) form
              (let ((undo-expansion (get-undo-expansion head rest)))
                (if undo-expansion
                    `(unwind-protect ,(chain form)
                       ,undo-expansion)
                    (chain form))))))))

;;;; ALTERNATION / SEQUENCE

(define-condition cut () ())

(defun cut () (signal 'cut))

(defmacro alt1 (form1 form2)
  (if form2
      (with-gensyms (active-redo-p condition)
        `(let ((,active-redo-p t))
           ;; cuts too much?
           (handler-bind ((cut (lambda (,condition)
                                 (declare (ignore ,condition))
                                 (when ,active-redo-p
                                   (setf ,active-redo-p nil)))))
             (restart-case (chain-expression ,form1)
               (redo ()
                 :report ,(format nil "Try alternative: ~a" form2)
                 :test (lambda (,condition)
                         (declare (ignore ,condition))
                         ,active-redo-p)
                 (chain-expression ,form2))))))
      form1))

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
           (chain-expression ,form1)))
      `(chain-expression ,form1)))

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

;; Not really, but ok for now
(define-undo-expansion $unify (a b)
  (declare (ignore b))
  `(unify::%make-logical-variable-unbound ,a))

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

(let ((result))
  (with-logical-variables (?a ?b)
    (with-backtracking ()
      ($member ?a '(1 2 3))
      ($member ?b '(a b))
      (push (cons ?a ?b) result)
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

(with-logical-variables (?a ?res)
  (with-backtracking ()
    ($findall ?a
              (lambda ()
                (seq
                  ($member ?a '(1 2 3))
                  (is (oddp ?a))))
              ?res))
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
    (unify::with-logical-variables (?a)
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
