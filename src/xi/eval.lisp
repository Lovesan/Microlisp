(in-package #:microlisp)

(enable-xi-syntax)

(define-condition lisp-runtime-error (error)
  ((message :initarg :message
            :reader lisp-runtime-error-message))
  (:default-initargs :message nil)
  (:report (lambda (condition stream)
             (format stream "Lisp runtime error~:[.~; ~:*~a~]"
                     (lisp-runtime-error-message condition))
             condition)))

(defun lisp-runtime-error (message &rest args)
  (error 'lisp-runtime-error
         :message (apply #'format nil message args)))

(defvar *dynamic-vars* '())
(defvar *catch-tags* '())
(defvar +global-var-info+ (if (boundp '+global-var-info+)
                            (symbol-value '+global-var-info+)
                            (make-hash-table :test #'eq)))

(defstruct (env (:constructor %make-env))
  vars
  tags
  blocks
  cenv) ;; cenv is environment for macros and constants

(defun make-env (&key parent vars tags blocks)
  (flet ((ct-vars (vars)
           (remove-if
            (lambda (var &aux (kind (cadr var)))
              (not (or (eq kind :macro)
                       (eq kind :constant))))
            vars)))
    (if (null parent)
      (%make-env :vars vars
                 :tags tags
                 :blocks blocks
                 :cenv (%make-env :vars (ct-vars vars)))
      (let ((parent-vars (env-vars parent)))
        (%make-env :vars (append vars parent-vars)
                   :tags (append tags (env-tags parent))
                   :blocks (append blocks (env-blocks parent))
                   :cenv (%make-env
                          :vars (append (ct-vars vars)
                                        (env-vars (env-cenv parent)))))))))

(defun env-var (env symbol)
  (find symbol (env-vars env)
        :key #'car :test #'eq))

(defun env-tag (env symbol)
  (find symbol (env-tags env)
        :key #'car :test #'eq))

(defun env-block (env symbol)
  (find symbol (env-blocks env)
        :key #'car :test #'eq))

(defun error-unbound-variable (symbol)
  (eval-call [signal-error]
             (list (sym-value [unbound-variable]) symbol)))

(defun error-undeclared-variable (symbol)
  (eval-call [signal-error]
             (list (sym-value [undeclared-variable]) symbol)))

(defun error-constant-modification (symbol)
  (eval-call [signal-error]
             (list (sym-value [constant-modification]) symbol)))

(defun error-arg-count (args)
  (eval-call [signal-error]
             (list (sym-value [invalid-argument-count]) args)))

(defun error-not-symbol (datum)
  (eval-call [signal-error]
             (list (sym-value [class-error])
                   datum
                   (sym-value [symbol]))))

(defun get-symbol-value (symbol)
  (let ((dynvar (find symbol *dynamic-vars*
                      :key #'car)))
    (if (not dynvar)
      (case (gethash symbol +global-var-info+)
        ((:static :constant :macro :dynamic)
         (if (eq (sym-value symbol) :unbound)
           (error-unbound-variable symbol)
           (sym-value symbol)))
        (T (error-undeclared-variable symbol)))
      (if (eq :unbound (cdr dynvar))
        (error-unbound-variable symbol)
        (cdr dynvar)))))

(defun set-symbol-value (new-value symbol)
  (let ((dynvar (find symbol *dynamic-vars*
                      :key #'car)))
    (if (not dynvar)
      (case (gethash symbol +global-var-info+)
        ((:static :dynamic)
         (setf (sym-value symbol) new-value))
        (:macro (setf (sym-value symbol)
                      (if (function-object-p new-value)
                        new-value
                        (alloc-fun
                          :args (list [%form] [%env])
                          :code (constantly new-value)))))
        (:constant (error-constant-modification symbol))
        (T (error-undeclared-variable symbol)))
      (setf (cdr dynvar) new-value))))

(defun eval-ref (symbol env)
  (let ((var (env-var env symbol)))
    (if (null var)
      (case (gethash symbol +global-var-info+)
        (:dynamic (get-symbol-value symbol))
        ((:static :constant)
         (if (eq :unbound (sym-value symbol))
           (error-unbound-variable symbol)
           (sym-value symbol)))
        (:macro (if (eq :unbound (sym-value symbol))
                  (error-unbound-variable symbol)
                  (eval-form (eval-macroexpand (sym-value symbol) symbol)
                             env)))
        (T (error-undeclared-variable symbol)))
      (ecase (second var)
        (:dynamic (get-symbol-value symbol))   
        (:macro (eval-form (eval-macroexpand (cddr var) symbol)
                           env))
        ((:static :constant) (cddr var))))))

(defun eval-setq (form env)
  (if (= (length form) 3)
    (if (symbol-object-p (second form))
      (let* ((symbol (second form))
             (value (third form))
             (var (env-var env symbol)))
        (if (null var)
          (case (gethash symbol +global-var-info+)
            (:dynamic (set-symbol-value symbol (eval-form value env)))
            (:static (setf (sym-value symbol) (eval-form value env)))
            (:constant (error-constant-modification symbol))
            (:macro (eval-form `([setf] ,symbol ,value) env))
            (T (error-undeclared-variable symbol)))
          (ecase (second var)
            (:dynamic (set-symbol-value symbol (eval-form value env)))
            (:static (setf (cddr var) (eval-form value env)))
            (:constant (error-constant-modification symbol))
            (:macro (eval-form `([setf] ,symbol ,value) env)))))
      (error-not-symbol (second form)))
    (error-arg-count (rest form))))

;; we pass `nil' here because XI lexenv structure
;;  is not suitable for target lisp.
;; TODO: make lexenv a lisp object. 
(defun eval-macroexpand (expander form)
  (let ((*dynamics* '()))
    (eval-call expander (list form nil))))

;; Microlisp set of special operators is very similiar to that
;;  of Common Lisp, so in case of XI we reuse existing CL control structures
;;  and specifically, an implementation of NLX an dynamic environments.
;; Upcoming Microlisp runtime and compiler should implement all
;;   this stuff from scratch.

(disable-xi-syntax)
