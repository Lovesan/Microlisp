(in-package #:microlisp)

(enable-xi-syntax)

(defmacro defcinst (name)
  `(defconstant ,name (if (boundp ',name)
                        (symbol-value ',name)
                        (alloc-inst))))

(defcinst +standard-class+)

(disable-xi-syntax)