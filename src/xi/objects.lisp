(in-package #:microlisp)

(deftype index () '(integer 0 #.(1- array-total-size-limit)))

(defstruct (symbol-object
            (:constructor alloc-sym)
            (:conc-name sym-))
  name
  (value :unbound)
  namespace
  assembly)

(defstruct (function-object
            (:constructor alloc-fun)
            (:conc-name fun-))
  name
  args
  env
  code)

(defstruct (instance-object
            (:constructor alloc-inst)
            (:conc-name inst-))
  function
  class
  slots)

(defmacro inst-ref (instance i)
  `(svref (inst-slots ,instance) ,i))

(defmacro defaccessors (&rest names)
  `(progn ,@(loop :with inst = (gensym)
                  :for name :in names
                  :for i :from 0
                  :collect `(defmacro ,name (,inst)
                              (list 'inst-ref ,inst ,i)))))
