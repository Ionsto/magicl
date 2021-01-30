(defpackage #:magicl
  (:use #:common-lisp
        #:abstract-classes)
  (:import-from #:magicl.foreign-libraries
                #:print-availability-report)
  (:shadow #:vector
           #:=
           #:map
           #:trace
           #:every
           #:some
           #:notevery
           #:notany)
  (:export #:with-blapack
           
           ;; abstract-tensor protocol
           #:specialize-tensor
           #:generalize-tensor
           #:shape
           #:layout
           #:copy-tensor
           #:deep-copy-tensor
           #:cast
           #:tref
           #:order
           #:size
           #:element-type
           #:lisp-array

           #:every
           #:some
           #:notevery
           #:notany

           #:map
           #:map!

           #:reshape
           #:slice
           
           ;; Classes
           #:tensor
           #:matrix

           ;; Accessors
           #:nrows
           #:ncols

           ;; Subtypes
           #:tensor/single-float
           #:tensor/double-float
           #:tensor/complex-single-float
           #:tensor/complex-double-float
           #:matrix/single-float
           #:matrix/double-float
           #:matrix/complex-single-float
           #:matrix/complex-double-float
           #:vector/single-float
           #:vector/double-float
           #:vector/complex-single-float
           #:vector/complex-double-float

           ;; Constructors
           #:make-tensor
           #:empty
           #:const
           #:rand
           #:eye
           #:arange
           #:from-array
           #:from-list
           #:from-diag
           #:zeros
           #:ones

           #:random-unitary

           ;; Operators
           #:binary-operator
           #:.+
           #:.-
           #:.*
           #:./
           #:.^
           #:=
           #:map
           
           ;; Matrix operators
           #:square-matrix-p
           #:identity-matrix-p
           #:unitary-matrix-p
           #:hermitian-matrix-p
           #:row
           #:column
           #:@
           #:mult
           #:kron
           #:scale
           #:scale!
           #:diag
           #:det
           #:upper-triangular
           #:lower-triangular
           #:triu
           #:tril
           #:transpose
           #:transpose!
           #:orthonormalize
           #:orthonormalize!
           #:trace
           #:direct-sum
           #:conjugate-transpose
           #:conjugate-transpose!
           #:dagger
           #:dagger!
           #:eig
           #:hermitian-eig
           #:inv
           #:lu
           #:csd
           #:svd
           #:ql
           #:qr
           #:rq
           #:lq
           #:expm
           #:logm
           
           #:polynomial
           #:make-polynomial
           #:polynomial-coefficients
           #:polynomial-solve
           #:polynomial-eval
           #:polynomial-diff
           #:polynomial-newton-iteration

           ;; Vector operators
           #:dot
           #:norm))
