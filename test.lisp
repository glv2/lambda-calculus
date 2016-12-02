(load "lambda-calculus")
(load "basic-functions")

(defun check (term result)
  (or (lambda-calculus:are-alpha-equivalent
       (lambda-calculus:beta-reduce term)
       (lambda-calculus:beta-reduce result))
      (error "Terms are not equivalent:~%term 1 = ~s~%term 2 = ~s~%"
             term
             result)))

(defun check-not (term result)
  (or (not (lambda-calculus:are-alpha-equivalent
            (lambda-calculus:beta-reduce term)
            (lambda-calculus:beta-reduce result)))
      (error "Terms are equivalent:~%term 1 = ~s~%term 2 = ~s~%"
             term
             result)))

(progn
  (check 'x 'y)
  (check '(x y) '(a b))
  (check-not '(x y) '(x x))
  (check '(x x) '(a a))
  (check '(x (y x)) '(a (b a)))
  (check-not '(x (y x)) '(x (x y)))
  (check '(lambda x (y z)) '(lambda a (b c)))
  (check-not '(lambda x (y z)) '(lambda a (b b)))
  (check `(,not. (,zerop. ,zero.)) false.)
  (check `((,eq. (,inc. ,one.)) ,two.) true.)
  (check `(,zerop. (,dec. ,one.)) true.)
  (check `(((,if. ,true.) ,one.) ,two.) one.)
  (check `(((,if. ,false.) ,one.) ,two.) two.)
  (check `((,leq. ,five.) ,one.) false.)
  (check `(,fac. (,inc. ,two.)) `((,add. ,two.) ((,mul. ,two.) ,two.)))
  (check `((,eq. ((,pow. ,two.) ,three.)) ,eight.) true.)
  (check `((,eq. (,fib. ,five.)) ,eight.) true.)
  (format t "All tests OK~%"))
