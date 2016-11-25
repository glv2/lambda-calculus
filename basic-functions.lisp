;;;; This file is part of lambda-calculus, an interpreter for the
;;;; lambda calculus.
;;;;
;;;; Copyright 2016 Guillaume LE VAILLANT
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;;
;;;; This file contains the Church encodings of basic functions.


;;; Logic

(defvar true. '(lambda a (lambda b a)))
(defvar false. '(lambda a (lambda b b)))

;; if P then A else B
(defvar if. '(lambda p (lambda a (lambda b ((p a) b)))))
;; P and Q
(defvar and. `(lambda p (lambda q ((p q) ,false.))))
;; P or Q
(defvar or. `(lambda p (lambda q ((p ,true.) q))))
;; not P
(defvar not. `(lambda p (lambda a (lambda b ((p b) a)))))


;;; Arithmetic

(defvar zero. '(lambda f (lambda x x)))
(defvar one. '(lambda f (lambda x (f x))))
(defvar two. '(lambda f (lambda x (f (f x)))))
(defvar three. '(lambda f (lambda x (f (f (f x))))))
(defvar four. '(lambda f (lambda x (f (f (f (f x)))))))
(defvar five. '(lambda f (lambda x (f (f (f (f (f x))))))))
(defvar six. '(lambda f (lambda x (f (f (f (f (f (f x)))))))))
(defvar seven. '(lambda f (lambda x (f (f (f (f (f (f (f x))))))))))
(defvar eight. '(lambda f (lambda x (f (f (f (f (f (f (f (f x)))))))))))
(defvar nine. '(lambda f (lambda x (f (f (f (f (f (f (f (f (f x))))))))))))
(defvar ten. '(lambda f (lambda x (f (f (f (f (f (f (f (f (f (f x)))))))))))))

;; N + 1
(defvar inc. '(lambda n (lambda f (lambda x (f ((n f) x))))))
;; N + M
(defvar add. '(lambda n (lambda m (lambda f (lambda x ((n f) ((m f) x)))))))
;; N * M
(defvar mul. '(lambda n (lambda m (lambda f (n (m f))))))
;; B^E
(defvar pow. '(lambda b (lambda e (e b))))
;; N - 1
(defvar dec. '(lambda n
               (lambda f
                 (lambda x
                   (((n (lambda g (lambda h (h (g f))))) (lambda u x))
                    (lambda u u))))))
;; M - N
(defvar sub. `(lambda m (lambda n ((n ,dec.) m))))

(defvar hundred. `((,mul. ,ten.) ,ten.))
(defvar thousand. `((,mul. ,ten.) ,hundred.))

;; N = 0
(defvar zerop. `(lambda n ((n (lambda x ,false.)) ,true.)))
;; M <= N
(defvar leq. `(lambda m (lambda n (,zerop. ((,sub. m) n)))))
;; M = N
(defvar eq. `(lambda m (lambda n ((,and. ((,leq. m) n)) ((,leq. n) m)))))


;;; Pairs

(defvar cons. '(lambda x (lambda y (lambda f ((f x) y)))))
(defvar car. `(lambda p (p ,true.)))
(defvar cdr. `(lambda p (p ,false.)))
(defvar nil. `(lambda x ,true.))
(defvar null. `(lambda p (p (lambda x (lambda y ,false.)))))


;;; Recursion using the Y combinator

(defvar Y. '(lambda f ((lambda x (f (x x))) (lambda x (f (x x))))))

;; factorial
(defvar %fac `(lambda f
                (lambda n
                  (((,zerop. n) ,one.) ((,mul. n) (f (,dec. n)))))))
(defvar fac. `(,Y. ,%fac))

;; fibonacci
(defvar %fib `(lambda f
                (lambda n
                  (((,if. ((,leq. n) ,one.))
                    ,one.)
                   ((,add. (f (,dec. n))) (f (,dec. (,dec. n))))))))
(defvar fib. `(,Y. ,%fib))
