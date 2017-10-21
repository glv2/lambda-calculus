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
;;;; Based on the paper by Luigia AIELLO and Gianfranco PRINI entitled
;;;; "An efficient interpreter for the lambda-calculus".
;;;;
;;;;
;;;; Format of lambda terms:
;;;;   A             where A is a symbol (alpha characters)
;;;;   (B C)         where B and C are lambda terms
;;;;   (lambda D E)  where D is a symbol and E is a lambda term
;;;;
;;;; Examples:
;;;;  (lambda x x)
;;;;  (lambda x (lambda y (y x)))
;;;;  ((lambda x (x x)) (lambda y (lambda z y)))


(defpackage lambda-calculus
  (:use cl)
  (:export are-alpha-equivalent
           beta-reduce))

(in-package lambda-calculus)


;;; Slow interpreter

(defun lengthp (list n)
  (and (listp list)
       (= (length list) n)))

(defun is-variable (term)
  (and (symbolp term)
       (not (eq term 'lambda))))

(defun is-application (term)
  (lengthp term 2))

(defun is-lambda-abstraction (term)
  (and (lengthp term 3)
       (eq (car term) 'lambda)
       (is-variable (cadr term))))

(defun is-lambda-term (term)
  (or (is-variable term)
      (is-application term)
      (is-lambda-abstraction term)))

(defun mk-variable (name)
  name)

(defun mk-application (function argument)
  (list function argument))

(defun mk-lambda-abstraction (binder form)
  (list 'lambda binder form))

(defun name-of (term)
  term)

(defun function-of (term)
  (car term))

(defun argument-of (term)
  (cadr term))

(defun binder-of (term)
  (cadr term))

(defun form-of (term)
  (caddr term))

(defun free-variables (term)
  (cond
    ((is-variable term) (list term))
    ((is-application term) (union (free-variables (function-of term))
                                  (free-variables (argument-of term))))
    ((is-lambda-abstraction term) (remove (binder-of term)
                                          (free-variables (form-of term))))
    (t (error "~s is not a lambda term" term))))

(defun is-free-in (variable term)
  (member variable (free-variables term)))

(defun generate-new-variable-free-in (term)
  (declare (ignore term))
  (mk-variable (gensym)))

(defun free-substitute (term variable host-term)
  (cond
    ((is-variable host-term)
     (if (eq (name-of host-term) (name-of variable))
         term
         host-term))

    ((is-application host-term)
     (mk-application (free-substitute term variable (function-of host-term))
                     (free-substitute term variable (argument-of host-term))))

    ((is-lambda-abstraction host-term)
     (cond
       ((eq (name-of (binder-of host-term)) (name-of variable))
        host-term)

       ((not (is-free-in (binder-of host-term) term))
        (mk-lambda-abstraction (binder-of host-term)
                               (free-substitute term
                                                variable
                                                (form-of host-term))))
       (t
        (let ((new-variable (generate-new-variable-free-in
                             (mk-application term (form-of host-term)))))
          (mk-lambda-abstraction new-variable
                                 (free-substitute
                                  term
                                  variable
                                  (free-substitute
                                   new-variable
                                   (binder-of host-term)
                                   (form-of host-term))))))))

    (t (error "~s is not a lambda term" term))))

(defun rtnf-slow (term)
  (cond
    ((is-variable term) term)
    ((is-application term) (let ((redfun (rtlf-slow (function-of term)))
                                 (arg (argument-of term)))
                             (if (is-lambda-abstraction redfun)
                                 (rtnf-slow (free-substitute arg
                                                             (binder-of redfun)
                                                             (form-of redfun)))
                                 (mk-application redfun (rtnf-slow arg)))))
    ((is-lambda-abstraction term) (mk-lambda-abstraction (binder-of term)
                                                         (rtnf-slow
                                                          (form-of term))))
    (t (error "~s is not a lambda term" term))))

(defun rtlf-slow (term)
  (cond
    ((is-variable term) term)
    ((is-application term) (let ((redfun (rtlf-slow (function-of term)))
                                 (arg (argument-of term)))
                             (if (is-lambda-abstraction redfun)
                                 (rtlf-slow (free-substitute arg
                                                             (binder-of redfun)
                                                             (form-of redfun)))
                                 (mk-application redfun (rtnf-slow arg)))))
    ((is-lambda-abstraction term) term)
    (t (error "~s is not a lambda term" term))))

(defun reduce-to-normal-form-slow (term)
  (rtnf-slow term))

(defun are-alpha-equivalent (term-1 term-2)
  "Check if TERM-1 AND TERM-2 are equivalent lambda terms with regard to
alpha-conversion."
  (labels ((are-equivalent-vars (var-1 var-2 vars)
             (eql (position var-1 vars :key #'car)
                  (position var-2 vars :key #'cdr)))

           (are-equivalent-terms (term-1 term-2 vars)
             (cond
               ((and (is-variable term-1)
                     (is-variable term-2))
                (are-equivalent-vars (name-of term-1)
                                     (name-of term-2)
                                     vars))

               ((and (is-application term-1)
                     (is-application term-2))
                (and (are-equivalent-terms (function-of term-1)
                                           (function-of term-2)
                                           vars)
                     (are-equivalent-terms (argument-of term-1)
                                           (argument-of term-2)
                                           vars)))

               ((and (is-lambda-abstraction term-1)
                     (is-lambda-abstraction term-2))
                (are-equivalent-terms (form-of term-1)
                                      (form-of term-2)
                                      (cons (cons (binder-of term-1)
                                                  (binder-of term-2))
                                            vars)))

               (t nil))))

    (unless (is-lambda-term term-1)
      (error "~s is not a lambda term" term-1))
    (unless (is-lambda-term term-2)
      (error "~s is not a lambda term" term-2))

    (let ((free-vars-1 (free-variables term-1))
          (free-vars-2 (free-variables term-2)))
      (and (= (length free-vars-1) (length free-vars-2))
           (are-equivalent-terms term-1 term-2 (pairlis free-vars-1
                                                        free-vars-2))))))


;;; Fast interpreter
;;; Representing terms as ordered directed acyclic graphs

(defun mk-var (name)
  (list 'var name 0))

(defun is-var (term)
  (eq (car term) 'var))

(defun nam-of (var)
  (cadr var))

(defun ren-of (var)
  (caddr var))

(defun nar-of (var)
  (cdddr var))

(defun update-ren-of (var ren)
  (rplaca (cddr var) ren))

(defun update-nar-of (var nar)
  (rplacd (cddr var) nar))

(defun decorate (var)
  (update-ren-of var (1+ (ren-of var)))
  (decnar (nar-of var)))

(defun decnar (narlist)
  (when narlist
    (decorate (car narlist))
    (decnar (cdr narlist))))

(defun mk-app (fun arg)
  (list 'app fun arg))

(defun is-app (term)
  (eq (car term) 'app))

(defun fun-of (app)
  (cadr app))

(defun arg-of (app)
  (caddr app))

(defun mk-lam (bnd frm)
  (list 'lam bnd frm))

(defun is-lam (term)
  (eq (car term) 'lam))

(defun bnd-of (lam)
  (cadr lam))

(defun frm-of (lam)
  (caddr lam))

(defun mk-arid ()
  nil)

(defun is-arid (env)
  (null env))

(defun mk-bind (var val env)
  (acons var val env))

(defun var-of (env)
  (caar env))

(defun val-of (env)
  (cdar env))

(defun rest-of (env)
  (cdr env))

(defun look-up (var env)
  (if (or (is-arid env)
          (eq var (var-of env)))
      env
      (look-up var (rest-of env))))

(defun update-val-of (env val)
  (rplacd (car env) val))

(defun rep (term repenv)
  (cond
    ((is-variable term)
     (let ((env (look-up term repenv)))
       (if (is-arid env)
           (mk-var (name-of term))
           (val-of env))))

    ((is-application term)
     (mk-app (rep (function-of term) repenv)
             (rep (argument-of term) repenv)))

    ((is-lambda-abstraction term)
     (let ((var (mk-var (name-of (binder-of term)))))
       (mk-lam var (rep (form-of term) (mk-bind (binder-of term) var repenv)))))

    (t (error "~s is not a lambda term" term))))

(defun represent (term)
  (rep term (mk-arid)))

(defun full-name-of (var)
  (let ((nam (nam-of var))
        (ren (ren-of var)))
    (read-from-string (format nil "~a~:[_~d~;~]" nam (zerop ren) ren))))

(defun unrepresent (term)
  (cond
    ((is-var term) (mk-variable (full-name-of term)))
    ((is-app term) (mk-application (unrepresent (fun-of term))
                                   (unrepresent (arg-of term))))
    ((is-lam term) (mk-lambda-abstraction (unrepresent (bnd-of term))
                                          (unrepresent (frm-of term))))
    (t (error "~s is not a lambda term" term))))

(defun is-void (scolis)
  (null scolis))

(defun mk-void ()
  nil)

(defun mk-sco (var scolis)
  (cons var scolis))

(defun head-of (scolis)
  (car scolis))

(defun tail-of (scolis)
  (cdr scolis))

(defun vars-of (env)
  (if (is-arid env)
      env
      (mk-sco (var-of env) (vars-of (rest-of env)))))

(defun vals-of (env)
  (if (is-arid env)
      env
      (mk-sco (val-of env) (vals-of (rest-of env)))))

(defun mk-susp (term env)
  (list 'susp term env))

(defun is-susp (susp)
  (eq (car susp) 'susp))

(defun term-of (susp)
  (cadr susp))

(defun env-of (susp)
  (caddr susp))

(defun rename (var dynsco)
  (if (is-void dynsco)
      var
      (let ((val (head-of dynsco)))
        (if (eq var val)
            var
            (progn
              (when (and (eq (nam-of var) (nam-of val))
                         (eq (ren-of var) (ren-of val)))
                (decorate val)
                (update-nar-of var (cons val (nar-of var))))
              (rename var (tail-of dynsco)))))))

(defun propagate-renaming (term dynsco)
  (cond
    ((is-var term)
     (rename term dynsco))

    ((is-app term)
     (propagate-renaming (fun-of term) dynsco)
     (propagate-renaming (arg-of term) dynsco)
     term)

    ((is-lam term)
     (propagate-renaming (frm-of term) dynsco)
     term)

    (t (error "~s is not a lambda term" term))))

(defun rtnf (term lexenv dynsco)
  (cond
    ((is-var term) (rtnf-var term lexenv dynsco))
    ((is-app term) (rtnf-app term lexenv dynsco))
    ((is-lam term) (rtnf-lam term lexenv dynsco))
    (t (error "~s is not a lambda term" term))))

(defun rtnf-var (var lexenv dynsco)
  (let ((env (look-up var lexenv)))
    (if (is-arid env)
        (propagate-renaming var dynsco)
        (let ((susp (val-of env)))
          (if (is-susp susp)
              (let ((val (rtnf (term-of susp) (env-of susp) dynsco)))
                (update-val-of env val)
                val)
              (propagate-renaming susp dynsco))))))

(defun rtnf-app (app lexenv dynsco)
  (let ((susp (rtlf (fun-of app) lexenv dynsco)))
    (if (is-susp susp)
        (let ((fun (term-of susp))
              (env (env-of susp)))
          (rtnf (form-of fun)
                (mk-bind (bnd-of fun)
                         (mk-susp (arg-of app) lexenv)
                         env)
                dynsco))
        (mk-app susp (rtnf (arg-of app) lexenv dynsco)))))

(defun rtnf-lam (lam lexenv dynsco)
  (let ((newvar (mk-var (nam-of (bnd-of lam)))))
    (mk-lam newvar (rtnf (frm-of lam)
                         (mk-bind (bnd-of lam) newvar lexenv)
                         (mk-sco newvar dynsco)))))

(defun rtlf (term lexenv dynsco)
  (cond
    ((is-var term) (rtlf-var term lexenv dynsco))
    ((is-app term) (rtlf-app term lexenv dynsco))
    ((is-lam term) (rtlf-lam term lexenv dynsco))
    (t (error "~s is not a lambda term" term))))

(defun rtlf-var (var lexenv dynsco)
  (let ((env (look-up var lexenv)))
    (if (is-arid env)
        (propagate-renaming var dynsco)
        (let ((susp (val-of env)))
          (if (is-susp susp)
              (let ((val (rtlf (term-of susp) (env-of susp) dynsco)))
                (update-val-of env val)
                val)
              (if (is-lam susp)
                  (mk-susp susp (mk-arid))
                  (propagate-renaming susp dynsco)))))))

(defun rtlf-app (app lexenv dynsco)
  (let ((susp (rtlf (fun-of app) lexenv dynsco)))
    (if (is-susp susp)
        (let ((fun (term-of susp))
              (env (env-of susp)))
          (rtlf (form-of fun)
                (mk-bind (bnd-of fun)
                         (mk-susp (arg-of app) lexenv)
                         env)
                dynsco))
        (mk-app susp (rtnf (arg-of app) lexenv dynsco)))))

(defun rtlf-lam (lam lexenv dynsco)
  (declare (ignore dynsco))
  (mk-susp lam lexenv))

(defun reduce-to-normal-form (term)
  (unrepresent (rtnf (represent term) (mk-arid) (mk-void))))

(defun beta-reduce (term)
  "Reduce a lambda term to normal form using beta-reduction."
  (reduce-to-normal-form term))
