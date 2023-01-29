;;;; Copyright (C) 2023  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(cond-expand
 (guile
  (define-module (tegfs web-handle-profun-results)
    :export (web::handle-profun-results web::handle-profun-results/hooked web::handle-profun-results/or web::handle-profun-results/default-fail-fun)
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((euphrates words-to-string) :select (words->string))
    :use-module ((tegfs web-bad-request) :select (web::bad-request))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-permission-denied) :select (web::permission-denied)))))



(define (web::handle-profun-results/2 results0 fun fail-fun)
  (define results (cdr results0))
  (define head
    (if (null? results)
        (fail-fun results0)
        (car results)))

  (cond
   ((and (= 2 (length head))
         (equal? 'equals (car head)))
    (let ((equals (cadr head)))
      (fun equals)))

   ((and (= 3 (length head))
         (equal? '= (car head)))
    (let ((equals (list (map (comp (apply (lambda (EQ A B) (cons A B)))) results))))
      (fun equals)))

   ((equal? '((true)) results) (fun '(())))
   ((equal? '((false)) results) (fun '()))
   (else (fail-fun results0))))

(define (web::handle-profun-results results fun)
  (web::handle-profun-results/hooked results fun identity))

(define (web::handle-profun-results/default-error args)
  (define errargs
    (and (= 1 (length args))
         (car args)))

  (define errtype
    (and (list? errargs)
         (not (null? errargs))
         (car errargs)))

  (define (bad-request)
    (web::bad-request
     "Error: ~a"
     (words->string (map ~s errargs))))

  (case errtype
    ((not-found) (web::not-found))
    ((permission-denied) (web::permission-denied))
    (else
     (if errargs (bad-request)
         (raisu 'unexpected-error-from-backend args)))))

(define (web::handle-profun-results/default-fail-fun* hook)
  (lambda (results)
    (hook results)
    (case (car results)
      ((its)
       (raisu 'unexpected-its-result-from-backend-76123 results))
      ((error) (web::handle-profun-results/default-error (cdr results)))
      (else
       (raisu 'unexpected-results-from-backend-87156243510 results)))))

(define web::handle-profun-results/default-fail-fun
  (web::handle-profun-results/default-fail-fun* identity))

(define (web::handle-profun-results/hooked results fun hook)
  (define fail-fun (web::handle-profun-results/default-fail-fun* hook))
  (web::handle-profun-results/or results fun fail-fun))

(define (web::handle-profun-results/or results fun fail-fun)
  (case (car results)
    ((its)
     (web::handle-profun-results/2 results fun fail-fun))
    (else
     (fail-fun results))))
