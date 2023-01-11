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

%run guile

%var web::iterate-profun-results

%use (fn-alist) "./euphrates/fn-alist.scm"
%use (web::handle-profun-results/default-fail-fun web::handle-profun-results/or) "./web-handle-profun-results.scm"

(define-syntax web::iterate-profun-results
  (syntax-rules (:or :results :wrap)

    ((_ :results results :or fail-fun . rest)
     (web::iterate-profun-results
      :or fail-fun
      :results results
      . rest))
    ((_ :results results :wrap wrapper . rest)
     (web::iterate-profun-results
      :wrap wrapper
      :results results
      . rest))
    ((_ :wrap wrapper :or fail-fun . rest)
     (web::iterate-profun-results
      :or fail-fun
      :wrap wrapper
      . rest))

    ((_ :or fail-fun :wrap wrapper :results results (name . names) . bodies)
     (let ()
       (define fun (fn-alist (name . names) . bodies))
       (define (fun* equals)
         (cond
          ((null? equals) 'false)
          ((null? (cdr equals))
           (fun (car equals)))
          (else
           (for-each fun equals))))
       (define fun**
         (if wrapper
             (lambda (equals)
               (let ((success? #t)
                     (empty? (null? equals)))
                 (wrapper success? empty? (lambda _ (fun* equals)))))
             fun*))
       (define fail-fun*
         (if wrapper
             (lambda (results)
               (let ((success? #f)
                     (empty? #t))
                 (wrapper success? empty? (lambda _ (fail-fun results)))))
             fail-fun))

       (web::handle-profun-results/or results fun** fail-fun*)))

    ((_ :or fail-fun :results results (name . names) . bodies)
     (web::iterate-profun-results
      :or fail-fun
      :wrap #f
      :results results
      (name . names) . bodies))

    ((_ :wrap wrapper :results results (name . names) . bodies)
     (web::iterate-profun-results
      :or web::handle-profun-results/default-fail-fun
      :wrap wrapper
      :results results
      (name . names) . bodies))

    ((_ :results results (name . names) . bodies)
     (web::iterate-profun-results
      :wrap #f
      :results results
      (name . names) . bodies))

    ((_ results (name . names) . bodies)
     (web::iterate-profun-results
      :results results
      (name . names)
      . bodies))

    ))
