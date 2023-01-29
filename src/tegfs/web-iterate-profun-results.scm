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
  (define-module (tegfs web-iterate-profun-results)
    :export (web::iterate-profun-results)
    :use-module ((euphrates fn-alist) :select (fn-alist))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs web-handle-profun-results) :select (web::handle-profun-results/default-fail-fun web::handle-profun-results/or)))))



(define-syntax web::iterate-profun-results
  (syntax-rules (:or :results :onfalse)

    ((_ :results results :or fail-fun . rest)
     (web::iterate-profun-results
      :or fail-fun
      :results results
      . rest))
    ((_ :results results :onfalse onfalse . rest)
     (web::iterate-profun-results
      :onfalse onfalse
      :results results
      . rest))
    ((_ :onfalse onfalse :or fail-fun . rest)
     (web::iterate-profun-results
      :or fail-fun
      :onfalse onfalse
      . rest))

    ((_ :or fail-fun :onfalse onfalse :results results (name . names) . bodies)
     (let ()
       (define fun (fn-alist (name . names) . bodies))
       (define (fun* equals)
         (cond
          ((null? equals)
           (case onfalse
             ((nothing) (when #f #t))
             ((fail) (fail-fun results))
             (else
              (if (procedure? onfalse)
                  (onfalse results)
                  (raisu 'type-error "Bad type for onfalse value")))))
          ((null? (cdr equals))
           (fun (car equals)))
          (else
           (for-each fun equals))))

       (web::handle-profun-results/or results fun* fail-fun)))

    ((_ :or fail-fun :results results (name . names) . bodies)
     (web::iterate-profun-results
      :or fail-fun
      :onfalse #f
      :results results
      (name . names) . bodies))

    ((_ :onfalse onfalse :results results (name . names) . bodies)
     (web::iterate-profun-results
      :or web::handle-profun-results/default-fail-fun
      :onfalse onfalse
      :results results
      (name . names) . bodies))

    ((_ :results results (name . names) . bodies)
     (web::iterate-profun-results
      :onfalse #f
      :results results
      (name . names) . bodies))

    ((_ results (name . names) . bodies)
     (web::iterate-profun-results
      :results results
      (name . names)
      . bodies))

    ))
