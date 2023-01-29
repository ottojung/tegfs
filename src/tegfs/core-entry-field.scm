;;;; Copyright (C) 2022  Otto Jung
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
  (define-module (tegfs core-entry-field)
    :export (core::entry-field)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates bool-to-profun-result) :select (bool->profun-result))
    :use-module ((euphrates list-and-map) :select (list-and-map))
    :use-module ((euphrates profun-accept) :select (profun-accept profun-ctx-set profun-set))
    :use-module ((euphrates profun-answer-huh) :select (profun-answer?))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-reject) :select (profun-reject))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?)))))



(define (profun-op-return-iter ctx initialize-iter ret-name)
  (define iter (or ctx (initialize-iter)))
  (define x (iter))
  (if x
      (profun-set
       (ret-name <- x)
       (if ctx
           (profun-accept)
           (profun-ctx-set iter)))
      (profun-reject)))

(define (get-field-value entry field)
  (bool->profun-result
   (if (and (list? entry)
            (list-and-map pair? entry))
       (let ((field* (cond
                      ((symbol? field) field)
                      ((string? field) (string->symbol field))
                      (else #f))))
         (if field*
             (assq-or field* entry (profun-reject))
             (make-profun-error 'not-a-valid-field field)))
       (make-profun-error 'not-a-valid-entry entry))))

(define (return-field-value entry field result-name)
  (define ret (get-field-value entry field))
  (if (profun-answer? ret) ret
      (profun-set (result-name <- ret))))

(define (check-field-value entry field result)
  (define ret (get-field-value entry field))
  (if (profun-answer? ret) ret
      (bool->profun-result (equal? ret result))))

(define (enumerate-fields ctx entry field-name result-name)
  (define rest (or ctx entry))
  (let loop ((rest rest))
    (if (not (pair? rest))
        (profun-reject)
        (let ((x (car rest)))
          (if (not (pair? x)) (loop (cdr rest))
              (let ((field (car x))
                    (result (cdr x)))
                (profun-set
                 (field-name <- field)
                 (profun-set
                  (result-name <- result)
                  (profun-ctx-set (cdr rest))))))))))

(define core::entry-field
  (profun-op-lambda
   (ctx (entry field result)
        (entry-name field-name result-name))

   (cond
    ((and (profun-bound-value? entry)
          (profun-bound-value? field)
          (profun-unbound-value? result))
     (return-field-value entry field result-name))

    ((and (profun-bound-value? entry)
          (profun-bound-value? field)
          (profun-bound-value? result))
     (check-field-value entry field result-name))

    ((and (profun-bound-value? entry)
          (profun-unbound-value? field)
          (profun-unbound-value? result))
     (enumerate-fields ctx entry field-name result-name))
    (else (make-profun-error 'not-implemented)))))
