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

%run guile

%var entry-field-handler

%use (assq-or) "./euphrates/assq-or.scm"
%use (bool->profun-result) "./euphrates/bool-to-profun-result.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (profun-accept profun-ctx-set profun-set) "./euphrates/profun-accept.scm"
%use (profun-answer?) "./euphrates/profun-answer-huh.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"

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

(define (return-field-value entry field result-name)
  (define ret
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

  (if (profun-answer? ret) ret
      (profun-set (result-name <- ret))))

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

(define entry-field-handler
  (profun-op-lambda
   (ctx (entry field result)
        (entry-name field-name result-name))

   (cond
    ((and (profun-bound-value? entry)
          (profun-bound-value? field)
          (profun-unbound-value? result))
     (return-field-value entry field result-name))

    ((and (profun-bound-value? entry)
          (profun-unbound-value? field)
          (profun-unbound-value? result))
     (enumerate-fields ctx entry field-name result-name))
    (else (make-profun-error 'not-implemented)))))
