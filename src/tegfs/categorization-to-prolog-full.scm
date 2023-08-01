;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (categorization->prolog/full categorization/ast)
  (define ret (stack-make))

  (define reverse-mapping (make-hashmap))

  (define (yield rule)
    (stack-push! ret rule))

  (for-each
   (lambda (production)
     (define LHS (car production))
     (define RHS (cdr production))
     (for-each
      (lambda (sym)
        (define current
          (hashmap-ref reverse-mapping sym '()))
        (hashmap-set! reverse-mapping sym
                      (cons LHS current)))
      RHS))
   categorization/ast)

  (for-each
   (lambda (production)
     (define LHS (car production))
     (define RHS (cdr production))

     (for-each
      (lambda (sym)
        (define implying-lst
          (hashmap-ref reverse-mapping sym '()))

        (when (list-singleton? implying-lst)
          (let ((implying (car implying-lst)))
            (yield `((,implying X) (,sym X)))))

        (define qualified-name
          (string->symbol
           (string-append (~a LHS) "/" (~a sym))))

        (yield `((,LHS X) (,qualified-name X))))
      RHS))
   categorization/ast)

  (define db (stack->list ret))
  (define ambiguous
    (filter
     (negate
      (lambda (p)
        (or (null? (cdr p))
            (list-singleton? (cdr p)))))
     (hashmap->alist reverse-mapping)))

  (values db ambiguous))
