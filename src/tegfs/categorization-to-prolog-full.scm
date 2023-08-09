;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (categorization->prolog/full categorization/ast)
  (define ret (stack-make))
  (define (yield rule)
    (stack-push! ret rule))

  (define (yield-choice for)
    ;; Any '%choice with this `for' works.
    (yield `((,for X)
             (,'%choice ,(symbol->string for) ANYSYM X))))

  (define reverse-mapping (make-hashmap))

  (define ast/unstarred
    (map (comp (map unstar-symbol)) categorization/ast))

  (define root-predicate-name (car (car ast/unstarred)))

  (for-each
   (lambda (production)
     (define consequent (car production))
     (define antecedents (cdr production))
     (for-each
      (lambda (sym)
        (define current
          (hashmap-ref reverse-mapping sym '()))
        (hashmap-set! reverse-mapping sym
                      (cons consequent current)))
      antecedents))
   ast/unstarred)

  (for-each
   (lambda (production)
     (define consequent (car production))
     (define antecedents (cdr production))

     (unless (equal? consequent root-predicate-name)
       (yield-choice consequent))

     (for-each
      (lambda (sym)
        (define implying-lst
          (hashmap-ref reverse-mapping sym '()))

        (when (list-singleton? implying-lst)
          (let ((implying (car implying-lst)))
            (unless (equal? implying root-predicate-name)
              (yield `((,implying X) (,sym X))))))

        (yield-choice sym))
      antecedents))
   ast/unstarred)

  (define db
    (list-deduplicate/reverse
     (stack->list ret)))

  (define ambiguous
    (filter
     (negate
      (lambda (p)
        (or (null? (cdr p))
            (list-singleton? (cdr p)))))
     (hashmap->alist reverse-mapping)))

  (values db ambiguous))
