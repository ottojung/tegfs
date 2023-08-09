;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (categorization-complete-selection/cont ast/flatten translated-choices)
  (define-values (translated-tree ambiguous-branches)
    (categorization->prolog/full ast/flatten))

  (define choices-as-tags
    (map (lambda (choice)
           (cons (string->symbol (caddr choice))
                 (cdddr choice)))
         translated-choices))

  (define rules/0
    (append translated-tree
            (map list translated-choices)
            (map list choices-as-tags)))

  (define rules
    (append rules/0 `(((%any X)))))

  (define all-tags
    (map car rules/0))

  (define result/0
    (profun-compute-ground rules all-tags))

  (define result
    (filter (negate (comp car (equal? '%choice))) result/0))

  (define result/for-humans
    (map unparse-tag result))

  (define result/final
    (list-deduplicate/reverse
     (filter (negate type-tag?)
             result/for-humans)))

  (define selected
    (map tag-choice->immediate-tag translated-choices))

  (define ambiguous
    (filter
     (lambda (x)
       (and (member (car x) selected)
            (not (type-tag? x))
            (list-and-map
             (lambda (specializer)
               (not (member specializer result/final)))
             (cdr x))))
     ambiguous-branches))

  (define duplicates
    (map car (list-get-duplicates selected)))

  (append
   (list (cons 'ok result/final))
   (if (null? ambiguous) '()
       (list (cons 'ambiguous ambiguous)))
   (if (null? duplicates) '()
       (list (cons 'duplicates duplicates)))
   ))
