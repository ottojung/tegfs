;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (categorization-complete-selection/cont ast/flatten all-tags starred)
  (define-values (translated-tree ambiguous-branches)
    (categorization->prolog/full ast/flatten))

  (define parser (parse-tag 0))
  (define translated-choices
    (categorization-translate-choices parser ast/flatten starred))

  (define rules
    (append translated-choices
            translated-tree
            `(((%any X)))))

  (define result
    (profun-compute-ground rules all-tags))

  (define result/for-humans
    (map unparse-tag result))

  (define selected (map unstar-symbol starred))

  (define result/final
    (list-deduplicate/reverse
     (filter (negate type-tag?)
             (append selected result/for-humans))))

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
