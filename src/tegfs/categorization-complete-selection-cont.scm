;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs categorization-complete-selection-cont)
    :export (categorization-complete-selection/cont)
    :use-module ((euphrates list-and-map) :select (list-and-map))
    :use-module ((euphrates list-deduplicate) :select (list-deduplicate/reverse))
    :use-module ((euphrates list-get-duplicates) :select (list-get-duplicates))
    :use-module ((euphrates negate) :select (negate))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs categorization-to-prolog-full) :select (categorization->prolog/full))
    :use-module ((tegfs categorization-translate-choices) :select (categorization-translate-choices))
    :use-module ((tegfs parse-tag) :select (parse-tag))
    :use-module ((tegfs profun-compute-ground) :select (profun-compute-ground))
    :use-module ((tegfs unparse-tag) :select (unparse-tag))
    :use-module ((tegfs unstar-symbol) :select (unstar-symbol))
    )))

(define (type-symbol? x)
  (define s (~a x))
  (or (string-prefix? "<" s)
      (string-suffix? ">" s)))

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
     (filter (negate type-symbol?)
             (append selected result/for-humans))))

  (define ambiguous
    (filter
     (lambda (x)
       (and (member (car x) selected)
            (not (type-symbol? x))
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
