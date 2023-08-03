;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Translates tags like "recording*", "im*age" into "(video/recording $)" and "(image $)"
(define (categorization-translate-choices parser ast/flatten starred)
  (define ret (stack-make))
  (define (yield x) (stack-push! ret x))

  (for-each
   (lambda (tag)
     (define tag/unstarred (unstar-symbol tag))

     (if (equal? tag/unstarred tag)
         (yield (parser tag/unstarred))
         (for-each
          (lambda (production)
            (define consequent (car production))
            (define antecedents (cdr production))

            (cond
             ((equal? consequent tag)
              (yield (parser tag/unstarred)))
             ((member tag antecedents)
              (yield
               (parser
                (mangle-tag-choice
                 (unstar-symbol consequent)
                 tag/unstarred))))))
          ast/flatten)))
   starred)

  (stack->list ret))