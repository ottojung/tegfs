;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Translates tags like "recording*", "im*age" into "(choice "video" "recording" "$")" and "(choice "image" "image" "$")"
(define (categorization-translate-choices parser ast/flatten starred)
  (define ret (stack-make))
  (define (yield x) (stack-push! ret x))

  (define main-name
    (if (null? ast/flatten) #f
        (car (car ast/flatten))))

  (for-each
   (lambda (tag)
     (define tag/unstarred (unstar-symbol tag))
     (define tag/parsed/all (parser tag/unstarred))

     (for-each
      (lambda (tag/parsed)
        (for-each
         (lambda (production)
           (define consequent (car production))
           (define antecedents (cdr production))
           (define consequent/no* (unstar-symbol consequent))

           (cond
            ((equal? consequent tag)
             (yield
              (categorization-translate-direct-parsed-choice tag/parsed)))
            ((member tag antecedents)
             (let ((impl (if (equal? consequent main-name) (car tag/parsed) consequent/no*)))
               (yield
                (categorization-make-choice impl tag/parsed))))))
         ast/flatten))
      tag/parsed/all))
   starred)

  (reverse
   (stack->list ret)))
