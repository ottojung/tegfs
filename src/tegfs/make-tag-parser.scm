;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define tag-grammar
  `( tag = word arg*
     arg = equal idset
     idset = variable comma idset / variable
     word = wordc+ / quoted
     wordc = alnum / "%" / "-"
     variable = letter alnum*  / ,tags-this-variable/string
     alnum = letter / digit
     letter = (re alpha)
     digit = (re numeric)
     quoted = (re string)
     comma = "," / "+"
     equal = "=" / ":"
     other = (re any)
     ))

(define backend-parser
  (lalr-parser/simple
   `(:grammar ,tag-grammar
     :join (variable word)
     :inline (variable word arg* idset)
     :skip (equal comma))))

(define (tag->tree tag)
  (define (errorp . args) #f)
  (backend-parser errorp (~a tag)))

(define (make-tag-parser counter)
  (lambda (tag)
    (define single-tree (tag->tree tag))

    (unless single-tree
      (raisu* :from "make-tag-parser"
              :type 'type-error
              :message "Input tag is malformed"
              :args (list tag)))

    (define pred
      (string->symbol
       (cadr single-tree)))

    (define args0
      (map cdr (cddr single-tree)))

    (define args
      (if (null? args0)
          (list (list tags-this-variable/string))
          args0))

    (define tags
      (cartesian-product
       (list pred) args))

    tags))
