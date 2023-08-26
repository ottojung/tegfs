;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (make-rule-parser cnt)
  (define parse-term (make-term-parser cnt))

  (define (reparse-terms elements)
    (define tags-only
      (filter (negate string?) elements))
    (define inlined
      (map cadr tags-only))
    (define parsed
      (apply append (map parse-term inlined)))

    parsed)

  (lambda (rule-string)
    (define tree (rule->parse-tree rule-string))
    (define antecendents (list-ref tree 1))
    (define consequents (list-ref tree 2))
    (define antecendents/p
      (reparse-terms (cdr antecendents)))
    (define consequents/p
      (reparse-terms (cdr consequents)))
    (cartesian-product
     consequents/p
     (list antecendents/p))))


