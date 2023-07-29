;;;; Copyright (C) 2023, 2022  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Returns a tree that represents the tags structure

(define (categorization-parse categorization-text)
  (define-values (cfg-part rules-part)
    (categorization-split categorization-text))

  (define words
    (call-with-input-string cfg-part
      (lambda (p)
        (read-list p))))

  (define words-flat
    (list-map/flatten (curry-if pair? identity list) words))

  (define ast
    (if (null? words-flat) '()
        (CFG-CLI->CFG-AST words-flat)))

  (define ast/flatten
    (map (lambda (production)
           (cons (car production)
                 (list-deduplicate
                  (apply append (cdr production)))))
         ast))

  ast/flatten)
