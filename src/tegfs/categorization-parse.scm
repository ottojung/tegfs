;;;; Copyright (C) 2023, 2022  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Returns a tree that represents the tags structure

(define (categorization-parse categorization-text)
  (define-values (cfg-part rules-part)
    (categorization-split categorization-text))

  (define ast/flatten
    (categorization-parse-tree cfg-part))

  (define additional-rules
    (map inference->profun-rule
         (reverse
          (dump-rules-from-text/list rules-part))))

  (values ast/flatten additional-rules))
