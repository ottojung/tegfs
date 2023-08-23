;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (make-tag-parser counter)
  (lambda (tag)
    (define single-tree
      (tag->parse-tree tag))

    (unless single-tree
      (raisu* :from "make-tag-parser"
              :type 'type-error
              :message "Input tag is malformed"
              :args (list tag)))

    (define tag-body (cadr single-tree))

    (cond
     ((equal? 'flat-tag (car tag-body))
      (let ()
        (define pred
          (string->symbol
           (cadr tag-body)))

        (define args0
          (map cdr (cddr tag-body)))

        (define args
          (if (null? args0)
              (list (list tags-this-variable/string))
              args0))

        (define tags
          (cartesian-product
           (list pred) args))

        tags))
     (else
      (raisu 'impossible-tag-type tag-body)))))
