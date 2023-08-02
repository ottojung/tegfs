;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (profun-compute-ground rules all-tags)
  (define db
    (profun-create-falsy-database
     profun-standard-handler
     rules))

  (define stack (stack-make))
  (define (yield x) (stack-push! stack x))

  (for-each
   (lambda (tag)
     (define query (list (list tag 'X)))
     (define terms (profun-eval-query/terms db query))
     (for-each yield terms))
   all-tags)

  (stack->list stack))
