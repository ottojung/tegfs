;;;; Copyright (C) 2022  Otto Jung
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
  (define-module (tegfs get-random-access-token)
    :export (get-random-access-token)
    :use-module ((euphrates alphanum-lowercase-alphabet) :select (alphanum-lowercase/alphabet))
    :use-module ((euphrates random-choice) :select (random-choice))
    )))



(define (get-random-access-token)
  (list->string (random-choice 60 alphanum-lowercase/alphabet)))
