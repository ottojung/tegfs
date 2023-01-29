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
  (define-module (tegfs get-random-network-name)
    :export (get-random-network-name)
    :use-module ((euphrates alphanum-alphabet) :select (alphanum/alphabet))
    :use-module ((euphrates random-choice) :select (random-choice)))))

;; Short names for users to see.
;; Use this only if exporting over a slow network.
;; Case-sensitive!



(define (get-random-network-name)
  (list->string
   (random-choice 10 alphanum/alphabet)))
