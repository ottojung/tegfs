;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs make-permission)
    :export (make-permission)
    :use-module ((euphrates hashmap) :select (make-hashmap))
    :use-module ((euphrates hashset) :select (make-hashset))
    :use-module ((tegfs current-time-p) :select (current-time/p))
    :use-module ((tegfs get-random-access-token) :select (get-random-access-token))
    :use-module ((tegfs permission) :select (permission-constructor)))))



(define (make-permission expiery-time admin? maybepassword dynamic)
  (define token (get-random-access-token))
  (define start (current-time/p))
  (define time expiery-time)

  (permission-constructor
   token start time
   admin? dynamic
   (make-hashmap) (make-hashset)))
