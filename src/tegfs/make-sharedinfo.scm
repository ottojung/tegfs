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
  (define-module (tegfs make-sharedinfo)
    :export (make-sharedinfo)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs current-time-p) :select (current-time/p))
    :use-module ((tegfs get-random-network-name) :select (get-random-network-name))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-ctr))
    )))



(define (make-sharedinfo entry target-fullpath for-duration)
  (define now (or (current-time/p) (raisu 'current-time-is-not-set)))
  (define recepientid (get-random-network-name))
  (define senderid (get-random-network-name))
  (sharedinfo-ctr entry target-fullpath recepientid senderid now for-duration))
