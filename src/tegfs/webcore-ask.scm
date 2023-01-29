;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs webcore-ask)
    :export (webcore::ask)
    :use-module ((euphrates profune-communicator) :select (profune-communicator-handle))
    :use-module ((tegfs webcore-current-communicator-p) :select (webcore::current-communicator/p)))))



(define (webcore::ask sentence)
  (profune-communicator-handle
   (webcore::current-communicator/p)
   (cons 'reset sentence)))
