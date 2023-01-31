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
  (define-module (tegfs path-safe-extension)
    :export (path-safe-extension)
    :use-module ((euphrates alphanum-alphabet) :select (alphanum/alphabet/index))
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates compose-under) :select (compose-under))
    :use-module ((euphrates list-and-map) :select (list-and-map))
    :use-module ((euphrates path-extension) :select (path-extension))
    )))



(define (path-safe-extension path)
  (define max-size 7)
  (define ext (path-extension path))

  (if (and (>= max-size (string-length ext))
           (list-and-map (compose-under or (comp (equal? #\.)) alphanum/alphabet/index)
                         (string->list ext)))
      ext
      ""))
