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
  (define-module (tegfs get-registry-files)
    :export (get-registry-files)
    :use-module ((euphrates string-split-simple) :select (string-split/simple))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs get-root) :select (get-root))
    )))



(define (get-registry-files)
  (define root (get-root))
  (define config (get-config))
  (define registries/string
    (cadr (or (assoc 'registries config) '(registries ""))))
  (define registries (string-split/simple registries/string #\:))
  registries)
