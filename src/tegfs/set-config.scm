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
  (define-module (tegfs set-config)
    :export (set-config)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs get-root) :select (get-root))
    )))



(define (set-config name value)
  (define path (append-posix-path (get-root) "config.tegfs.lisp"))
  (define existing (or (get-config) '()))
  (define new (assoc-set-value name value existing))
  (define p (open-file-port path "w"))
  (for-each (lambda (x) (write x p) (newline p)) new)
  (close-port p)
  (values))
