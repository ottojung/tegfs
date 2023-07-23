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
  (define-module (tegfs config-set)
    :export (config-set!)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates assq-set-value-star) :select (assq-set-value*))
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs get-root) :select (get-root))
    )))

(define (config-set! keylist config value)
  (define path (append-posix-path (get-root) "config.tegfs.lisp"))
  (define value* (and value (or (catch-any (lambda _ (string->number value)) (lambda _ #f)) value)))
  (define new (assq-set-value* keylist value* config))

  (unless (or (string? value*) (number? value*))
    (raisu 'type-error "Config values should be strings or numbers, but got something else." value))

  (call-with-output-file
      path
    (lambda (p)
      (for-each (lambda (x) (write x p) (newline p)) new)))

  new)
