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
  (define-module (tegfs entries-iterate)
    :export (entries-iterate)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((euphrates stack) :select (list->stack stack-empty? stack-pop!))
    :use-module ((tegfs get-registry-files) :select (get-registry-files))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path))
    :use-module ((tegfs warning) :select (warning)))))



;; Iterator returns #f when done.
(define (entries-iterate)
  (define registries
    (list->stack
     (get-registry-files)))

  (define input-port #f)
  (define registry-property #f)

  (define (init-next-registry)
    (define registry-path
      (stack-pop! registries))
    (define registry-fullpath
      (append-posix-path (get-root) registry-path))
    (define p
      (catch-any
       (lambda _
         (open-file-port registry-fullpath "r"))
       (lambda _
         (warning "Could not open registry ~s" registry-path)
         #f)))

    (cond
     (p
      (set! input-port p)
      (set! registry-property
            (cons keyword-entry-registry-path registry-path)))
     ((stack-empty? registries)
      (set! input-port #f))
     (else
      (init-next-registry))))

  (define (next)
    (if input-port
        (let ((x (read input-port)))
          (if (eof-object? x)
              (begin
                (close-port input-port)
                (set! input-port #f)
                (next))
              (cons registry-property x)))
        (if (stack-empty? registries)
            #f
            (begin
              (init-next-registry)
              (next)))))

  next)
