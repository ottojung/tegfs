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

%run guile

%var entries-iterate

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (catch-any) "./euphrates/catch-any.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (list->stack stack-empty? stack-pop!) "./euphrates/stack.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (get-root) "./get-root.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (warning) "./warning.scm"

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
