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
  (define-module (tegfs cli-talk)
    :export (CLI::talk)
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates profune-communicator) :select (profune-communicator-handle))
    :use-module ((euphrates read-list) :select (read-list))
    :use-module ((euphrates read-string-line) :select (read-string-line))
    :use-module ((euphrates serialization-short) :select (deserialize/short serialize/short))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((euphrates words-to-string) :select (words->string))
    :use-module ((tegfs tegfs-make-communicator) :select (tegfs-make-communicator))
    :use-module ((tegfs web-make-communicator) :select (web::make-communicator))
    :use-module ((tegfs with-current-time) :select (with-current-time))
    )))



(define (CLI::talk --web)

  (define (read-sentence)
    (catch-any
     (lambda _
       (define line (read-string-line))
       (if (eof-object? line) line
           (with-input-from-string
               line
             (lambda _ (deserialize/short (read-list))))))
     (lambda _
       (display "Error parsing input\n" (current-error-port))
       (read-sentence))))

  (define comm
    (if --web
        (let ()
          (define-values (comm server-operator-key) (web::make-communicator))
          comm)
        (tegfs-make-communicator)))

  (define (send-to-server echo? read-sentence)
    (display "[client] " (current-error-port))

    (let ((sentence (read-sentence)))

      (when echo?
        (for-each (lambda (w) (write w) (display " ")) (serialize/short sentence))
        (newline))

      (if (eof-object? sentence)
          (begin
            (display "\nGoodbye!\n" (current-error-port))
            #f)
          (let ((answer
                 (with-current-time
                  (profune-communicator-handle comm sentence))))

            (display "[server] ")
            (display (words->string (map ~s (serialize/short answer))))
            (newline)
            #t))))

  (let loop ()
    (when (send-to-server #f read-sentence)
      (loop))))
