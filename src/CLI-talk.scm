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

%run guile

%var CLI-talk

%use (catch-any) "./euphrates/catch-any.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (deserialize/short serialize/short) "./euphrates/serialization-short.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (get-admin-permissions) "./get-admin-permissions.scm"
%use (tegfs-make-communicator) "./tegfs-make-communicator.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-make-context) "./web-make-context.scm"
%use (with-current-time) "./with-current-time.scm"

(define (CLI-talk --web)

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
        (web-make-communicator (web-make-context))
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

  (send-to-server
   #t (const `(listen ((admin-permissions ,(get-admin-permissions))))))

  (let loop ()
    (when (send-to-server #f read-sentence)
      (loop))))
