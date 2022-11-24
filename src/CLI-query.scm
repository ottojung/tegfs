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

%var CLI-query

%use (comp) "./euphrates/comp.scm"
%use (monad-make/hook) "./euphrates/monad-make-hook.scm"
%use (printf) "./euphrates/printf.scm"
%use (with-monad) "./euphrates/with-monad.scm"
%use (entry-print/formatted) "./entry-print-formatted.scm"
%use (entry-print) "./entry-print.scm"
%use (fatal) "./fatal.scm"
%use (get-admin-permissions) "./get-admin-permissions.scm"
%use (tegfs-query) "./tegfs-query.scm"

(define (CLI-query --diropen --dirpreview --entries <query-format> <query...>)
  (define counter 0)
  (define print-func
    (cond
     (--entries (lambda (entry) (entry-print entry) (newline)))
     (<query-format> (comp (entry-print/formatted <query-format>)))
     (else (fatal "Unexpected mode: both --entries and <query-format> were not set"))))
  (define show-monad
    (monad-make/hook
     (lambda (tags)
       (cond
        ((memq 'unfold-entry tags) (lambda (f) (f)))
        ((memq 'entry tags)
         (lambda (entry)
           (set! counter (+ 1 counter))
           (print-func entry)
           (newline)))
        ((memq 'ask tags)
         (lambda (type)
           (case type
             ((query/split) <query...>)
             ((filemap/2) #f) ;; TODO: get from server
             ((permissions) (get-admin-permissions)) ;; TODO: get from command-line & server
             ((diropen?) --diropen)
             ((dirpreview?) --dirpreview))))))))

  (with-monad show-monad (tegfs-query))

  (parameterize ((current-output-port (current-error-port)))
    (if (equal? 0 counter)
        (display "No matches.")
        (printf "Total of ~a matches." counter))
    (newline)))
