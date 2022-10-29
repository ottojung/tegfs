;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var CLI-query

%use (comp) "./euphrates/comp.scm"
%use (monad-make/hook) "./euphrates/monad-make-hook.scm"
%use (monadstate-qtags) "./euphrates/monadstate.scm"
%use (printf) "./euphrates/printf.scm"
%use (with-monad) "./euphrates/with-monad.scm"
%use (core-query) "./core-query.scm"
%use (entry-print/formatted) "./entry-print-formatted.scm"
%use (entry-print) "./entry-print.scm"
%use (fatal) "./fatal.scm"

(define (CLI-query --diropen --dirpreview --entries <query-format> <query...>)
  (define counter 0)
  (define print-func
    (cond
     (--entries (lambda (entry) (entry-print entry) (newline)))
     (<query-format> (comp (entry-print/formatted <query-format>)))
     (else (fatal "Unexpected mode: both --entries and <query-format> were not set"))))
  (define show-monad
    (monad-make/hook
     tags (entry)
     (when (memq 'say-entry tags)
       (set! counter (+ 1 counter))
       (print-func entry)
       (newline))))

  (with-monad
   show-monad
   (core-query
    --diropen
    --dirpreview
    <query...>))

  (parameterize ((current-output-port (current-error-port)))
    (if (equal? 0 counter)
        (display "No matches.")
        (printf "Total of ~a matches." counter))
    (newline)))
