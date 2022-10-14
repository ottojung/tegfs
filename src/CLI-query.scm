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

%use (appcomp comp) "./euphrates/comp.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (printf) "./euphrates/printf.scm"
%use (entry-print/formatted) "./entry-print-formatted.scm"
%use (entry-print) "./entry-print.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"

(define (CLI-query --diropen --dirpreview --entries <query-format> <query...>)
  (define counter 0)
  (define opening-properties
    (appcomp
     '()
     ((curry-if (const --diropen) (comp (cons keyword-diropen))))
     ((curry-if (const --dirpreview) (comp (cons keyword-dirpreview))))))

  (cond
   (--entries
    (tegfs-query/open
     opening-properties
     <query...>
     (lambda (entry)
       (set! counter (+ 1 counter))
       (entry-print entry)
       (display "\n\n"))))
   (<query-format>
    (tegfs-query/open
     opening-properties
     <query...>
     (lambda (entry)
       (set! counter (+ 1 counter))
       (entry-print/formatted <query-format> entry)
       (display "\n")))))

  (parameterize ((current-output-port (current-error-port)))
    (if (equal? 0 counter)
        (display "No matches.")
        (printf "Total of ~a matches." counter))
    (newline)))
