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
  (define-module (tegfs cli-query)
    :export (CLI::query)
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates fn-alist) :select (fn-alist))
    :use-module ((euphrates printf) :select (printf))
    :use-module ((euphrates profune-communicator) :select (profune-communicator-handle))
    :use-module ((tegfs entry-print-formatted) :select (entry-print/formatted))
    :use-module ((tegfs entry-print) :select (entry-print))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs tegfs-make-communicator) :select (tegfs-make-communicator))
    )))



(define (CLI::query --diropen --dirpreview --entries <query-format> <query...>)
  (define print-func
    (cond
     (--entries (lambda (entry) (entry-print entry) (newline)))
     (<query-format> (comp (entry-print/formatted <query-format>)))
     (else (fatal "Unexpected mode: both --entries and <query-format> were not set"))))

  (define query
    (or <query...> '("%any")))

  (define result
    (profune-communicator-handle
     (tegfs-make-communicator)
     `(whats
       (diropen? ,--diropen)
       (dirpreview? ,--dirpreview)
       (query ,<query...>)
       (entry E)
       more (99999)
       )))

  (define equals (cadr (cadr result)))
  (for-each
   (fn-alist
    (E)
    (print-func E)
    (newline))
   equals)

  (parameterize ((current-output-port (current-error-port)))
    (let ((counter (length equals)))
      (if (equal? 0 counter)
          (display "No matches.")
          (printf "Total of ~a matches." counter))
      (newline))))
