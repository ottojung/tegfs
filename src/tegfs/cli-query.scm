;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (CLI::query --diropen --dirpreview --sexp-format <format> <query...>)
  (define print-func
    (cond
     (--sexp-format (lambda (entry) (entry-print entry) (newline)))
     (<format> (comp (entry-print/formatted <format>)))
     (else (fatal "Unexpected mode: both --sexp-format and <format> were not set"))))

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

  (let ((counter (length equals)))
    (if (equal? 0 counter)
        (log-info "No matches.")
        (log-info "Total of ~a matches." counter))))
