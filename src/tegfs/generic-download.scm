;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (generic-download headers url output)
  (define user-agent
    "--user-agent=Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:52.0) Gecko/20100101 Firefox/52.0")

  (define outfile
    (cond
     ((string? output) output)
     ((equal? output 'string) "-")
     (else (raisu 'type-error "Unknown input type" output))))

  (define verbosity
    (cond
     ((> 30 (verbosity-level/p)) "--quiet")
     ((> 60 (verbosity-level/p)) "--no-verbose")
     (else "--verbose")))

  (define caller
    (cond
     ((string? output)
      (lambda args
        (values #f (apply system*/exit-code args))))
     ((equal? output 'string) run-syncproc/re*)
     (else
      (raisu 'type-error "Unknown input type" output))))

  (define (make-header p)
    (define key (car p))
    (define value (cdr p))
    (list "--header"
          (string-append
           (~a key) ": " (~a value))))

  (define header-arguments
    (list-map/flatten make-header headers))

  (define all-args
    (append
     (list "wget" user-agent verbosity
           "--timeout=10" ;; 10 seconds for response.
           "--random-wait"
           "--no-netrc"
           "--tries=1000"
           )
     header-arguments
     "-O" outfile url))

  (apply caller all-args))
