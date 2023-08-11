;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (tegfs-query/noopen/notall/prolog iter0 <query...>)
  (define output-path (string-append (make-temporary-filename/local) ".pl"))
  (define output-port (open-file-port output-path "w"))

  (define _333
    (parameterize ((current-output-port output-port))
      (define-values (parsed-query variables) (prolog-query-parse <query...>))
      (define initializations (map (lambda (v) `(vandthis ,(make-prolog-var 'This) ,(make-prolog-var v))) variables))
      (define prolog-query-0 (map tag->prolog-term (append initializations parsed-query)))
      (define prolog-query (apply string-append (list-intersperse ", " prolog-query-0)))
      (display ":- initialization(main, main).") (newline)
      (if (and (tegfs-dump-prolog)
               (not (null? parsed-query)))
          (let ()
            (printf "single(This) :- ~a.\n" prolog-query)
            (printf "thises(This) :- i(This, Id), single(This), writeln(Id).\n")
            (printf "main(_Argv) :- findall(This, thises(This), _).")
            (newline) (newline))
          (let ()
            (printf "main(_Argv) :- true.")
            (newline) (newline)))))

  (define _222
    (close-port output-port))

  (define-values (ids/string code)
    (run-syncproc/re* "prolog" "-s" output-path))

  (define _444
    (file-delete output-path))

  (define _111
    (unless (equal? 0 code)
      (raisu 'prolog-execution-failed code ids/string)))

  (define ids
    (appcomp ids/string
             string->lines
             (map string-strip)
             (filter (negate string-null?))
             make-hashset))

  (define (iter)
    (define entry (iter0))
    (and entry
         (let ((id (assq-or keyword-id entry #f)))
           (if (hashset-has? ids id)
               entry
               (iter)))))

  iter)
