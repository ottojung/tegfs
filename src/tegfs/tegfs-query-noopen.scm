;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (tegfs-query/noopen <query...>)
  (define iter0 (entries-iterate))
  (if (and <query...> (not (null? <query...>)))
      ;; (tegfs-query/noopen/notall iter0 <query...>)
      (tegfs-query/noopen/notall/profun iter0 <query...>)
      iter0))

(define (inference->profun-rule inference)
  (define RHS (generify-dumped-term (cddr inference)))
  (define consequent (generify-dumped-term (list (cadr inference))))
  (append consequent RHS))

(define (tegfs-query/noopen/notall/profun iter0 <query...>)
  (define-values (parsed-query/1 variables) (prolog-query-parse <query...>))
  (define parsed-query
    (generify-dumped-term parsed-query/1))
  (define rules
    (let ((stack (stack-make)))
      (stack-push! stack `((%any X)))
      (dump-rules
       (lambda (thing)
         (stack-push! stack (inference->profun-rule thing))))
      (stack->list stack)))
  (define db0
    (profun-create-falsy-database
     profun-standard-handler
     rules))

  (define (profun-filter entry)
    (define translated (translate-entry-tags entry))
    (define db (profun-database-extend db0 (map list translated)))
    (profun-eval-query/boolean db parsed-query))

  (define (iter)
    (let loop ()
      (define entry (iter0))
      (and entry
           (if (profun-filter entry)
               entry
               (loop)))))

  iter)

(define (tegfs-query/noopen/notall iter0 <query...>)
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
