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
  (define-module (tegfs tegfs-query-noopen)
    :export (tegfs-query/noopen)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates comp) :select (appcomp))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates hashset) :select (hashset-has? make-hashset))
    :use-module ((euphrates list-intersperse) :select (list-intersperse))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((euphrates printf) :select (printf))
    :use-module ((euphrates profun-database) :select (profun-database-extend))
    :use-module ((euphrates profun-standard-handler) :select (profun-standard-handler))
    :use-module ((euphrates profun) :select (profun-create-falsy-database profun-eval-query/boolean))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates run-syncproc-re-star) :select (run-syncproc/re*))
    :use-module ((euphrates stack) :select (stack->list stack-make stack-push!))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates string-to-lines) :select (string->lines))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs dump-rules) :select (dump-rules))
    :use-module ((tegfs entries-iterate) :select (entries-iterate))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    :use-module ((tegfs prolog-query-parse) :select (prolog-query-parse))
    :use-module ((tegfs prolog-var) :select (make-prolog-var prolog-var-name prolog-var?))
    :use-module ((tegfs prolog) :select (tegfs-dump-prolog translate-entry-tags))
    :use-module ((tegfs tag-to-prolog-term) :select (tag->prolog-term))
    )))



(define (tegfs-query/noopen <query...>)
  (define iter0 (entries-iterate))
  (if <query...>
      ;; (tegfs-query/noopen/notall iter0 <query...>)
      (tegfs-query/noopen/notall/profun iter0 <query...>)
      iter0))

(define (generify-query query)
  (define (place-variables clause)
    (map
     (lambda (obj)
       (if (prolog-var? obj)
           (let ((name (prolog-var-name obj)))
             (cond
              ((symbol? name) name)
              (else (string->symbol (~a name)))))
           obj))
     clause))
  (map place-variables (map cdr query)))

(define (tegfs-query/noopen/notall/profun iter0 <query...>)
  (define-values (parsed-query/1 variables) (prolog-query-parse <query...>))
  (define parsed-query
    (generify-query parsed-query/1))
  (define rules
    (let ((stack (stack-make)))
      (dump-rules
       (lambda (thing)
         (define RHS (generify-query (cddr thing)))
         (define consequent (generify-query (list (cadr thing))))
         (stack-push! stack (append consequent RHS))))
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
