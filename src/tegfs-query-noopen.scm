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

%run guile

%var tegfs-query/noopen

%use (assq-or) "./euphrates/assq-or.scm"
%use (appcomp) "./euphrates/comp.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (hashset-has? make-hashset) "./euphrates/hashset.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (printf) "./euphrates/printf.scm"
%use (profun-standard-handler) "./euphrates/profun-standard-handler.scm"
%use (profun-create-database profun-eval-query) "./euphrates/profun.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (entries-iterate) "./entries-iterate.scm"
%use (keyword-id) "./keyword-id.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (prolog-query-parse) "./prolog-query-parse.scm"
%use (make-prolog-var prolog-var-name prolog-var?) "./prolog-var.scm"
%use (tegfs-dump-prolog translate-entry-tags) "./prolog.scm"
%use (tag->prolog-term) "./tag-to-prolog-term.scm"

(define (tegfs-query/noopen <query...>)
  (define iter0 (entries-iterate))
  (if <query...>
      ;; (tegfs-query/noopen/notall iter0 <query...>)
      (tegfs-query/noopen/notall/profun iter0 <query...>)
      iter0))

(define (devectorize-translated translated)
  (map
   (lambda (clause)
     (list-map/flatten
      (lambda (obj)
        (if (vector? obj)
            (vector->list obj)
            (list obj)))
      clause))
   translated))

(define (generify-query query)
  (define (place-variables clause)
    (map
     (lambda (obj)
       (if (prolog-var? obj)
           (let ((name (prolog-var-name obj)))
             (if (equal? name 'This)
                 0 name))
           obj))
     clause))
  (map place-variables (map cdr query)))

(define (tegfs-query/noopen/notall/profun iter0 <query...>)
  (define-values (parsed-query/0 variables) (prolog-query-parse <query...>))
  (define parsed-query/1
    (devectorize-translated parsed-query/0))
  (define parsed-query
    (generify-query parsed-query/1))
  (define (profun-filter entry)
    (define translated (translate-entry-tags entry))
    (define translated* (devectorize-translated translated))
    (define db-definitions (map list translated*))
    (define db
      (profun-create-database
       profun-standard-handler
       db-definitions))
    (define result (profun-eval-query db parsed-query))
    (not (null? result)))

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

  (define-pair (ids/string code)
    (system-re "prolog -s ~a" output-path))

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
