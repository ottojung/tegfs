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

%var tegfs-query/noopen

%use (appcomp) "./euphrates/comp.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (hashset-ref make-hashset) "./euphrates/ihashset.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (printf) "./euphrates/printf.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (entries-for-each) "./entries-for-each.scm"
%use (keyword-id) "./keyword-id.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (make-prolog-var) "./prolog-var.scm"
%use (tegfs-dump-prolog) "./prolog.scm"
%use (query-parse) "./query-parse.scm"
%use (tag->prolog-term) "./tag-to-prolog-term.scm"

(define (tegfs-query/noopen <query...> for-each-fn)
  (define output-path (string-append (make-temporary-filename/local) ".pl"))
  (define output-port (open-file-port output-path "w"))

  (define _333
    (parameterize ((current-output-port output-port))
      (define-values (parsed-query variables) (query-parse <query...>))
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

  (entries-for-each
   (lambda (entry)
     (define id (cdr (assoc keyword-id entry)))
     (when (hashset-ref ids id)
       (for-each-fn entry)))))
