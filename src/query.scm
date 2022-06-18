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

%var tegfs-query
%var tegfs-query/parse

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (printf) "./euphrates/printf.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (appcomp comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (fn-cons) "./euphrates/fn-cons.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (list-ref-or) "./euphrates/list-ref-or.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (list-span-while) "./euphrates/list-span-while.scm"
%use (alphanum/alphabet/index) "./euphrates/alphanum-alphabet.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (make-hashset hashset-ref hashset-length) "./euphrates/ihashset.scm"
%use (list-deduplicate/reverse) "./euphrates/list-deduplicate.scm"
%use (cons!) "./euphrates/cons-bang.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"
%use (root/p) "./root-p.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (parse-tag) "./parse-tag.scm"
%use (tegfs-dump-prolog) "./prolog.scm"
%use (query-parse) "./query-parse.scm"
%use (tag->prolog-term) "./tag-to-prolog-term.scm"
%use (entries-for-each) "./entries-for-each.scm"
%use (entry-print) "./entry-print.scm"
%use (entry-print/formatted) "./entry-print-formatted.scm"
%use (id-name) "./id-name.scm"
%use (make-prolog-var) "./prolog-var.scm"
%use (fatal) "./fatal.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"

(define (tegfs-query/parse --entries <query-format> <query...>)
  (define entries
    (tegfs-query <query...>))

  (cond
   (--entries
    (for-each
     (lambda (entry)
       (entry-print entry)
       (display "\n\n"))
     entries))
   (<query-format>
    (for-each
     (lambda (entry)
       (entry-print/formatted <query-format> entry)
       (display "\n"))
     entries)))

  (parameterize ((current-output-port (current-error-port)))
    (let ((len (length entries)))
      (if (equal? 0 len)
          (display "No matches.")
          (printf "Total of ~a matches." len))
      (newline)))

  )

(define (tegfs-query <query...>)
  (define output-path (string-append (make-temporary-filename) ".pl"))
  (define output-port (open-file-port output-path "w"))

  (define _333
    (parameterize ((current-output-port output-port))
      (display ":- initialization(main, main).") (newline)
      (tegfs-dump-prolog)

      (define-values (parsed-query variables) (query-parse <query...>))
      (define initializations (map (lambda (v) `(vandthis ,(make-prolog-var 'This) ,(make-prolog-var v))) variables))
      (define prolog-query-0 (map tag->prolog-term (append initializations parsed-query)))
      (define prolog-query (apply string-append (list-intersperse ", " prolog-query-0)))

      (printf "single(This) :- ~a.\n" prolog-query)
      (printf "thises(This) :- i(This, Id), single(This), writeln(Id).\n")
      (printf "main(_Argv) :- findall(This, thises(This), _).")
      (newline) (newline)
      ))

  (define _222
    (close-port output-port))

  (define-pair (ids/string code)
    (system-re "prolog -s ~a" output-path))

  (define _111
    (unless (equal? 0 code)
      (fatal "Prolog execution failed with code ~s and contents:\n~a" code ids/string)))

  (define ids
    (appcomp ids/string
             string->lines
             (map string-strip)
             (filter (negate string-null?))
             make-hashset))

  (define ret '())

  (entries-for-each
   (lambda (entry)
     (define id (cdr (assoc id-name entry)))
     (when (hashset-ref ids id)
       (cons! entry ret))))

  ret)
