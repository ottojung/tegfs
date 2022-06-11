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

%var query-parse

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (printf) "./euphrates/printf.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (list-ref-or) "./euphrates/list-ref-or.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
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
%use (fn-cons) "./euphrates/fn-cons.scm"
%use (list-deduplicate/reverse) "./euphrates/list-deduplicate.scm"
%use (curry-if) "./euphrates/curry-if.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"
%use (root/p) "./root-p.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (parse-tag) "./parse-tag.scm"
%use (make-prolog-var) "./prolog-var.scm"

(define (query-parse <query...>)
  (define (tovar x)
    (make-prolog-var
     ((curry-if (comp (equal? tags-this-variable/string)) (const 'This)) x)))

  (define parser (parse-tag tags-this-variable/string))
  (define (maybe-parse tag)
    (if (or (string? tag) (symbol? tag) (number? tag))
        (parser tag)
        (list tag)))

  (define parsed-query-0
    (apply
     append
     (map maybe-parse <query...>)))
  (define (convert-arguments args)
    (define mapped (map tovar args))
    (list
     (if (null? (cdr mapped))
         (car mapped)
         (list->vector mapped))))

  (define parsed-query-1 (map (curry-if pair? (fn-cons identity convert-arguments)) parsed-query-0))
  (define parsed-query (map (curry-if pair? (comp (cons 't))) parsed-query-1))
  (define this-strings `(,tags-this-variable/string "This"))
  (define (get-args term)
    (if (pair? term) (cdr term) '()))

  (define variables
    (filter (lambda (v) (not (member (~a v) this-strings)))
            (list-deduplicate/reverse
             (apply append (map get-args parsed-query-0)))))
  (values parsed-query variables))
