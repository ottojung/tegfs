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

%var tag->prolog-term
%var print-tag-as-prolog-term

%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
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

%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (prolog-var? prolog-var-name) "./prolog-var.scm"
%use (prolog-cut-symbol?) "./prolog-cut-symbol.scm"

(define (prolog-var-needs-quoting? var/chars)
  (define first (alphanum/alphabet/index (car var/chars)))
  (not
   (or (equal? '(#\~) var/chars)
       (and first
            (> first 9)
            (< first 36)
            (list-and-map
             (compose-under
              or
              (comp (equal? #\_))
              alphanum/alphabet/index)
             (cdr var/chars))))))

(define (print-prolog-symbol arg)
  (cond
   ((symbol? arg)
    (let* ((str (symbol->string arg))
           (chars (string->list str)))
      (if (prolog-var-needs-quoting? chars)
          (begin
            (display "'")
            (display str)
            (display "'"))
          (display str))))
   ((string? arg) (write arg))
   ((integer? arg) (write arg))
   ((prolog-var? arg) (display (prolog-var-name arg)))
   ((prolog-cut-symbol? arg) (display "!"))
   (else (raisu 'uknown-type arg))))

(define (comma-translate lst)
  (let loop ((lst lst))
    (unless (null? lst)
      (print-tag-as-prolog-term (car lst))
      (unless (null? (cdr lst))
        (display ", "))
      (loop (cdr lst)))))

(define (print-tag-as-prolog-term thing)
  (cond
   ((pair? thing)
    (print-prolog-symbol (car thing))
    (unless (null? (cdr thing))
      (display "(")
      (comma-translate (cdr thing))
      (display ")")))
   ((vector? thing)
    (display "[")
    (comma-translate (vector->list thing))
    (display "]"))
   (else
    (print-prolog-symbol thing))))

(define (tag->prolog-term thing)
  (with-output-to-string
    (lambda _ (print-tag-as-prolog-term thing))))
