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

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
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

(define (print-tag-as-prolog-term thing)
  (define type (car thing))

  (define (print arg)
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
     ((pair? arg)
      (case (car arg)
        ((var) (display (cdr arg)))
        (else 'bad-pair arg)))
     (else (raisu 'uknown-type arg))))

  (define (comma-print lst)
    (let loop ((lst lst))
      (unless (null? lst)
        (print (car lst))
        (unless (null? (cdr lst))
          (display ", "))
        (loop (cdr lst)))))

  (case type
    ((t)
     (display "t(")
     (print (cadr thing))
     (display ", ")
     (unless (null? (cdr (cddr thing)))
       (display "["))
     (comma-print (cddr thing))
     (unless (null? (cdr (cddr thing)))
       (display "]"))
     (display ")"))
    ((i v)
     (display type)
     (display "(")
     (comma-print (cdr thing))
     (display ")"))
    (else
     (raisu 'bad-thing thing))))

(define (tag->prolog-term thing)
  (with-output-to-string
    (lambda _ (print-tag-as-prolog-term thing))))
