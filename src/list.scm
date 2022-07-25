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

%var tegfs-list
%var tegfs-list/parse

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
%use (file-is-directory?/no-readlink) "./euphrates/file-is-directory-q-no-readlink.scm"
%use (directory-files-depth-foreach) "./euphrates/directory-files-depth-foreach.scm"

%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"
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
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (standalone-file->entry) "./standalone-file-to-entry.scm"

%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-list/parse <listdepth> --entries <list-format>)
  (tegfs-list
   <listdepth>
   (if --entries
       (lambda (e) (entry-print e) (display "\n\n"))
       (lambda (e) (entry-print/formatted <list-format> e) (display "\n")))))

(define (tegfs-list <listdepth> fn)
  (if <listdepth>
      (entries-for-each
       (lambda (entry)
         (define target-fullpath (entry-target-fullpath entry))
         (fn entry)
         (when (file-is-directory?/no-readlink target-fullpath)
           (directory-files-depth-foreach
            <listdepth>
            (lambda (p)
              (define path (car p))
              (fn (standalone-file->entry path)))
            target-fullpath))))
      (entries-for-each fn)))
