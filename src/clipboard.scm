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

%var dump-clipboard-to-temporary
%var dump-clipboard-to-file
%var get-clipboard-data-types
%var get-clipboard-text-content
%var classify-clipboard-text-content
%var get-clipboard-type-extension
%var choose-clipboard-data-type

%use (system-fmt) "./euphrates/system-fmt.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (dprint) "./euphrates/dprint.scm"
%use (directory-files-rec/filter) "./euphrates/directory-files-rec-filter.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (mimetype/extensions) "./euphrates/mimetype-extensions.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (eval-in-current-namespace) "./euphrates/eval-in-current-namespace.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (range) "./euphrates/range.scm"
%use (assoc-set-default) "./euphrates/assoc-set-default.scm"
%use (comp) "./euphrates/comp.scm"
%use (path-without-extension) "./euphrates/path-without-extension.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (list-take-n) "./euphrates/list-take-n.scm"
%use (print-in-frame) "./euphrates/print-in-frame.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (define-pair) "./euphrates/define-pair.scm"

%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (fatal) "./fatal.scm"

(define (classify-clipboard-text-content text-content)
  (cond
   ((string-null? text-content) 'data)
   ((a-weblink? text-content) 'link)
   ((file-or-directory-exists? text-content) 'localfile)
   (else 'pasta)))

(define (dump-clipboard-to-file data-type target)
  (= 0 (system-fmt "xclip -selection clipboard -target ~a -out > ~a"
                   data-type target)))

(define (dump-clipboard-to-temporary data-type)
  (define target (make-temporary-filename/local))
  (and (dump-clipboard-to-file data-type target)
       target))

(define (get-clipboard-data-types)
  (define-pair (types-list/str status)
    (system-re "xclip -o -target TARGETS -selection clipboard"))

  (and (= 0 status)
       (string->lines types-list/str)))

(define (choose-clipboard-data-type)
  (define types-list (get-clipboard-data-types))
  (define types-list/str (lines->string types-list))
  (define-pair (chosen status)
    (system-re "echo ~a | fzf" types-list/str))
  (and (= 0 status)
       (string-strip chosen)))

(define (get-clipboard-text-content)
  (define-pair (text status)
    (system-re "xclip -selection clipboard -out"))

  (and (= 0 status) text))

(define (string-data-type? s)
  (or (member s '("STRING" "UTF8_STRING" "TEXT" "COMPOUND_TEXT"))
      (string-prefix? "text/" s)))

(define (get-mime-extension mimetype)
  (let ((ext (assoc mimetype mimetype/extensions)))
    (and ext (string-append "." (car (cdr ext))))))

(define (get-clipboard-type-extension data-type)
  (cond
   ((string-data-type? data-type) ".txt")
   (else (get-mime-extension data-type))))
