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

%var tegfs-dump-clipboard
%var tegfs-dump-clipboard/parse
%var tegfs-dump-clipboard/pasta

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
%use (raisu) "./euphrates/raisu.scm"
%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"

%use (fatal) "./fatal.scm"
%use (classify-clipboard-text-content dump-clipboard-to-temporary dump-clipboard-to-file get-clipboard-data-types get-clipboard-text-content get-clipboard-type-extension choose-clipboard-data-type) "./clipboard.scm"

(define (tegfs-dump-clipboard/pasta text)
  (let* ((pref (make-temporary-filename))
         (extension ".txt")
         (target (string-append pref extension)))
    (write-string-file target text)
    target))

(define (tegfs-dump-clipboard)
  (define text
    (or (get-clipboard-text-content)
        (fatal "Could not get clipboard text")))
  (define real-type
    (classify-clipboard-text-content text))

  (case real-type
    ((data)
     (let* ((data-type
             (or (choose-clipboard-data-type)
                 (fatal "Could not get clipboard data type")))
            (extension (get-clipboard-type-extension data-type))
            (pref (make-temporary-filename))
            (target (string-append pref extension)))
       (unless (dump-clipboard-to-file data-type target)
         (fatal "Could not dump clipboard content"))
       target))
    ((pasta)
     (tegfs-dump-clipboard/pasta text))
    ((link localfile)
     text)
    (else
     (raisu 'unexpected-real-type real-type))))

(define (tegfs-dump-clipboard/parse)
  (display (tegfs-dump-clipboard)))
