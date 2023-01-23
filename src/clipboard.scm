;;;; Copyright (C) 2022  Otto Jung
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

%var dump-clipboard-to-temporary
%var dump-clipboard-to-file
%var get-clipboard-data-types
%var get-clipboard-text-content
%var classify-clipboard-text-content
%var get-clipboard-type-extension
%var choose-clipboard-data-type

%use (define-pair) "./euphrates/define-pair.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (mimetype/extensions) "./euphrates/mimetype-extensions.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"

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

  (if (= status 0) text ""))

(define (string-data-type? s)
  (member s '("STRING" "UTF8_STRING" "TEXT" "COMPOUND_TEXT")))

(define (get-mime-extension mimetype)
  (let ((ext (assoc mimetype mimetype/extensions)))
    (and ext (string-append "." (car (cdr ext))))))

(define (get-clipboard-type-extension data-type)
  (cond
   ((string-data-type? data-type) ".txt")
   (else (get-mime-extension data-type))))
