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

(cond-expand
 (guile
  (define-module (tegfs clipboard)
    :export (dump-clipboard-to-temporary dump-clipboard-to-file get-clipboard-data-types get-clipboard-text-content classify-clipboard-text-content get-clipboard-type-extension choose-clipboard-data-type)
    :use-module ((euphrates asyncproc-input-text-p) :select (asyncproc-input-text/p))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates lines-to-string) :select (lines->string))
    :use-module ((euphrates mimetype-extensions) :select (mimetype/extensions))
    :use-module ((euphrates run-syncproc-re-star) :select (run-syncproc/re*))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates string-to-lines) :select (string->lines))
    :use-module ((euphrates system-fmt) :select (system-fmt))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    )))



(define (classify-clipboard-text-content text-content)
  (cond
   ((string-null? text-content) 'data)
   ((a-weblink? text-content) 'link)
   ((file-or-directory-exists? text-content) 'localfile)
   (else 'pasta)))

(define (dump-clipboard-to-file data-type/0 target)
  (define data-type (if (equal? (~a data-type/0) "text/plain") 'TEXT data-type/0))
  (= 0 (system-fmt "xclip -selection clipboard -target ~a -out > ~a"
                   data-type target)))

(define (dump-clipboard-to-temporary data-type)
  (define target (make-temporary-filename/local))
  (and (dump-clipboard-to-file data-type target)
       target))

(define (get-clipboard-data-types)
  (define-values (types-list/str status)
    (run-syncproc/re* "xclip" "-o" "-target" "TARGETS" "-selection" "clipboard"))

  (and (= 0 status)
       (string->lines types-list/str)))

(define (choose-clipboard-data-type)
  (define types-list (get-clipboard-data-types))
  (and types-list
       (let ()
         (define types-list/str (lines->string types-list))
         (define-values (chosen status)
           (parameterize ((asyncproc-input-text/p types-list/str))
             (run-syncproc/re* "fzf")))
         (and (= 0 status)
              (string-strip chosen)))))

(define (get-clipboard-text-content)
  (define-values (text status)
    (run-syncproc/re* "xclip" "-selection" "clipboard" "-out"))

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
