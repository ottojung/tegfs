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
  (define-module (tegfs dump-clipboard)
    :export (tegfs-dump-clipboard tegfs-dump-clipboard/parse tegfs-dump-clipboard/pasta)
    :use-module ((euphrates make-temporary-filename) :select (make-temporary-filename))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs clipboard) :select (choose-clipboard-data-type classify-clipboard-text-content dump-clipboard-to-file get-clipboard-text-content get-clipboard-type-extension))
    :use-module ((tegfs fatal) :select (fatal))
    )))



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
