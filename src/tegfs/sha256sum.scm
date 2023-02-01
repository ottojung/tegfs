;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs sha256sum)
    :export (sha256sum)
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates comp) :select (appcomp))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates run-syncproc-re) :select (run-syncproc/re))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates string-to-lines) :select (string->lines))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    )))


(define (sha256sum text)
  (define path
    (make-temporary-filename/local))
  (define ret
    (catch-any
     (lambda _
       (write-string-file path text)
       (appcomp
        (run-syncproc/re "sha256sum" path)
        string-strip
        string->lines
        car
        string->words
        car))
     (lambda errs
       (file-delete path)
       (raisu 'could-not-get-a-hash errs))))

  (file-delete path)
  ret)
