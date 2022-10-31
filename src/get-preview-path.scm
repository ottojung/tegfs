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

%var get-preview-path

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (string-plus-encoding-make) "./euphrates/string-plus-encode.scm"
%use (uri-safe/alphabet uri-safe/alphabet/index) "./euphrates/uri-safe-alphabet.scm"
%use (get-file-type) "./get-file-type.scm"
%use (get-root) "./get-root.scm"

(define encoder
  (string-plus-encoding-make uri-safe/alphabet uri-safe/alphabet/index #\+))

(define (get-preview-path target-fullpath)
  (define preview-directory
    (append-posix-path (get-root) "cache" "preview"))
  (define file-type (get-file-type target-fullpath))
  (define preview-extension
    (case file-type
      ((image) ".jpeg")
      ((video) ".gif")
      ((weblink) ".jpeg")
      (else #f)))

  (and preview-extension
       (let* ((relative-path (remove-common-prefix target-fullpath (string-append (get-root) "/")))
              (encoded-path (encoder relative-path))
              (preview-name
               (string-append encoded-path preview-extension)))
         (append-posix-path preview-directory preview-name))))
